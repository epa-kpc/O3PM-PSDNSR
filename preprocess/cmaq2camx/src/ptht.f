      program ptht
      implicit none
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Copyright (C) 2003-2011  ENVIRON
c
c
c   This program is free software; you can redistribute it and/or
c   modify it under the terms of the GNU General Public License
c   as published by the Free Software Foundation; either version 2
c   of the License, or (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   To obtain a copy of the GNU General Public License
c   write to the Free Software Foundation, Inc.,
c   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
c
c
c   For comments and questions, send to bkoo@environcorp.com
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     PTHT adds "effective plume height" to a CAMx elevated point source
c     emissions file (CAMx PT) that was created by CMAQ2UAM processor.
c     The effective plume height is assigned to the center heihgt of the
c     cell where the virtual stack is located. The cell center height is
c     calculated using layer interface heights from CAMx Height/Pressure
c     files (CAMx ZP). The grid definition and time interval of the CAMx
c     ZP file is assumed to be consistent with the CAMx PT file.
c
c     This code is a sub-program of CMAQ2CAMx v2.
c
c     INPUT: via standard input
c       Input CAMx PT   - Input CAMx PT file name
c       Input CAMx ZP   - Input CAMx ZP file name
c       Output CAMx PT  - Output CAMx PT file name
c       No. of MET layers - Number of layers in the CAMx height file
c                           If blank, uses the number of layers
c                           specified in the input emissions file.
c
c     OUTPUT:
c       CAMx PT file with "effective plume height"
c
c     HISTORY:
c       created by bkoo (09/04/2003)
c       revised by bkoo (04/14/2006)      - revised for Fortran90
c       modified by bkoo (05/27/2006)     - modified to take user input
c                                           for the number of MET layers
c
      integer, parameter :: jin1 = 99, jin2 = 98, jout = 97

      character(256) :: line
      character(4), dimension(10) :: name,lspec
      character(4), dimension(60) :: note
      integer :: nseg,nstk,nspec,iutm,nx,ny,nz,ibdate,iedate
      integer :: iseg,ixseg,iyseg,nxseg,nyseg,nzlowr,nzuppr
      real :: xorg,yorg,delx,dely,btime,etime
      real :: refx,refy,htsur,htlow,htupp

      character(4), allocatable :: mspec(:,:)
      integer, allocatable :: ijkstk(:,:)
      real, allocatable :: parstk(:,:)
      real, allocatable :: ptems(:)
      real, allocatable :: height(:,:,:)
      real, allocatable :: plmht(:)
      integer :: istat

      integer :: lmet
      integer :: i,j,k,l,n
      logical :: lfirst


11    format(20x,a)
c
c     Read input CAMx PT file name and open it
c
      write(*,*) 'Enter name of input CAMx PT file:'
      read(*,11) line
      write(*,*) TRIM(line)
      open(jin1,file=line,status='OLD',form='UNFORMATTED')
c
c     Read input CAMx ZP file name and open it
c
      write(*,*) 'Enter name of input CAMx ZP file:'
      read(*,11) line
      write(*,*) TRIM(line)
      open(jin2,file=line,status='OLD',form='UNFORMATTED')
c
c     Read output CAMx PT file name and open it
c
      write(*,*) 'Enter name of output CAMx PT file:'
      read(*,11) line
      write(*,*) TRIM(line)
      open(jout,file=line,status='NEW',form='UNFORMATTED')
c
c     Read CAMx PT header portion
c
      read(jin1) name,note,nseg,nspec,ibdate,btime,iedate,etime
      read(jin1) refx,refy,iutm,xorg,yorg,delx,dely,nx,ny,nz,
     &           nzlowr,nzuppr,htsur,htlow,htupp
      read(jin1) ixseg,iyseg,nxseg,nyseg
c
c     Read user input for the number of MET layers
c
      write(*,*) 'Enter the number of MET layers:'
      read(*,11) line
      if (line.eq.'') then
        lmet = nz
        write(*,*) 'Uses # of layers in the input emiss file - ',lmet
        if (nz.le.1) then
          write(*,*) 'Invalid # of input emiss layers - ',nz
          stop
        endif
      else
        read(line,'(i)') lmet
        if (nz.gt.lmet) then
          write(*,*) 'ERROR: # of MET layers is less than NZ - ',nz
          stop
        endif
        write(*,*) lmet
      endif

      allocate (mspec(10,nspec), stat = istat)
      if (istat.ne.0) stop'ERROR: memory allocation failed - MSPEC'

      read(jin1) ((mspec(n,l),n=1,10),l=1,nspec)
      read(jin1) iseg,nstk

      allocate (parstk(6,nstk), stat = istat)
      if (istat.ne.0) stop'ERROR: memory allocation failed - PARSTK'
      allocate (ijkstk(3,nstk), stat = istat)
      if (istat.ne.0) stop'ERROR: memory allocation failed - IJKSTK'
      allocate (ptems(nstk), stat = istat)
      if (istat.ne.0) stop'ERROR: memory allocation failed - PTEMS'

      read(jin1) ((parstk(k,n),k=1,6),n=1,nstk)
c
c     Write CAMx PT header portion
c
      write(jout)name,note,nseg,nspec,ibdate,btime,iedate,etime
      write(jout)refx,refy,iutm,xorg,yorg,delx,dely,nx,ny,nz,
     &           nzlowr,nzuppr,htsur,htlow,htupp
      write(jout)ixseg,iyseg,nxseg,nyseg
      write(jout)((mspec(n,l),n=1,10),l=1,nspec)
      write(jout)iseg,nstk
      write(jout)((parstk(k,n),k=1,6),n=1,nstk)
c
c     Allocate memory for height array
c
      allocate (height(nx,ny,lmet), stat = istat)
      if (istat.ne.0) stop'ERROR: memory allocation failed - HEIGHT'
      allocate (plmht(nstk), stat = istat)
      if (istat.ne.0) stop'ERROR: memory allocation failed - PLMHT'
c
c     Time-variant portion
c
      lfirst = .true.
100   read(jin1,end=900) ibdate,btime,iedate,etime
      write(jout)        ibdate,btime,iedate,etime
      write(*,'(2(i,f))')ibdate,btime,iedate,etime

      read(jin1) iseg,nstk
      write(jout)iseg,nstk

      read(jin1) ((ijkstk(j,n),j=1,3),(parstk(k,n),k=1,2),n=1,nstk)
      if (lfirst) then
        do k = 1, lmet
          read(jin2,end=800)btime,ibdate,((height(i,j,k),i=1,nx),j=1,ny)
          read(jin2) ! Ignore the pressure part
        enddo
        do n = 1, nstk
          if (ijkstk(3,n).eq.1) then
c           write(*,*) 'ERROR: elevated source (#',n,')'
c           write(*,*) '       is put in the ground level'
c           stop
          plmht(n) = 1.0 !KB
          else !KB
c         endif
          plmht(n) = 0.5 * ! cell center height
     &             ( height(ijkstk(1,n),ijkstk(2,n),ijkstk(3,n)-1)
     &             + height(ijkstk(1,n),ijkstk(2,n),ijkstk(3,n)) )
          endif !KB
        enddo
        lfirst = .false.
      endif ! lfirst
      do k = 1, lmet
        read(jin2,end=800)btime,ibdate,((height(i,j,k),i=1,nx),j=1,ny)
        read(jin2) ! Ignore the pressure part
      enddo
      do n = 1, nstk
        if(ijkstk(3,n).eq.1) then !KB
        parstk(2,n) = -1.0 !KB
        else !KB
        parstk(2,n) = plmht(n)
        plmht(n) = 0.5 * ! cell center height
     &           ( height(ijkstk(1,n),ijkstk(2,n),ijkstk(3,n)-1)
     &           + height(ijkstk(1,n),ijkstk(2,n),ijkstk(3,n)) )
        parstk(2,n) = -0.5 * ( parstk(2,n) + plmht(n) ) ! "Negative" time avg
        endif !KB
      enddo
      write(jout)((ijkstk(j,n),j=1,3),(parstk(k,n),k=1,2),n=1,nstk)

      do l = 1, nspec
        read(jin1) iseg,(lspec(j),j=1,10),(ptems(n),n=1,nstk)
        write(jout)iseg,(lspec(j),j=1,10),(ptems(n),n=1,nstk)
      enddo

      goto 100
800   write(*,*) 'ERROR: Premature End of CAMx PZ file'
      stop
900   write(*,*) 'End of CAMx PT file'
c
c     Close files
c
      close(jin1)
      close(jin2)
      close(jout)

      end

