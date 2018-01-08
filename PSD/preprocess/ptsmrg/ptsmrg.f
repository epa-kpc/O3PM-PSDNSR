      PROGRAM PTSMRG

      CHARACTER*4 IFILE(10), NOTE(60), MNAME(10)
      CHARACTER*4  SPNAME(10,300), NEWSPEC(10,300), MSPEC(10,300)
      REAL       EMISS(2000000,200)

      character*300 INFILE,mfile(300)
      character*300 fname
      character*10 cname

      REAL*4     DUMX(3000000),DUMY(3000000),IDUMX(3000000),IDUMY(3000000)
      REAL       DUMXX(3000000),DUMYY(3000000),FLOW(3000000),PLUMHT(3000000)                 
      INTEGER    ICELL(3000000),JCELL(3000000)     
      INTEGER    KCELL(3000000),mask(3000000) 

      character*10 cspec,intspec(300)
      integer zz,ii,lastpt,firsttime,intspecs
      integer uamid,nxc,nyc,nfiles,mrgpts,iv(250,250)
      integer iunit,infilespec(300),newjdate,nstack(300)
      real xorg,yorg,lx,begtim,endtim
      integer nseg,nspecs,jdate,idate


      read(*,'(10x,a)') fname
      read(fname,*) newjdate 
      write(*,*)'Julian date for emissions file (YYDDD):',newjdate

      read(*,'(10x,a)') infile
      open(9,file=infile,form='unformatted')
      write(*,*) 'Opened new emissions file: ',infile

      read(*,'(10x,a)') fname
      read(fname,*) nfiles
      write(*,*)'Number of point files to merge:',nfiles

      do numfiles = 1,nfiles

      iunit = numfiles + 20
      read(*,'(10x,a)') mfile(numfiles) 
c      open(iunit,file=mfile(numfiles),form='unformatted',status='old',err=999)
      write(*,*) 'Opened emissions file: ',mfile(numfiles)

      read(*,'(10x,a)') fname
      read(fname,*) nstack(numfiles)
      write(*,*)'Number for this source group:',nstack(numfiles)

      enddo



      mrgpts = 1

      do numfiles = 1,nfiles

      iunit = numfiles + 20
      open(iunit,file=mfile(numfiles),form='unformatted',status='old',err=999)
      write(*,*) 'Opened emissions file: ',mfile(numfiles)
     
      READ (iunit) IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM

      write(*,*) IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM

      READ (iunit) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $ NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP

      write(*,*) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $ NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP

      WRITE(*,*)'Number of species on file:',NSPECS
      WRITE (*,1007) IDATE, BEGTIM, JDATE, ENDTIM 

      READ  (iunit) IX,IY,NXCLL,NYCLL
      READ  (iunit) ((MSPEC(I,J),I=1,10),J=1,NSPECS)

       infilespec(numfiles) = NSPECS

       if(numfiles.eq.1) then !make species index

       do J = 1,NSPECS
       write(cspec,'(10a1)') (MSPEC(i,J),i=1,10)
       intspec(J) = cspec
       iv(J,numfiles) = J

        do i=1,10
        write(NEWSPEC(i,J),'(a)') cspec(i:i)
        enddo

       enddo
       intspecs = NSPECS

       else !check on list or add to list

         do J = 1, NSPECS
       write(cspec,'(10a1)') (MSPEC(i,J),i=1,10)
       flag = 0
        do K = 1,intspecs
       if (cspec .eq. intspec(K)) then
       flag = 1
       iv(J,numfiles) = K 
       endif
        enddo
       if(flag.eq.0) then !new specie
       intspec(intspecs+1) = cspec
       intspecs = intspecs + 1
       iv(J,numfiles) = intspecs

        do i=1,10
        write(NEWSPEC(i,intspecs),'(a)') cspec(i:i)
        enddo

       endif
         enddo

       endif

       do J = 1, NSPECS
      WRITE (*,1022) (MSPEC(I,J),I=1,10),iv(J,numfiles),J
       enddo
 
       write(*,*) 'Species:',NSPECS,intspecs

C  TIME INVARIANT DATA
 
      READ (iunit) ISEGM,NPMAX

      lastpt = NPMAX + mrgpts - 1
      write(*,*)'Total # of stacks',NPMAX,mrgpts,lastpt

      READ (iunit) (DUMX(II),DUMY(II),IDUMX(II),IDUMY(II),DUMXX(II),
     &   DUMYY(II),II=mrgpts,lastpt)

             do ii=mrgpts,lastpt
                if(nstack(numfiles).gt.1) then
                 IDUMY(ii)=-(IDUMY(ii))
                else
                 !do not set for PIG
                endif
             enddo


          mrgpts = mrgpts + NPMAX
          firsttime = 0

       close(iunit)

       enddo !end loop over input files

c-----------write camx final merged file header

      do J = 1, intspecs
       write(*,*) J,intspec(J)
      enddo

      WRITE(9)IFILE,NOTE,NSEG,intspecs,newjdate,BEGTIM,newjdate,ENDTIM
      WRITE(*,*)IFILE,NOTE,NSEG,intspecs,newjdate,BEGTIM,newjdate,ENDTIM
      WRITE(9) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $ NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP
      WRITE (9) IX,IY,NXCLL,NYCLL
      write(*,*) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY

      WRITE (9) ((NEWSPEC(I,J),I=1,10),J=1,intspecs)
      WRITE (*,1013) ((NEWSPEC(I,J),I=1,10),J=1,intspecs)

      NPMAX = lastpt

      WRITE(9) ISEGM,NPMAX
      write(*,*)'Total # of stacks',NPMAX,lastpt
      WRITE(9) (DUMX(II),DUMY(II),IDUMX(II),IDUMY(II),DUMXX(II),
     &   DUMYY(II),II=1,NPMAX)

c      do ii = 1, NPMAX
c       write(*,*) ii,IDUMY(ii)
c      enddo

c------- reopen and reread header info of input files

      do numfiles = 1, nfiles

      iunit = numfiles + 20
      open(iunit,file=mfile(numfiles),form='unformatted',status='old',err=999)
      write(*,*) 'Opened emissions file: ',mfile(numfiles)

      READ (iunit) IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM
      READ (iunit) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $ NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP

      WRITE (*,1007) IDATE, BEGTIM, JDATE, ENDTIM

      READ  (iunit) IX,IY,NXCLL,NYCLL
      READ  (iunit) ((MSPEC(I,J),I=1,10),J=1,NSPECS)
      READ (iunit) ISEGM,NPMAX
      READ (iunit) (DUMX(II),DUMY(II),IDUMX(II),IDUMY(II),DUMXX(II),
     &   DUMYY(II),II=1,NPMAX)

      enddo

c-----------Time variant section

      DO IH = 1,24

      mrgpts = 1

      do numfiles = 1, nfiles      
       iunit = numfiles + 20
          READ(iunit) IBGDAT,BEGTIM,IENDAT,ENDTIM
          READ(iunit) ISEGNM,NUMPTS

             lastpt = NUMPTS + mrgpts - 1

             READ(iunit) (ICELL(II),JCELL(II),KCELL(II),FLOW(II), 
     &          PLUMHT(II),II=mrgpts,lastpt) 

             do ii=mrgpts,lastpt
                if(nstack(numfiles).gt.0) then
                 mask(ii)=-(nstack(numfiles))
                else
                 mask(ii)=KCELL(II)
                endif
             enddo
                         
              DO K=1,infilespec(numfiles)
          READ (iunit) ISEGNM,(SPNAME(II,iv(K,numfiles)),II=1,10),
     &      (EMISS(II,iv(K,numfiles)),II=mrgpts,lastpt)   
              ENDDO !end nspecs loop

          mrgpts = mrgpts + NUMPTS

       enddo !end loop over number of files

c---- write to output file

          write(9) newjdate,real(IH-1),newjdate,real(IH)
          
          NPMAX = lastpt
   
             WRITE(9) ISEGNM,NPMAX
             WRITE(9)(ICELL(II),JCELL(II),mask(II),FLOW(II),
     &          PLUMHT(II),II=1,NPMAX)

              DO K=1,intspecs  
            WRITE(9) ISEGNM,(NEWSPEC(II,K),II=1,10),(EMISS(II,K),
     &             II=1,NPMAX) 
              ENDDO !end nspecs loop 

           write(*,*) NPMAX,IBGDAT,BEGTIM,IENDAT,ENDTIM
           write(*,*) NPMAX, newjdate, real(IH-1), newjdate,real(IH)
          ENDDO !end loop over 24 hours

c-----------End write out final file

 1007 FORMAT(2(I10,F10.2),I10)
 1013 FORMAT(1X,10A1)
 1022 FORMAT(1X,10A1,1X,i3,1x,i3)

  999 STOP
      END 

