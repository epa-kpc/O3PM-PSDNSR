

      IMPLICIT NONE
      INCLUDE SUBST_PARMS_ID      ! I/O parameters definitions
      INCLUDE SUBST_FDESC_ID      ! file header data structuer
      INCLUDE SUBST_IODECL_ID     ! I/O definitions and declarations

      INTEGER :: RUNLEN,TRIMLEN
      INTEGER :: TSTEP, NSTEPS, V, VMAX, UMAX, DMAX
      CHARACTER*16    VNAME( MXVARS3 ) !  list of vble names, from user
      CHARACTER*16    UNITS( MXVARS3 ) !  list of vble units
      CHARACTER*80    VDESC( MXVARS3 ) !  list of vble descs
      CHARACTER*200   ifile
      CHARACTER*16    targetspec, emisspec
      CHARACTER*1     nh3flag
      CHARACTER*3     NH3
      DATA            NH3 / 'NH3' /
      CHARACTER*256   MESG    !  buffer for m3exit(), etc
      LOGICAL         EFLAG !flag: error has happened

      CHARACTER*16    PROGNAME
      DATA            PROGNAME / 'IOMATH' /
      CHARACTER*16    METCRO3D
      DATA            METCRO3D / 'G3D ' /
      CHARACTER*16    OUTFILE
      DATA            OUTFILE / 'OUTF ' /

      INTEGER I, N, C, R, L, S, JDATE, JTIME, SDATE, STIME,J
      INTEGER MONTH, DAY, YEAR, MDATE, MTIME, NHT,DAY1,NVARS
      INTEGER NCOLS, NROWS, NLAYS, ALTFLAG,FSTIME,FSDATE
      REAL X, Y, DX, DY, XORIG, YORIG, sum

      CHARACTER( 80 ) :: VARDESC
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 96 ) :: MSG1 = ' '

      CHARACTER (16), ALLOCATABLE:: OUTVAR ( : )
      REAL, ALLOCATABLE:: ZF( : , : , : ),ZZ(:,:,:)
      REAL, ALLOCATABLE:: COUT( : , : , : ) , NH3OUT(:,:,:)
      REAL emission, xlong, xlat,xloc,yloc,rlat0,rlon0,tlat1,tlat2
      REAL tempx,tempy,distance,maxdist,stkht
      INTEGER icell, jcell

C..........External Functions :
      INTEGER, EXTERNAL :: ENVINT    !  get environment variable as integer
      INTEGER STATUS

c..read in information

      read(*,'(10x,a)') ifile
      read(ifile,*) nh3flag
      write(*,*) 'Include NH3 from file Y/N:',nh3flag

      read(*,'(10x,a)') ifile
      read(ifile,*) maxdist
      write(*,*) 'Maximum distance written to text file:',maxdist

      read(*,'(10x,a)') ifile
      read(ifile,*) targetspec
      write(*,*) 'Target specie for text file:',targetspec

      read(*,'(10x,a)') ifile
      read(ifile,*) emisspec
      write(*,*)'Source label:',emisspec

      read(*,'(10x,a)') ifile
      read(ifile,*) stkht
      write(*,*)'Source stack height (m):',stkht

      read(*,'(10x,a)') ifile
      read(ifile,*) emission
      write(*,*) 'Emissions TPY:',emission

      read(*,'(10x,2f15.7)') xlat,xlong
      write(*,*)'Stack location:',xlat,xlong

      read(*,'(10x,4f15.7)') rlat0,rlon0,tlat1,tlat2
      write(*,*)'Grid projection information:',rlat0,rlon0,tlat1,tlat2

      read(*,'(10x,a)') ifile
      open(9,file=ifile)
      write(*,*) 'Opened extraction file: ',ifile


C Open METCRO3D file and get domain specs.
      IF ( .NOT. OPEN3( METCRO3D, FSREAD3, PROGNAME ) ) THEN
                CALL M3ERR( PROGNAME, JDATE, JTIME,
     &                      'Could not open ' // METCRO3D // ' file',
     &                      .TRUE. )
      END IF
      IF ( .NOT. DESC3( METCRO3D ) ) THEN
            CALL M3ERR( PROGNAME, JDATE, JTIME,
     &           'Could not get '
     &           // METCRO3D // ' file description', .TRUE. )
      END IF

      DX = XCELL3D
      DY = YCELL3D
      XORIG = XORIG3D
      YORIG = YORIG3D
      JDATE = SDATE3D
      JTIME = STIME3D
      FSDATE = SDATE3D
      FSTIME = STIME3D
      NCOLS = NCOLS3D
      NROWS = NROWS3D
      NLAYS = NLAYS3D
      NSTEPS = MXREC3D
      TSTEP = TSTEP3D

      ALLOCATE  ( ZF(NCOLS, NROWS, NVARS3D+2),
     &            ZZ(NCOLS, NROWS, NVARS3D+2),
     &            COUT(NCOLS, NROWS,NLAYS),
     &            NH3OUT(NCOLS, NROWS,NLAYS))


C.......   Get max string-lengths for use in variables-listing:

        VMAX = TRIMLEN( VNAME3D( 1 ) )
        UMAX = TRIMLEN( UNITS3D( 1 ) )
        DMAX = TRIMLEN( VDESC3D( 1 ) )
        DO  I = 1, NVARS3D
            VMAX = MAX( VMAX , TRIMLEN( VNAME3D( I ) ) )
            UMAX = MAX( UMAX , TRIMLEN( UNITS3D( I ) ) )
            DMAX = MAX( DMAX , TRIMLEN( VDESC3D( I ) ) )
        END DO

92000   FORMAT ( 5X , A )
        WRITE( *,92000 )
     &  ' ', 'The list of variables in this file is:', ' ',
     &  ( VNAME3D( I )( 1:VMAX ) // ' (' //
     &    UNITS3D( I )( 1:UMAX ) // '): ' //
     &    VDESC3D( I )( 1:DMAX ), I = 1, NVARS3D )

        write(*,*) NVARS3D

c    add new variables

      VNAME3D(NVARS3D+1) = 'DISTANCE'
      UNITS3D(NVARS3D+1) = 'KM'
      VTYPE3D(NVARS3D+1) = M3REAL
      VDESC3D(NVARS3D+1) = 'Distance from source'

      VNAME3D(NVARS3D+2) = 'QD'
      UNITS3D(NVARS3D+2) = 'TPY/KM'
      VTYPE3D(NVARS3D+2) = M3REAL
      VDESC3D(NVARS3D+2) = 'Q OVER D'

                NVARS = NVARS3D
                DO  V = 1, NVARS3D+2
                 VNAME( V ) = VNAME3D( V )
                 UNITS( V ) = UNITS3D( V )
                 VDESC( V ) = VDESC3D( V )
                 EFLAG = EFLAG .OR. ( VTYPE3D( V ) .NE. M3REAL )
                 write(*,*) V,VNAME3D(V),UNITS3D(V)
                END DO

c    zero out array

        do c = 1, NCOLS
        do r = 1, NROWS
         do n = 1, NLAYS
         NH3OUT(c,r,l) = -9.
         enddo
         do  V = 1, NVARS+2
         zf(c,r,V) = 0.
         zz(c,r,V) = 0.
         enddo
        enddo
        enddo


c     get stack coordinates on this grid

      call lcpgeo(0,rlat0,rlon0,tlat1,tlat2,xloc,yloc,xlong,xlat)

        icell = int(( (xloc*1000) - XORIG)/DX) + 1
        jcell = int(( (yloc*1000) - YORIG)/DY) + 1

      write(*,*)'Facilitiy grid location:',xloc,yloc,icell,jcell


c     read input files

      write(9,1002)

        DO  V = 1, NVARS

      JTIME = STIME3D
      JDATE = SDATE3D

        DO N = 1, NSTEPS

        write(*,*) 'Processing:',VNAME(V),JDATE,JTIME         

        IF ( .NOT. READ3( METCRO3D, VNAME( V ), ALLAYS3,JDATE, JTIME, COUT )) THEN
          MESG = 'Read failure:  file ' // METCRO3D //
     &                 ' variable ' // VNAME( V )
          CALL M3EXIT( 'VERTOT:VERSTEP', JDATE, JTIME,MESG, 2 )
         ENDIF      !  if read3() failed

       if(nh3flag.eq.'T'.or.nh3flag.eq.'Y') then
        IF ( .NOT. READ3( METCRO3D, NH3, ALLAYS3,JDATE, JTIME,NH3OUT )) THEN
          MESG = 'Read failure:  file ' // METCRO3D //
     &                 ' variable ' // NH3
          CALL M3EXIT( 'VERTOT:VERSTEP', JDATE, JTIME,MESG, 2 )
         ENDIF      !  if read3() failed
       endif

c        write(*,*) 'Processing:',VNAME(V),JDATE,JTIME

        do c = 1, NCOLS
        do r = 1, NROWS
         
         tempx = XORIG + c*DX
         tempy = YORIG + r*DY
         distance = sqrt( ((tempx-(xloc*1000))*(tempx-(xloc*1000))) +
     &       ((tempy-(yloc*1000))*(tempy-(yloc*1000))) ) / 1000.

         zz(c,r,V) =  COUT(c,r,1)
         zf(c,r,V) =  COUT(c,r,1) / distance
         zf(c,r,NVARS+1) = distance
         zf(c,r,NVARS+2) = emission / distance

        if(zf(c,r,NVARS+1).le.maxdist.and.VNAME(V).eq.targetspec) then
        write(9,1001) JDATE,JTIME,VNAME(V),emisspec,c,r,emission,stkht,xlat,xlong,NH3OUT(c,r,1),zf(c,r,NVARS+1),zz(c,r,V)
        endif

        enddo
        enddo


        JTIME = JTIME + TSTEP

        if(JTIME.eq.240000) then ! next day
        JTIME = 000000
        JDATE = JDATE + 1
        elseif(JTIME.gt.240000) then !next day
        JTIME = JTIME - TSTEP
        JDATE = JDATE + 1
        endif

        if(JDATE.eq.1992367) then 
        JDATE = 1993001
        endif

        ENDDO !end loop over time steps
        enddo !end loop over variables       

 1002 format('DATE,TIME,SPECIE,SOURCE,I,J,EMISSIONS,STACKHEIGHT,LATITUDE,LONGITUDE,NH3,DISTANCE,CONC')
 1001 format(i7,',',i6,',',a15,',',a15,',',i3,',',i3,',',f8.1,',',f6.1,',',f15.7,',',f15.7,',',f10.1,',',f10.1,',',f20.10)

c make domain total sums 

        do V = 1, NVARS+2
        sum = 0.
        do c = 1, NCOLS
        do r = 1, NROWS
         sum =  sum + zf(c,r,V) 
        enddo
        enddo
        write(*,*)'DOMAINSUM:',V,VNAME(V),sum
!        write(15,1010) SDATE3D,V,VNAME(V),sum
        enddo

 1010  format(i7,1x,i5,1x,a15,1x,f20.5)

c open new output file

      NLAYS3D = 1
      TSTEP3D = 240000
      NVARS3D = NVARS+2

      IF ( .NOT. OPEN3( OUTFILE, FSCREA3, PROGNAME ) ) THEN
            CALL M3ERR( PROGNAME, JDATE, JTIME,
     &           'Could not create ' // OUTFILE // ' file', .TRUE. )
      END IF                 !  if OPEN3() failed

        DO  V = 1, NVARS+2

       IF ( .NOT. WRITE3( OUTFILE, VNAME(V), FSDATE,FSTIME,zf(1,1,V) ) ) THEN
          CALL M3ERR( PROGNAME, JDATE, JTIME,
     &        'Could not write ' // OUTFILE // ' file', .TRUE. )
       END IF

        ENDDO



      


      CALL M3EXIT(PROGNAME, 0, 0, 'Program completed successfully', 0 )

1000  FORMAT(I4,1x,I2,1X,I2)
1100  format(a60)
1200  format(a893)

      STOP
      END




      subroutine lcpgeo(iway,phic,xlonc,truelat1,truelat2,xloc,yloc,
     &                  xlon,ylat)
c
c----CAMx v6.11 141215
c
c     LCPGEO performs Lambert Conformal to geodetic (lat/lon) translation
c
c     Code based on the TERRAIN preprocessor for MM5 v2.0,
c     developed by Yong-Run Guo and Sue Chen, National Center for
c     Atmospheric Research, and Pennsylvania State University
c     10/21/1993
c
c    Copyright 1996 - 2014
c     ENVIRON International Corporation
c
c     Modifications:
c        1/3/06              Now handles case with only 1 true lat
c
c     Input arguments:
c        iway                Conversion type
c                            0 = geodetic to Lambert Conformal
c                            1 = Lambert Conformal to geodetic
c        phic                Central latitude (deg, neg for southern hem)
c        xlonc               Central longitude (deg, neg for western hem)
c        truelat1            First true latitute (deg, neg for southern hem)
c        truelat2            Second true latitute (deg, neg for southern hem)
c        xloc/yloc           Projection coordinates (km)
c        xlon/ylat           Longitude/Latitude (deg)
c
c     Output arguments:
c        xloc/yloc           Projection coordinates (km)
c        xlon/ylat           Longitude/Latitude (deg)
c
c     Routines called:
c        none
c
c     Called by:
c        GRDPREP
      implicit none
c
      integer iway
      real phic,xlonc,truelat1,truelat2,xloc,yloc,xlon,ylat
c
      real conv,a,sign,pole,xn,psi1,psi0,xc,yc,flp,flpp,r,cell,
     &     rxn,cel1,cel2,psx,ylon
c
      data conv/57.29578/, a/6370./
c
c-----Entry Point
c
      if (phic.lt.0) then
        sign = -1.
      else
        sign = 1.
      endif
      pole = 90.
      if (abs(truelat1).gt.90.) then
        truelat1 = 60.
        truelat2 = 30.
        truelat1 = sign*truelat1
        truelat2 = sign*truelat2
      endif
      if (truelat1.eq.truelat2) then
        xn = sin(abs(truelat2)/conv)
      else
        xn = alog10(cos(truelat1/conv)) - alog10(cos(truelat2/conv))
        xn = xn/(alog10(tan((45. - abs(truelat1)/2.)/conv)) - 
     &           alog10(tan((45. - abs(truelat2)/2.)/conv)))
      endif
      psi1 = 90. - abs(truelat1)
      psi1 = psi1/conv
      if (phic.lt.0.) then
        psi1 = -psi1
        pole = -pole
      endif
      psi0 = (pole - phic)/conv
      xc = 0.
      yc = -a/xn*sin(psi1)*(tan(psi0/2.)/tan(psi1/2.))**xn
c
c-----Calculate lat/lon of the point (xloc,yloc)
c
      if (iway.eq.1) then
        xloc = xloc + xc
        yloc = yloc + yc
        if (yloc.eq.0.) then
          if (xloc.ge.0.) flp = 90./conv
          if (xloc.lt.0.) flp = -90./conv
        else
          if (phic.lt.0.) then
            flp = atan2(xloc,yloc)
          else
            flp = atan2(xloc,-yloc)
          endif
        endif
        flpp = (flp/xn)*conv + xlonc
        if (flpp.lt.-180.) flpp = flpp + 360.
        if (flpp.gt. 180.) flpp = flpp - 360.
        xlon = flpp
c
        r = sqrt(xloc*xloc + yloc*yloc)
        if (phic.lt.0.) r = -r
        if (truelat1.eq.truelat2) then
          cell = r/(a*tan(psi1))
        else
          cell = (r*xn)/(a*sin(psi1))
        endif
        rxn  = 1.0/xn
        cel1 = tan(psi1/2.)*cell**rxn
        cel2 = atan(cel1)
        psx  = 2.*cel2*conv
        ylat = pole - psx
c
c-----Calculate x/y from lat/lon
c
      else
        ylon = xlon - xlonc
        if (ylon.gt. 180.) ylon = ylon - 360.
        if (ylon.lt.-180.) ylon = ylon + 360.
        flp = xn*ylon/conv
        psx = (pole - ylat)/conv
        r = -a/xn*sin(psi1)*(tan(psx/2.)/tan(psi1/2.))**xn
        if (phic.lt.0.) then
          xloc = r*sin(flp)
          yloc = r*cos(flp)
        else
          xloc = -r*sin(flp)
          yloc =  r*cos(flp)
        endif
      endif
c
      xloc = xloc - xc
      yloc = yloc - yc
c
      return
      end
