       program cmaq2asc

c *** Variable declarations
      implicit none
      
      include 'PARMS3.EXT'
      include 'IODECL3.EXT'
      include 'FDESC3.EXT'

      INTEGER :: NCOLS, NROWS, NLAYS, NVARS, JDATE, JTIME, RUNLEN
      INTEGER :: TSTEP, NSTEPS, I,V, VMAX, UMAX, DMAX, NRECS
      CHARACTER*16    VNAME( MXVARS3 ) !  list of vble names, from user
      CHARACTER*16    UNITS( MXVARS3 ) !  list of vble units
      CHARACTER*80    VDESC( MXVARS3 ) !  list of vble descs
      CHARACTER*256   MESG    !  buffer for m3exit(), etc
      CHARACTER*16    INAME   !  logical name of the input file
      LOGICAL         EFLAG !flag: error has happened
             
      REAL    :: rlon0,rlat0,tlat1,tlat2,xloc,yloc,rlat,rlon,xpos,ypos
      INTEGER :: xorg,yorg,nx,ny,j,deltax,colrow,bigyear,distance
      integer    c, r, k, n, z, layer, hour, ihour, idate, nfiles
      integer icell, jcell, tday, starti,endi,startj,endj
      real lat, long,newpm25,junk,secpm25,projlat,projlong
      integer saveday(365),dayflag,tempcell,useryear,numberdays

      REAL, ALLOCATABLE :: GRID( :,:,:),other(:,:,:),ozone(:,:,:)
      REAL, ALLOCATABLE :: nh4(:,:,:),no3(:,:,:),so4(:,:,:),ec(:,:,:)
      REAL, ALLOCATABLE :: oc(:,:,:),pm25(:,:,:),cm(:,:,:),crustal(:,:,:)
      REAL, ALLOCATABLE :: tsdep(:,:,:),tndep(:,:,:)

      CHARACTER(LEN=16) :: progname
      CHARACTER(LEN=16) :: met, outfile
      CHARACTER(LEN=300) :: ename(366),fname,land
      CHARACTER(LEN=4)  :: pmspec

      INTEGER :: istatus
      INTEGER :: TRIMLEN
      real       envreal

      EXTERNAL TRIMLEN, envreal

c *** Initialize variables

      progname = 'NCF2SSIA'

      read(*,'(10x,a)') fname
      read(fname,*) pmspec
      write(*,*)'Variable extraction:', pmspec

      read(*,'(10x,a)') fname
      read(fname,*) projlat,projlong
      write(*,*)'Source latitude & longitude:', projlat,projlong

      read(*,'(10x,a)') fname
      read(fname,*) distance
      write(*,*)'Distance (km) from source to window domain:', distance
      write(*,*)'(Value <=0 means no windowing will be applied)'

      read(*,'(10x,a)') fname
      read(fname,*) useryear
      write(*,*) 'Year for SSIA input file:',useryear

      read(*,'(10x,a)') fname
      read(fname,*) rlon0,rlat0,tlat1,tlat2,nx,ny,xorg,yorg,deltax
      write(*,'(a,t20,3f10.0)') 'Projection:',rlon0,rlat0,tlat1,tlat2
      write(*,*) 'X and Y cells:',nx,ny
      write(*,*) 'x and y orig:',xorg,yorg,deltax

      read(*,'(10x,a)') fname
      open(11,file=fname,recl=1000)
      write(*,*)'Openend output SSIA input text file:',fname

      read(*,'(10x,a)') fname
      open(12,file=fname,recl=1000)
      write(*,*)'Openend output SSIA source location text file:',fname

      read(*,'(10x,a)') fname
      read(fname,*) numberdays
      write(*,*)'Number of input days:', numberdays

      read(*,'(10x,a)') fname
      read(fname,*) nfiles
      write(*,*) 'Number of input files:',nfiles

      do n = 1, nfiles
      read(*,'(10x,a)') ename(n)
      enddo

      do n = 1,nfiles
      write(*,*)n,ename(n)
      enddo

c----- write out header to text file

      if(pmspec.eq.'O3') then
      write(11,1998)
      write(11,1997)
      elseif(pmspec.eq.'TDEP') then
      write(11,1998)
      write(11,1996)
      else !write header for PM25
      write(11,1998)
      write(11,1999)
      endif

 1998 format('Day') 
 1999 format('_ID, _TYPE, LAT, LONG, DATE, CRUSTAL, NH4, SO4, EC, NO3, OC, PM25, CM, PM25SEC')
 1997 format('_ID, _TYPE, LAT, LONG, DATE, O3')
 1996 format('_ID, _TYPE, LAT, LONG, DATE, TSDEP, TNDEP')

c----- Write out source's location text file for SSIA

      write(12,1990)
      write(12,1991) projlat,projlong

 1990 format('ID,Lat,Long')
 1991 format('10001,',f15.10,',',f15.10)

c---- allocate and clear arrays

      allocate ( GRID(nx, ny, 1) )
      allocate ( other(nx,ny,numberdays))
      allocate ( ozone(nx,ny,numberdays))
      allocate ( no3(nx,ny,numberdays))
      allocate ( so4(nx,ny,numberdays))
      allocate ( oc(nx,ny,numberdays))
      allocate ( ec(nx,ny,numberdays))
      allocate ( crustal(nx,ny,numberdays))
      allocate ( cm(nx,ny,numberdays))
      allocate ( nh4(nx,ny,numberdays))
      allocate ( pm25(nx,ny,numberdays))
      allocate ( tsdep(nx,ny,numberdays))
      allocate ( tndep(nx,ny,numberdays))

       DO R = 1, ny
         DO C = 1, nx
           do V = 1, 365
            other(C,R,V) = 0.
           enddo
         ENDDO
       ENDDO

c---- open ioapi files

      dayflag = 0

      do N = 1, nfiles
      land = ename(N)

      if ( .not. open3( land, FSREAD3, progname ) ) THEN
         MESG = 'Could not open file "' //
     &   land( 1: TRIMLEN(land))
     &   // '" for input'
         CALL M3EXIT( progname, 0, 0, MESG, 2 )
      end if

      IF ( .NOT. DESC3(land))THEN
         MESG = 'Could not get description info for file "' //
     &            land( 1: TRIMLEN( land) ) //'"'
         CALL M3EXIT( progname, 0, 0, MESG, 2 )
      ENDIF

        NCOLS = NCOLS3D
        NROWS = NROWS3D
        NLAYS = NLAYS3D
        TSTEP = TSTEP3D
        NRECS = MXREC3D

        write(*,*) NCOLS,NROWS,NLAYS,TSTEP

C.......   Get max string-lengths for use in variables-listing:

        VMAX = TRIMLEN( VNAME3D( 1 ) )
        UMAX = TRIMLEN( UNITS3D( 1 ) )
        DMAX = TRIMLEN( VDESC3D( 1 ) )
        DO  I = 1, NVARS3D
            VMAX = MAX( VMAX , TRIMLEN( VNAME3D( I ) ) )
            UMAX = MAX( UMAX , TRIMLEN( UNITS3D( I ) ) )
            DMAX = MAX( DMAX , TRIMLEN( VDESC3D( I ) ) )
        END DO

        WRITE( *,92000 )
     &  ' ', 'The list of variables in this file is:', ' ',
     &  ( VNAME3D( I )( 1:VMAX ) // ' (' //
     &    UNITS3D( I )( 1:UMAX ) // '): ' //
     &    VDESC3D( I )( 1:DMAX ), I = 1, NVARS3D )

        write(*,*) NVARS3D

         NVARS = NVARS3D
         DO  V = 1, NVARS3D
         VNAME( V ) = VNAME3D( V )
         UNITS( V ) = UNITS3D( V )
         END DO

c---- read in variables and do averaging

       do z = 1, NRECS

       JDATE = SDATE3D
       JTIME = STIME3D

       tday = JDATE - int(JDATE/1000)*1000
c       saveday(tday) = SDATE3D
       dayflag=dayflag + 1
       saveday(dayflag) = useryear*1000 + tday

       write(*,*) SDATE3D,STIME3D,tday,dayflag,saveday(dayflag)

        DO  V = 1, NVARS

            IF ( .NOT. READ3( land, VNAME( V ), ALLAYS3,
     &                        JDATE, JTIME, GRID ) ) THEN
                MESG = 'Read failure:  file ' // land //
     &                 ' variable ' // VNAME( V )
                CALL M3EXIT( 'VERTOT:VERSTEP', JDATE, JTIME,
     &                       MESG, 2 )
            END IF      !  if read3() failed

           DO R = 1, NROWS
            DO C = 1, NCOLS

              if(VNAME(V).eq.'PM25'.or.VNAME(V).eq.'PM25_TOT'.or.VNAME(V).eq.'PMIJ') then
              pm25( C,R, dayflag ) =  GRID( C,R,1 )
              elseif(VNAME(V).eq.'PNO3'.or.VNAME(V).eq.'PM25_NO3'.or.VNAME(V).eq.'ANO3IJ') then
              no3( C,R, dayflag ) =  GRID( C,R,1 )
              elseif(VNAME(V).eq.'PSO4'.or.VNAME(V).eq.'PM25_SO4'.or.VNAME(V).eq.'ASO4IJ') then
              so4( C,R, dayflag ) =  GRID( C,R,1 )
              elseif(VNAME(V).eq.'EC'.or.VNAME(V).eq.'PM25_EC'.or.VNAME(V).eq.'AECIJ') then
              ec( C,R, dayflag) =  GRID( C,R,1 )
              elseif(VNAME(V).eq.'OC'.or.VNAME(V).eq.'PM25_OC'.or.VNAME(V).eq.'AOCIJ') then
              oc( C,R, dayflag ) =  GRID( C,R,1 )
              elseif(VNAME(V).eq.'PNH4'.or.VNAME(V).eq.'PM25_NH4'.or.VNAME(V).eq.'ANH4IJ') then
              nh4( C,R, dayflag ) =  GRID( C,R,1 )
              elseif(VNAME(V).eq.'CRUSTAL'.or.VNAME(V).eq.'PM25_OTHR'.or.VNAME(V).eq.'PM_OTH') then
              crustal( C,R, dayflag ) =  GRID( C,R,1 )
              elseif(VNAME(V).eq.'CM'.or.VNAME(V).eq.'PMC_TOT'.or.VNAME(V).eq.'ATOTK') then
              cm( C,R, dayflag ) =  GRID( C,R,1 )
              elseif(VNAME(V).eq.'PM25_SODIUM'.or.VNAME(V).eq.'ANAIJ') then
              other( C,R,dayflag ) = other(C,R,dayflag) + GRID(C,R,1)
              elseif(VNAME(V).eq.'PM25_CL'.or.VNAME(V).eq.'ACLIJ') then
              other( C,R, dayflag ) =  other(C,R,dayflag) + GRID( C,R,1 )
              elseif(VNAME(V).eq.'ASEAS') then
              other( C,R, dayflag ) =  other(C,R,dayflag) + GRID( C,R,1 )
              elseif(VNAME(V).eq.'O3'.or.VNAME(V).eq.'O38HRMX') then
              ozone( C,R,dayflag ) = GRID(C,R,1)
              elseif(VNAME(V).eq.'TSDEP'.or.VNAME(V).eq.'TOTSULFDEP') then
              tsdep( C,R,dayflag ) = GRID(C,R,1)
              elseif(VNAME(V).eq.'TNDEP'.or.VNAME(V).eq.'TOTNITRDEP') then
              tndep( C,R,dayflag ) = GRID(C,R,1)
              endif

            ENDDO
           ENDDO

        ENDDO !end loop over variables on file

        SDATE3D = SDATE3D + 1
       enddo !go to next time record on this file


      if ( .not. close3 ( land ) ) THEN
         MESG = 'Could not close file'
         CALL M3EXIT( progname, 0, 0, MESG, 2 )
      end if

        ENDDO !end loop over number of input files 


c------ source location on grid

      call lcpgeo(0,rlat0,rlon0,tlat1,tlat2,xloc,yloc,projlong,projlat)

        icell = int(( (xloc) - XORG)/DELTAX) + 1
        jcell = int(( (yloc) - YORG)/DELTAX) + 1

      write(*,*)'Source grid location:',xloc,yloc,icell,jcell
      if(distance.gt.0) then
      write(*,*)'SW corner of windowed domain:'
      write(*,*) int(((xloc-distance)-XORG)/DELTAX)+1,int(((yloc-distance)-YORG)/DELTAX)+1
      write(*,*)'NE coordner of windowed domain:'
      write(*,*) int(((xloc+distance)-XORG)/DELTAX)+1,int(((yloc+distance)-YORG)/DELTAX)+1

      endif 

c------ output MATS file

       do tday = 1,numberdays

       if(distance.le.0) then !full domain extent
       starti = 1
       endi = NCOLS
       startj = 1
       endj = NROWS
       else !window domain
       starti = int(((xloc-distance)-XORG)/DELTAX)+1
       endi = int(((xloc+distance)-XORG)/DELTAX)+1
       startj = int(((yloc-distance)-YORG)/DELTAX)+1
       endj = int(((yloc+distance)-YORG)/DELTAX)+1
       endif

       do i = starti, endi
        do j = startj, endj

         colrow = i*1000 + j

          xpos = (XORG + (i - 0.5)*DELTAX)
          ypos = (YORG + (j - 0.5)*DELTAX)
          call lcpgeo(1,rlat0,rlon0,tlat1,tlat2,xpos,ypos,
     &                  long,lat)

        bigyear = saveday(tday)
        call caldate(bigyear)

        newpm25 = crustal(i,j,tday) + so4(i,j,tday) + ec(i,j,tday) + no3(i,j,tday) + oc(i,j,tday) + nh4(i,j,tday)

        if(pmspec.eq.'SULF') secpm25 = so4(i,j,tday)
        if(pmspec.eq.'NITR') secpm25 = no3(i,j,tday)
        if(pmspec.eq.'AMMN') secpm25 = nh4(i,j,tday)
        if(pmspec.eq.'PM25') secpm25 = newpm25
        if(pmspec.eq.'SNAS') secpm25 = so4(i,j,tday) + no3(i,j,tday) + nh4(i,j,tday)

        if(pmspec.eq.'O3') then
        write(11,1115)colrow,lat,long,bigyear,ozone(i,j,tday)
        elseif(pmspec.eq.'TDEP') then
        write(11,1116)colrow,lat,long,bigyear,tsdep(i,j,tday),tndep(i,j,tday)
        else
        write(11,1117) colrow,lat,long,bigyear,crustal(i,j,tday),nh4(i,j,tday),so4(i,j,tday),ec(i,j,tday),
     &      no3(i,j,tday),oc(i,j,tday),pm25(i,j,tday),cm(i,j,tday),secpm25
        endif

        enddo
       enddo
       enddo
 

 1115  format(i6,',"",',f10.6,',',f11.6,',',i8,',',f10.5)
 1116  format(i6,',"",',f10.6,',',f11.6,',',i8,',',f10.5,',',f10.5)
 1117  format(i6,',"",',f10.6,',',f11.6,',',i8,9(',',f10.5))

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91000   FORMAT ( //5X , '*** ERROR ABORT in program VERTOT ***',
     &            /5X , A ,
     &           // )        !  generic error message format

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

92999   FORMAT ( //5X , A, // )

C...........   Formatted file I/O formats............ 93xxx

93000   FORMAT ( A16 )

C...........   Internal buffering formats............ 94xxx

C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


      stop

      END


c-----Start subroutines
      subroutine lcpgeo(iway,phic,xlonc,truelat1,truelat2,xloc,yloc,
     &                  xlon,ylat)
c
c     LCPGEO performs Lambert Conformal to geodetic (lat/lon) translation
c
c     Code based on the TERRAIN preprocessor for MM5 v2.0,
c     developed by Yong-Run Guo and Sue Chen, National Center for
c     Atmospheric Research, and Pennsylvania State University
c     10/21/1993
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
      xn = alog10(cos(truelat1/conv)) - alog10(cos(truelat2/conv))
      xn = xn/(alog10(tan((45. - sign*truelat1/2.)/conv)) -
     &         alog10(tan((45. - sign*truelat2/2.)/conv)))           
      psi1 = 90. - sign*truelat1
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
        cell = (r*xn)/(a*sin(psi1))
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

      subroutine caldate(idate)

      integer idate
      dimension nday(12)
      data nday/31,28,31,30,31,30,31,31,30,31,30,31/

c
c-----Entry point
c
c-----If it is already in calender date, return
c

      icent = int(idate/100000)
      iyear = int((idate - icent*100000)/1000)
      jday = idate - icent*100000 - iyear*1000

      nday(2) = 28
c uncomment line below if base year is a leap year
c      if (mod(iyear,4).eq.0) nday(2) = 29
      mday = 0
      do imonth = 1,12
        mday = mday + nday(imonth)
        if (mday.ge.jday) go to 20
      enddo
 20   iday = jday - (mday - nday(imonth))
      idate = icent*1000000 + iyear*10000 + imonth*100 + iday
c
c      write(*,'(4i6)') icent,iyear,imonth,iday

      return
      end
