

      IMPLICIT NONE
      INCLUDE SUBST_PARMS_ID      ! I/O parameters definitions
      INCLUDE SUBST_FDESC_ID      ! file header data structuer
      INCLUDE SUBST_IODECL_ID     ! I/O definitions and declarations

      INTEGER :: RUNLEN,TRIMLEN
      INTEGER :: TSTEP, NSTEPS, V, VMAX, UMAX, DMAX
      CHARACTER*16    VNAME( MXVARS3 ) !  list of vble names, from user
      CHARACTER*16    UNITS( MXVARS3 ) !  list of vble units
      CHARACTER*80    VDESC( MXVARS3 ) !  list of vble descs
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
      REAL X, Y, DX, DY, XORIG, YORIG

      CHARACTER( 16 ) :: CTM_NVARS = 'CTM_NVARS'
      CHARACTER( 16 ) :: AIR_NOBS = 'AIR_NOBS'
      CHARACTER( 16 ) :: ALT_AGL = 'ALT_AGL'
      CHARACTER( 80 ) :: VARDESC
      CHARACTER( 96 ) :: XMSG = ' '
      CHARACTER( 96 ) :: MSG1 = ' '

      CHARACTER (16), ALLOCATABLE:: OUTVAR ( : )
      REAL, ALLOCATABLE:: ZF(:, : , : , : )
      REAL, ALLOCATABLE:: COUT( : , : , : )

C..........External Functions :
      INTEGER, EXTERNAL :: ENVINT    !  get environment variable as integer
      INTEGER STATUS


c above ground level or above sea level
      ALTFLAG = 0  !0 means above sea level
      VARDESC = '0=above sea level 1=above ground level'
      ALTFLAG  = ENVINT( ALT_AGL, VARDESC, ALTFLAG, STATUS )
      write(*,*)'>> ALTFLAG (0=above sea level 1=above ground level) = ', ALTFLAG


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

      ALLOCATE  ( ZF(NCOLS, NROWS, NLAYS, NVARS3D),
     &            COUT(NCOLS, NROWS,NLAYS) )


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

                NVARS = NVARS3D
                DO  V = 1, NVARS3D
                 VNAME( V ) = VNAME3D( V )
                 UNITS( V ) = UNITS3D( V )
                 VDESC( V ) = VDESC3D( V )
                 EFLAG = EFLAG .OR. ( VTYPE3D( V ) .NE. M3REAL )
                 write(*,*) V,VNAME3D(V),UNITS3D(V)
                END DO


c    zero out array

        do  V = 1, NVARS
        do c = 1, NCOLS
        do r = 1, NROWS
        do s = 1, NLAYS
         zf(c,r,s,V) = 0.
        enddo
        enddo
        enddo
        enddo
        
c    start reading files


        DO  V = 1, NVARS

      JTIME = STIME3D
      JDATE = SDATE3D

        DO N = 1, NSTEPS

         
        IF ( .NOT. READ3( METCRO3D, VNAME( V ), ALLAYS3,JDATE, JTIME, COUT )) THEN
          MESG = 'Read failure:  file ' // METCRO3D //
     &                 ' variable ' // VNAME( V )
          CALL M3EXIT( 'VERTOT:VERSTEP', JDATE, JTIME,MESG, 2 )
         ENDIF      !  if read3() failed

        write(*,*) 'Processing:',VNAME(V),JDATE,JTIME

        do s = 1, NLAYS
        do c = 1, NCOLS
        do r = 1, NROWS
c         zf(c,r,s,V) = zf(c,r,s,V) + COUT(c,r,s)
         zf(c,r,s,V) = amax1( zf(c,r,s,V) , COUT(c,r,s) )
        enddo
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

c do math    

c        DO  V = 1, NVARS
c        do s = 1, NLAYS
c        do c = 1, NCOLS
c        do r = 1, NROWS
c         if(zf(c,r,s,V).gt.0) then
c          zf(c,r,s,V) = zf(c,r,s,V) / NSTEPS
c         else
c          zf(c,r,s,V) = 0.
c         endif
c        enddo
c        enddo
c        enddo
c        enddo
 

c open new output file

c      NLAYS3D = 1
      TSTEP3D = 240000


      IF ( .NOT. OPEN3( OUTFILE, FSCREA3, PROGNAME ) ) THEN
            CALL M3ERR( PROGNAME, JDATE, JTIME,
     &           'Could not create ' // OUTFILE // ' file', .TRUE. )
      END IF                 !  if OPEN3() failed

        DO  V = 1, NVARS

       IF ( .NOT. WRITE3( OUTFILE, VNAME(V), FSDATE,FSTIME,zf(1,1,1,V) ) ) THEN
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
