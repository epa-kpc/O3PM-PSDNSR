      PROGRAM CRT_PT_EMIS
!  Purpose: Read in a text format of emission information file
!    then based on the environmental variable to create different
!    emission file for CMAQ (inline, stack_group), CAMX, and SCICHEM
!  Environmental variables used
!      EMISINPUT  --> text format of emission information file
!      GRIDDESC   --> Grid description file
!      PTCOORDTYPE --> LAM or LL
!      MODEL      --> CMAQ, CAMX or SCICHEM
!       program will based on the MODEL to determine what other
!       input files or output files need be defined
!      if MODEL == SCICHEM
!        IMC_FILE(IN)  --> file contains the speciation table
!        SCN_FILE(OUT) --> file contains the emission rate, grid information
!      if MODEL == CAMQ
!        STACK_GROUP(OUT) --> stack group file
!        INLINE_FILE(OUT) --> inline file
!      if MODEL == CAMX
!        PT_FILE(OUT) --> point source file
!***********************************************************************
      USE MODINFO
      IMPLICIT NONE

      INCLUDE 'PARMS3.EXT'
      INCLUDE 'FDESC3.EXT'
      INCLUDE 'IODECL3.EXT'
      INCLUDE 'STATE3.EXT'

!...define variables to save description
      INTEGER IOST
      REAL X, Y
      INTEGER I, J, K, L

!...define external functions
      LOGICAL         DSCGRID
      REAL            GETREAL
      LOGICAL         GETYN
      INTEGER         TRIMLEN
      INTEGER         PROMPTFFILE
      INTEGER         ENVINT
      INTEGER         JUNIT
      INTEGER         GETEFILE
      LOGICAL         LAMBERT

      EXTERNAL GETREAL, GETYN, TRIMLEN, JUNIT,
     &      DSCGRID, PROMPTFFILE, GETEFILE, LAMBERT

!...define arrays to store variable definitions

      CHARACTER*16      :: PROGNAME
      CHARACTER*16      :: ANAME
      CHARACTER*160     :: MESG
      CHARACTER*160     :: EMISINPUTNAME
      CHARACTER*260     :: OUTFILENAME
      LOGICAL           :: RDONLY, FMTFLAG
      LOGICAL           :: LAMSET
      INTEGER           :: IUNIT1, FDEV
      INTEGER           :: N,I1, J1, K1, L1
      INTEGER           :: STATUS
      CHARACTER*100     :: FMTBUF = ' '       ! format buffer
      CHARACTER*256     :: NEWSCNNAME
      CHARACTER*16      :: LAMBERNAME

!*****************************************************************************
!!!!! Main process start here                                                *
!*****************************************************************************
      DEBUG = 2
      LOGDEV = INIT3()
      PROGNAME   = 'CRT_PT_EMIS'
      RDONLY = .TRUE.
      FMTFLAG = .TRUE.

      MESG = 'Enter value for MODELNAME (CMAQ, CAMX or SCICHEM) '
      CALL GETSTR( MESG, '', MODELNAME)
      CALL UPCASE(MODELNAME)

      MESG = 'Enter value for PTCOORDTYPE (LAMBERT or LATLONG)'
      CALL GETSTR(MESG, 'LAMBERT', PTCOORDTYPE)
      CALL UPCASE(PTCOORDTYPE)

      IUNIT1 = PROMPTFFILE('Enter logical name for EMISINPUT',
     &   RDONLY, FMTFLAG, 'EMISINPUT', PROGNAME)

      ! Initialize information related to inline
      TOTAL_NUM_POL = 0
      SIZE_OF_MASTERLIST = 10
      ALLOCATE(MASTERPOLLIST(SIZE_OF_MASTERLIST))

      CALL RDEMISFILE(IUNIT1)

       ! GDNAM3D is coming from GRIDNAME_INFO
        GDNAM3D = GRIDNAME_INFO
        IF ( .NOT. DSCGRID( GDNAM3D, ANAME, CTYPE,
     &                      P_ALP3D, P_BET3D, P_GAM3D,
     &                      XCENT3D, YCENT3D, XORIG3D,
     &                      YORIG3D, XCELL3D, YCELL3D,
     &                      NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

            MESG = 'Grid "' // GDNAM3D( 1:TRIMLEN( GDNAM3D ) ) //
     &             '" not found in GRIDDESC file'
            CALL M3WARN( PROGNAME, 0, 0, MESG )
        END IF          !  if DSCGRID failed
      P_ALP = P_ALP3D
      P_BET = P_BET3D
      P_GAM = P_GAM3D
      XCENT = XCENT3D
      YCENT = YCENT3D
      XCELL = XCELL3D
      YCELL = YCELL3D
      XORIG = XORIG3D
      YORIG = YORIG3D
      NCOLS = NCOLS3D
      NROWS = NROWS3D
      NTHIK = NTHIK3D
      WRITE(*,*) 'XCELL = ', XCELL
      WRITE(*,*) 'YCELL = ', YCELL

      CALL CONVERTDATE()

      IF (PTCOORDTYPE .EQ. 'LATLONG') THEN
         LAMBERNAME = GRIDNAME_INFO
         LAMSET = LAMBERT(LAMBERNAME,P_ALP, P_BET, P_GAM, XCENT, YCENT)
        IF ( .NOT. LAMSET ) THEN
                CALL M3WARN( PROGNAME, 0, 0,
     &          'Error getting projection' )
        END IF
        DO I = 1, NUM_STACKS
           CALL LL2LAM(LONGITUDE(I),LATITUDE(I),X_COORD(I),Y_COORD(I))
           WRITE(*,*)'LAMBERT (X,Y) = ', X_COORD(I), Y_COORD(I)
        END DO
      END IF

      FIPS_SUPPORT = GETYN( 'Do you want to set FIPS for point source
     &     (YES or NO)', .FALSE. )
      IF (FIPS_SUPPORT) THEN
         CALL GETFIPS()
      END IF

      CALL UPCASE(MODELNAME)
      SELECT CASE(MODELNAME)
      CASE('SCICHEM')
         CALL RDSCIIMC()
         ! Get base name of SCICHEM SCN
         MESG = 'Enter logical name for SCNNAME '
         CALL GETSTR( MESG, '', SCNNAME)

         DO I = 1, NUM_STACKS
            ! Crate proper SCN filename
            WRITE( FMTBUF, '(I5.5)' ) I
            WRITE(*,*) 'FMTBUF = ', FMTBUF
            WRITE(*,*) 'SCNNAME = ', SCNNAME
            NEWSCNNAME = TRIM(SCNNAME) // TRIM(FMTBUF(1:5))
            WRITE(*,*) 'NEWSCNNAME = ', NEWSCNNAME
            FDEV = JUNIT()
            OPEN(FDEV,FILE=NEWSCNNAME,STATUS='NEW')
            ! new filename = SCNNAME //
            ! Open the filename then pass the fdev to WRSCISCN
            CALL WRSCISCN(FDEV,I)
         END DO
      CASE('CMAQ')
         CALL WRSTKGRP()
         CALL WRINLINE()
      CASE( 'CAMX' )
         CALL WRCAMX()
      CASE DEFAULT
            MESG = 'ERROR: Unrecognized MODEL_NAME setting; valid ' //
     &             'settings are CMAQ, SCICHEM, CAMX'
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
      END SELECT

500   Format(A)

      END
