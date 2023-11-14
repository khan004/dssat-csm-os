C=======================================================================
C  PT_BTHTIME, Subroutine
C
C  Determines Beta thermal time calculations
C-----------------------------------------------------------------------
C  Revision history
C
C               Written
C  10/30/2023 MSKhan added Beta thermal time sub-routine following Yin et al., 2003 
C  
C=======================================================================

      SUBROUTINE PT_BTHTIME (
     &    ISTAGE, TMAX, TMIN, DIF, DAYL, TBD, TOD, TCD, TSEN, SBD,  !Input
     &    SOD, SCD, SSEN,         !Input
     &    TDU, SDU, ETRM)                                           !Output
      IMPLICIT NONE
      !INTEGER DS
      INTEGER ISTAGE
      REAL TMAX, TMIN, DIF, DAYL, TBD, TOD, TCD, TSEN, TDU, SDU, ETRM
      REAL SBD, SOD, SCD, SSEN
      REAL SUNRIS, SUNSET, TMEAN, TT, ST, ETR, TD, SU, TU, Q10, IETR
      INTEGER I
      SAVE

! Mathematical equation to fortran variables mapping:
! TCD is Tc, TD is T, TOD is To, TBD is Tb, and TSEN is ct. 
! Constants Tb = 5.5 ˚C, To= 23.4 ˚C, Tc= 34.6 ˚C, ct = 1.6
! i.e. TBD=5.5, TOD=23.4, TCD=34.6, TSEN=1.6, respectively
! As TBD, TOD, TCD and TSEN are declared as input variables,
! these constant values must be provided at the call site.

!*---timing for sunrise and sunset
      SUNRIS = 12.0 - 0.5*DAYL
      SUNSET = 12.0 + 0.5*DAYL

!*---mean daily temperature
      TMEAN  = (TMAX + TMIN)/2.0
      TT     = 0.0
      ST     = 0.0
      ETR    = 0.0
      TDU    = 0.0
      SDU    = 0.0
      ETRM   = 0.0

!*---diurnal course of temperature
      DO 10 I = 1, 24
        IF (I.GE.SUNRIS .AND. I.LE.SUNSET) THEN
          TD = TMEAN+DIF+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
        ELSE
          TD = TMEAN    +0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
        ENDIF

!*---assuming development rate at supra-optimum (above optimum) temperatures during
!*   the reproductive phase equals that at the optimum temperature
        ! IF (DS.GT.1.) THEN
        IF (ISTAGE.EQ.2) THEN
           TD = MIN (TD,TOD)
        ELSE
           TD = TD
        ENDIF

!*---instantaneous thermal unit based on bell-shaped temperature response
        IF (TD.LT.TBD .OR. TD.GT.TCD) THEN
           TU = 0.0
        ELSE
           TU = (((TCD-TD)/(TCD-TOD))*((TD-TBD)/(TOD-TBD))**
     &          ((TOD-TBD)/(TCD-TOD)))**TSEN
        ENDIF

        IF (TD.LT.SBD .OR. TD.GT.SCD) THEN
           SU = 0.0
        ELSE
           SU = (((SCD-TD)/(SCD-SOD))*((TD-SBD)/(SOD-SBD))**
     &          ((SOD-SBD)/(SCD-SOD)))**SSEN
        ENDIF

        TT = TT + TU/24.0
        ST = ST + SU/24.0

!*---effect of instantaneous temperature on maintenance respiration
        Q10  = 2.0
        IETR = Q10**((TD-20.0)/10.0)
        ETR  = ETR + IETR/24.0
  10  CONTINUE

!*---daily thermal unit for phenology
      IF (ISTAGE .LE. 4) THEN
        TDU  = TT
      END IF
      IF (ISTAGE .GE. 6 .OR. ISTAGE .LE. 2) THEN
        SDU  = ST
      END IF

!*---daily average of temperature effect on maintenance respiration
      ETRM = ETR
        
      RETURN
      END SUBROUTINE PT_BTHTIME