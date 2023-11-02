!*----------------------------------------------------------------------*
!*  SUBROUTINE PT_BTHTIME                                               *
!*  Purpose: This subroutine calculates the daily amount of thermal day *
!*           and the average effect of daily temperature on maintenance *
!*           respiration                                                *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  DS      R4  Development stage                            -       I  *
!*  TMAX    R4  Daily maximum temperature                    oC      I  *
!*  TMIN    R4  Daily minimum temperature                    oC      I  *
!*  DIF     R4  Daytime plant-air temperature differential   oC      I  *
!*  DAYL    R4  Astronomic daylength (base = 0 degrees)      h       I  *
!*  TBD     R4  Base temperature for phenology               oC      I  *
!*  TOD     R4  Optimum temperature for phenology            oC      I  *
!*  TCD     R4  Ceiling temperature for phenology            oC      I  *
!*  TSEN    R4  Curvature for temperature response           -       I  *
!*  TDU     R4  Daily thermal-day unit                       -       O  *
!*  ETRM    R4  Daily mean temperature effect on maint. resp -       O  *
!*----------------------------------------------------------------------*
      SUBROUTINE PT_BTHTIME (
     &    DS, TMAX, TMIN, DIF, DAYL, TBD, TOD, TCD, TSEN,     !Input
     &    TDU, ETRM)                                          !Output
      IMPLICIT NONE
      INTEGER DS
      REAL TMAX, TMIN, DIF, DAYL, TBD, TOD, TCD, TSEN, TDU, ETRM
      REAL SUNRIS, SUNSET, TMEAN, TT, ETR, TD, TU, Q10, IETR
      INTEGER I
      SAVE

! Mathematical equation to fortran variables mapping:
! TCD is Tc, TD is T, TOD is To, TBD is Tb, and TSEN is ct. 
! Constants Tb = 5.5 ˚C, To= 23.4 ˚C, Tc= 34.6 ˚C, ct = 1.6
! i.e. TBD=5.5, TOD=23.4, TCD=34.6, TSEN=1.6
! As TBD, TOD, TCD and TSEN are declared as input variables,
! these constant values must be provided at the call site.

!*---timing for sunrise and sunset
      SUNRIS = 12.0 - 0.5*DAYL
      SUNSET = 12.0 + 0.5*DAYL

!*---mean daily temperature
      TMEAN  = (TMAX + TMIN)/2.0
      TT     = 0.0
      ETR    = 0.0
      
!*---diurnal course of temperature
      DO 10 I = 1, 24
        IF (I.GE.SUNRIS .AND. I.LE.SUNSET) THEN
          TD = TMEAN+DIF+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
        ELSE
          TD = TMEAN    +0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
        ENDIF

!*---assuming development rate at supra-optimum temperatures during
!*   the reproductive phase equals that at the optimum temperature
        IF (DS.GT.1.) THEN
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

        TT = TT + TU/24.0

!*---effect of instantaneous temperature on maintenance respiration
        Q10  = 2.0
        IETR = Q10**((TD-20.0)/10.0)
        ETR  = ETR + IETR/24.0
  10  CONTINUE

!*---daily thermal unit for phenology
      TDU  = TT

!*---daily average of temperature effect on maintenance respiration
      ETRM = ETR
      RETURN
      END SUBROUTINE PT_BTHTIME

!*-------------------------------------------------------------------