C=======================================================================
C  PT_BTHTIME, Subroutine
C
C  Determines Beta thermal time calculations
C-----------------------------------------------------------------------
C  Revision history
C
C               Written
C  10/30/2023 MSKhan added Beta thermal time sub-routine following
C  Yin et al., 2003 and Khan et al., 2019_Field_Crops_Res_242
C
C=======================================================================

      SUBROUTINE PT_BTHTIME (
     &    ISTAGE, L0, ST, TMAX, TMIN, TBD, TOD, TCD,   !Input
     &    TSEN, SBD, SOD, SCD, SSEN,                   !Input
     &    TDU, SDU)                                    !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, daily weather data.

      IMPLICIT NONE

      INTEGER ISTAGE, L0

      REAL TMAX, TMIN, TBD, TOD, TCD, TSEN, TDU, SDU
      REAL SBD, SOD, SCD, SSEN
      REAL TU, SU, TMEAN
      REAL ST(NL)

!-----------------------------------------------------------------------
!     Initialize outputs
!-----------------------------------------------------------------------

      TDU   = 0.0
      SDU   = 0.0

!-----------------------------------------------------------------------
!     Beta air thermal time
!
!     Same stage connection as default PT_THTIME:
!     TDU is calculated only when ISTAGE .LE. 4.
!     TDU is calculated for ISTAGE = 1, 2, 3, 4
!     TMEAN is the mean daily air temperature.
!-----------------------------------------------------------------------

      IF (ISTAGE .LE. 4) THEN

         TMEAN = (TMAX + TMIN)/2.0

!        Instantaneous thermal unit based on
!        bell-shaped Beta temperature response

         IF (TMEAN .LT. TBD .OR. TMEAN .GT. TCD) THEN
            TU = 0.0
         ELSE
            TU = (((TCD-TMEAN)/(TCD-TOD)) *
     &            ((TMEAN-TBD)/(TOD-TBD)) **
     &            ((TOD-TBD)/(TCD-TOD))) ** TSEN
         END IF

         TDU = TU

      END IF

 !-----------------------------------------------------------------------
!     Beta soil thermal time
!
!     Same stage connection as default PT_THTIME:
!     SDU is calculated when
!     ISTAGE .GE. 6 .OR. ISTAGE .LE. 2.
!     SDU is calculated for ISTAGE = 6, 7, 1, 2
!     ST(L0) is the soil temperature in the selected soil layer.
!
!-----------------------------------------------------------------------

      IF (ISTAGE .GE. 6 .OR. ISTAGE .LE. 2) THEN

         IF (ST(L0) .LT. SBD .OR. ST(L0) .GT. SCD) THEN
            SU = 0.0
         ELSE
            SU = (((SCD-ST(L0))/(SCD-SOD)) *
     &            ((ST(L0)-SBD)/(SOD-SBD)) **
     &            ((SOD-SBD)/(SCD-SOD))) ** SSEN
         END IF

         SDU = SU

      END IF

!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE PT_BTHTIME
