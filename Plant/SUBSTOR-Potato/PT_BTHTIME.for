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
                         ! parameters, hourly weather data.
      
      IMPLICIT NONE
      INTEGER ISTAGE, L0, I
      REAL TMAX, TMIN, TBD, TOD, TCD, TSEN, TDU, SDU 
      REAL SBD, SOD, SCD, SSEN
      REAL TT, TU, SS, SU, TMEAN  
      REAL ST(NL)
      SAVE
          
!*---mean daily temperature
      TMEAN  = (TMAX + TMIN)/2.0
      TT     = 0.0
      SS     = 0.0
      TDU    = 0.0
      SDU    = 0.0

!*---instantaneous thermal unit based on bell-shaped temperature response
        IF (TMEAN.LT.TBD .OR. TMEAN.GT.TCD) THEN
           TU = 0.0
        ELSE
           TU = (((TCD-TMEAN)/(TCD-TOD))*((TMEAN-TBD)/(TOD-TBD))**
     &          ((TOD-TBD)/(TCD-TOD)))**TSEN
        ENDIF

        IF (ST(L0).LT.SBD .OR. ST(L0).GT.SCD) THEN
           SU = 0.0
        ELSE
           SU = (((SCD-ST(L0))/(SCD-SOD))*((ST(L0)-SBD)/(SOD-SBD))**
     &          ((SOD-SBD)/(SCD-SOD)))**SSEN
        ENDIF

        TT = TT + TU
        SS = SS + SU

!*---daily thermal unit for phenology
      IF (ISTAGE .LE. 4) THEN
        TDU  = TT
      END IF
      IF (ISTAGE .GE. 6 .OR. ISTAGE .LE. 2) THEN
        SDU  = SS
      END IF
        
      RETURN
      END SUBROUTINE PT_BTHTIME
