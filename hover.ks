Parameter Mode is "Radar".
Parameter HoverAlt is RoundTens(Alt:Radar).
Parameter Silenced is False.
Parameter Steer is True.
Parameter LimitTWR is 100.
Parameter Correction is 1.9.

If Body:Name = "Kerbin" Set Correction to 1.9.
If Body:Name = "Mun" Set Correction to 14.

Set Config:IPU to 500. Set Ship:Control:PilotMainThrottle to 0.
Set ShipHeight to Round(GetShipHeight()). Lock TrueRadar to Alt:Radar - ShipHeight. Lock TrueAltitude to Altitude - ShipHeight. Lock TargetAlt to HoverAlt. Set TargetSpeed to 0.
Set HoverHeight to List(Time:Seconds, 0). Set IdealThrottle to 0. Set Mode to "Radar". Set SBurning to False. Set KillHSpeed to True. Set UseRCS to True. Set Started to True.
Set Throttle_PID to PIDLoop(0.3, 0.3, 0.005, 0, 1).

If Silenced = False {
  ClearScreen.
  Print "                     HOVER                    " at (0, 2).
  Print "         .===========================.         " at (0, 4).
  Print "         | (M) Mode:                 |         " at (0, 5).
  Print "     .===================================.    " at (0, 6).
  Print "     | (< >) Hover at:                   |    " at (0, 7).
  Print "  .=========================================. " at (0, 8).
  Print "  | Altitude:          | V-Speed:           | " at (0, 9).
  Print "  |-----------------------------------------| " at (0, 10).
  Print "  | Radar:             | H-Speed:           | " at (0, 11).
  Print "  |-----------------------------------------| " at (0, 12).
  Print "  | Throttle:          | (T) Limit TWR:     | " at (0, 13).
  Print "  |=========================================| " at (0, 14).
  Print "                                              " at (0, 15).
  Print "     H = Hover Here          L = Land         " at (0, 16).
  Print "                                              " at (0, 17).
  Print "     K = Kill H-Speed        Q = Exit         " at (0, 18).
  Print "                                              " at (0, 19).
  Print "     R = RCS                                  " at (0, 20).
} 

If Steer = True Lock Steering to LookDirUp(UP:ForeVector, Ship:Facing:TopVector). //If AvailableThrust = 0 Set Started to False.

Until Started = False { SAS Off.
  If Silenced = False {

    Print Mode + "               " at (21, 5). Print "|" at (37, 5).

    If Mode = "Radar" or Mode = "Altitude" { Print FormatDistance(HoverAlt, 2) + Spacer(FormatDistance(HoverAlt, 2):Length, 10) at (28, 7). } Else If Mode = "Vertical Speed" { Print Round(TargetSpeed) + " m/s" + Spacer((Round(TargetSpeed) + " m/s"):Length, 10) at (28, 7). }
 
    Print FormatDistance(TrueAltitude, 1) + Spacer(FormatDistance(TrueAltitude, 1):Length, 9) at (14, 9).						Print Round(VerticalSpeed) + " m/s" + Spacer((Round(VerticalSpeed) + " m/s"):Length, 10) at (34, 9).
    Print FormatDistance(TrueRadar, 1) + Spacer(FormatDistance(TrueRadar, 1):Length, 9) at (14, 11).							Print Round(GroundSpeed) + " m/s" + Spacer((Round(GroundSpeed) + " m/s"):Length, 10) at (34, 11).
    Print Round(Max(Min(IdealThrottle, 1), 0) * 100) + " %" + Spacer((Round(Max(Min(IdealThrottle, 1), 0) * 100) + " %"):Length, 9) at (14, 13).	If LimitTWR = 100 {  Print "Max" + Spacer("Max":Length, 4) at (40, 13). } Else { Print FormatDistance(LimitTWR * 1000, 1, False) + Spacer((FormatDistance(LimitTWR * 1000, 1, False) + ""):Length, 4) at (40, 13). }

    If KillHSpeed = True { Print "     K = Kill H-Speed (ON)" at (0, 18). } Else { Print "     K = Kill H-Speed     " at (0, 18). }
    If UseRCS = True { Print "     R = RCS (ON)" at (0, 20). } Else { Print "     R = RCS     " at (0, 20). }

    If Terminal:Input:HasChar { Set ch to Terminal:Input:GetChar().

      If UnChar(ch) = 109 {
        If Mode = "Radar" {                 Print "(< >) Hover at:                  " at (7, 7). Set Mode to "Altitude". Print "     H = Hover Here        " at (0, 16). Set HoverAlt to RoundTens(TrueAltitude).
        } Else If Mode = "Altitude" {       Print "(< >) Speed:                     " at (7, 7). Set Mode to "Vertical Speed". Print "     H = Kill V-Speed      " at (0, 16). Set TargetSpeed to MIN(MAX(Round(VerticalSpeed), -300), 300). Lock IdealThrottle to Throttle_PID:Update(Time:Seconds, VerticalSpeed). Lock Throttle to IdealThrottle.
        } Else If Mode = "Vertical Speed" { Print "---------------------------------" at (7, 7). Set Mode to "Anti Gravity". Print "     H = Hover Here        " at (0, 16). Set IdealThrottle to 0. Lock Throttle to IdealThrottle.
        } Else If Mode = "Anti Gravity" {   Print "(< >) Hover at:                  " at (7, 7). Set Mode to "Radar". Print "     H = Hover Here        " at (0, 16). Set HoverAlt to RoundTens(TrueRadar).
        }
      }
      If UnChar(ch) = 77 {
        If Mode = "Radar" {                 Print "---------------------------------" at (7, 7). Set Mode to "Anti Gravity". Print "     H = Hover Here        " at (0, 16). Set IdealThrottle to 0. Lock Throttle to IdealThrottle.
        } Else If Mode = "Altitude" {       Print "(< >) Hover at:                  " at (7, 7). Set Mode to "Radar". Print "     H = Hover Here        " at (0, 16). Set HoverAlt to RoundTens(TrueRadar).
        } Else If Mode = "Vertical Speed" { Print "(< >) Hover at:                  " at (7, 7). Set Mode to "Altitude". Print "     H = Hover Here        " at (0, 16). Set HoverAlt to RoundTens(TrueAltitude).
        } Else If Mode = "Anti Gravity" {   Print "(< >) Speed:                     " at (7, 7). Set Mode to "Vertical Speed". Print "     H = Kill V-Speed      " at (0, 16). Set TargetSpeed to MIN(MAX(Round(VerticalSpeed), -300), 300). Lock IdealThrottle to Throttle_PID:Update(Time:Seconds, VerticalSpeed). Lock Throttle to IdealThrottle.
        }
      }

      If Mode = "Radar" or Mode = "Altitude" {
        If ch = Terminal:Input:UpCursorOne { Set SBurning to False. If HoverAlt > 69900 { Set HoverAlt to 70000. } Else { Set HoverAlt to HoverAlt + 100. }}
        If ch = Terminal:Input:DownCursorOne { Set SBurning to False. If HoverAlt < 100 { Set HoverAlt to 0. } Else { Set HoverAlt to HoverAlt - 100. }}
        If ch = Terminal:Input:RightCursorOne { Set SBurning to False. If HoverAlt > 69990 { Set HoverAlt to 70000. } Else { Set HoverAlt to HoverAlt + 10. }}
        If ch = Terminal:Input:LeftCursorOne { Set SBurning to False. If HoverAlt < 10 { Set HoverAlt to 0. } Else { Set HoverAlt to HoverAlt - 10. }}
      } Else If Mode = "Vertical Speed" {
        If ch = Terminal:Input:UpCursorOne { Set SBurning to False. If TargetSpeed > 299 { Set TargetSpeed to 300. } Else { Set TargetSpeed to TargetSpeed + 1. }}
        If ch = Terminal:Input:DownCursorOne { Set SBurning to False. If TargetSpeed < -299 { Set TargetSpeed to -300. } Else { Set TargetSpeed to TargetSpeed - 1. }}
        If ch = Terminal:Input:RightCursorOne { Set SBurning to False. If TargetSpeed > 289 { Set TargetSpeed to 300. } Else { Set TargetSpeed to TargetSpeed + 10. }}
        If ch = Terminal:Input:LeftCursorOne { Set SBurning to False. If TargetSpeed < -289 { Set TargetSpeed to -300. } Else { Set TargetSpeed to TargetSpeed - 10. }}
      }

      If UnChar(ch) = 84 { If LimitTWR < 1.2 { Set LimitTWR to 1.1. } Else If LimitTWR = 100 { Set LimitTWR to 5. } Else { Set LimitTWR to LimitTWR - 0.1. }}
      If UnChar(ch) = 116 { If LimitTWR >= 5 { Set LimitTWR to 100. } Else { Set LimitTWR to LimitTWR + 0.1. }}

      If UnChar(ch) = 72 or UnChar(ch) = 104 {
        If Mode = "Radar" { Set HoverAlt to RoundTens(TrueRadar).
        } Else If Mode = "Altitude" { Set HoverAlt to RoundTens(TrueAltitude).
        } Else If Mode = "Anti Gravity" { Set HoverAlt to RoundTens(TrueRadar). Print "(< >) Hover at:                  " at (7, 7). Set Mode to "Radar". Print "     H = Hover Here        " at (0, 16).
        } Else If Mode = "Vertical Speed" { Set TargetSpeed to 0. }
      }
      
      If UnChar(ch) = 76 or UnChar(ch) = 108 { Set SBurning to False. Set HoverAlt to 0. Set Mode to "Radar". Set Throttle to 0. Print "(< >) Hover at:                  " at (7, 7). Set Mode to "Radar". Print "     H = Hover Here        " at (0, 16). }

      If UnChar(ch) = 75 or UnChar(ch) = 107 { If KillHSpeed = False { Set KillHSpeed to True. } Else { Set KillHSpeed to False. RCS_Kill("Release"). }}
      If UnChar(ch) = 81 or UnChar(ch) = 113 { Unlock Steering. Unlock Throttle. Set Started to False. }

      If UnChar(ch) = 82 or UnChar(ch) = 114 { If UseRCS = False { Set UseRCS to True. } Else { Set UseRCS to False. RCS_Kill("Release"). }}

      If Terminal:Input:HasChar Terminal:Input:Clear.
    }
  }
  
  If Started = True {

    If Mode = "Radar" {
      If TrueRadar > TargetAlt + 100 {
        If VerticalSpeed < -1 {
          If SBurning = False {
            Set BurnAlt to ABS(MaxVertDecel(TWR(LimitTWR) * AvailableThrust)) + 50.
            If TrueRadar < TargetAlt + BurnAlt + 100 { Set SBurning to True. Lock Throttle to IdealThrottle. } Else { Unlock Throttle. Set Throttle to 0. }
          }
          If TargetAlt < TrueRadar and TrueRadar < 5 {
            Set Throttle_PID to PIDLoop(0.3, 0.3, 0.005, 0, 1). Set IdealThrottle to Throttle_PID:Update(Time:Seconds, VerticalSpeed). Set Throttle_PID:SetPoint to -Min((TrueRadar / 2) / 2, 3).
          } Else {
            If SBurning = True Set IdealThrottle to Min(ABS(MaxVertDecel(TWR(LimitTWR) * AvailableThrust) / ((TrueRadar) - TargetAlt)), TWR(LimitTWR)).
          }
        } Else {
          Set IdealThrottle to 0.
        }
      } Else {
        Set SBurning to False. Set IdealThrottle to Throttle. Lock Throttle to IdealThrottle.
        If TargetAlt <= TrueRadar and TrueRadar < 2 {
          Set Throttle_PID to PIDLoop(0.3, 0.3, 0.005, 0, 1). Set IdealThrottle to Throttle_PID:Update(Time:Seconds, VerticalSpeed). Set Throttle_PID:SetPoint to -Min((TrueRadar / 2) / 2, 3).
        } Else {
          Local Maxa is 21. If MaxThrust > 0 and Mass > 0 Set Maxa to MaxThrust / Mass.
          Set IdealThrottle to Min(Seek(HoverHeight, TargetAlt - TrueRadar, Maxa, Correction, Body:MU / (TrueAltitude + Body:Radius)^2), TWR(LimitTWR)).
        }
      }
    }
    If Mode = "Altitude" {
      If TrueAltitude > TargetAlt + 100 {
        If VerticalSpeed < -1 {
          If SBurning = False {
            Set BurnAlt to ABS(MaxVertDecel(TWR(LimitTWR) * AvailableThrust)) + 50.
            If TrueAltitude < TargetAlt + BurnAlt + 100 { Set SBurning to True. Lock Throttle to IdealThrottle. } Else { Unlock Throttle. Set Throttle to 0. }
          }
          If TargetAlt <= TrueAltitude and TrueRadar < 5 {
            Set IdealThrottle to Throttle_PID:Update(Time:Seconds, VerticalSpeed). Set Throttle_PID:SetPoint to -Min((TrueRadar / 2) / 2, 3).
          } Else {
            If SBurning = True Set IdealThrottle to Min(ABS(MaxVertDecel(TWR(LimitTWR) * AvailableThrust) / ((TrueAltitude) - TargetAlt)), TWR(LimitTWR)).
          }
        } Else {
          Set IdealThrottle to 0.
        }
      } Else {
        Set SBurning to False. Set IdealThrottle to Throttle. Lock Throttle to IdealThrottle.
        If TargetAlt < TrueAltitude and TrueRadar < 5 {
          Set IdealThrottle to Throttle_PID:Update(Time:Seconds, VerticalSpeed). Set Throttle_PID:SetPoint to -Min((TrueRadar / 2) / 2, 3).
        } Else {
          Local Maxa is 21. If MaxThrust > 0 and Mass > 0 Set Maxa to MaxThrust / Mass.
          Set IdealThrottle to Min(Seek(HoverHeight, TargetAlt - TrueAltitude , Maxa, Correction, Body:MU / (TrueAltitude + Body:Radius)^2), TWR(LimitTWR)).
        }
      }
    }
    If Mode = "Vertical Speed" {
      Lock IdealThrottle to MIN(Throttle_PID:Update(Time:Seconds, VerticalSpeed), TWR(LimitTWR)). Set Throttle_PID:SetPoint to TargetSpeed.
    }
    If Mode = "Anti Gravity" {
      If AvailableThrust > 0 Set IdealThrottle to MIN(Body:MU * Mass / ((Altitude + Body:Radius)^2 * COS(VectorAngle(UP:ForeVector, Facing:ForeVector)) * AvailableThrust), TWR(LimitTWR)).
    }
  }

  If Steer = True and KillHSpeed = True and ShipStatus = "Flying" {
    If GroundSpeed < 0.1 {
      Lock Steering to LookDirUp(UP:ForeVector, Ship:Facing:TopVector). If UseRCS = True { RCS_Kill("Release"). }
    } Else {
      If GroundSpeed < 3 RCS Off.
      If AvailableThrust > 0 Set Steering to Heading(VectorHeading((Ship:Velocity:Surface - VDot(Ship:Velocity:Surface, UP:Vector) * UP:Vector) * -1), Min(Max(90 - GroundSpeed, ArcSin(Mass * (Body:MU / (Altitude + Body:Radius)^2) / (TWR(LimitTWR) * AvailableThrust)) -5), 89)).
      If AvailableThrust = 0 Lock Steering to LookDirUp(UP:ForeVector, Ship:Facing:TopVector).
      If UseRCS = True { If GroundSpeed < 3 { RCS_Kill("Release"). } Else { RCS_Kill("HSpeed"). }}
    }
  }

  If (ShipStatus = "Landed" or Ship:Status = "Splashed") { Set Landed to False.
    If Mode = "Radar" or Mode = "Altitude" { If HoverAlt <= TrueRadar + 1 Set Landed to True.
    } Else If Mode = "Vertical Speed" { If TargetSpeed < 1 Set Landed to True.
    } Else If Mode = "Anti Gravity" { }
    If Landed = True { Set SBurning to False. Set IdealThrottle to 0. If Steer = True Unlock Steering. } Else { If Steer = True and KillHSpeed = False Lock Steering to LookDirUp(UP:ForeVector, Ship:Facing:TopVector). } Wait 0.001.
  }
} Unlock Throttle. Unlock Steering. If Silenced = False ClearScreen.

// .=========================================================.
// |                        FUNCTIONS                        |
// |=========================================================|

// .================.
// | LOCAL FUNCTION |
// |================|

Function Seek { Parameter Deltas, Target, Authority, Aggro, Trim.
  Local DT is Time:Seconds - Deltas[0].
  Local Vel is 0. If DT <> 0 Set Vel to (Target - Deltas[1]) / DT.
  Set Deltas[0] to Time:Seconds. Set Deltas[1] to Target.
  Return ((Target + (Vel * ABS(Vel)) / (Authority / Aggro) + Vel) + Trim) / Authority.
}

Function Exit { Set Started to False. }

// .=====.
// | TWR |
// |=====|

Function TWR {
  Parameter TWRMode, Numeric is False, cAltitude is Altitude.	// "CUR" -> Current | "MAX" -> Maximum | [Number] -> Set TWR
  If TWRMode = "CUR" or TWRMode = "MAX" {
    Set mThrust to 0. Set cThrust to 0. List Engines in EngList.
    For Eng in EngList { If Eng:Name:Contains("decoupler") = False and Eng:Name:Contains("heatshield") = False {Set cThrust to cThrust + Eng:Thrust. If Eng:Ignition = True Set mThrust to mThrust + Eng:AvailableThrust. }}
    If Numeric = False {
      Set cThrust to Round(cThrust / ((Body:Mu / (Body:Radius + cAltitude)^2) * Mass), 2) + "". If cThrust:Contains(".") = True { If (cThrust + ""):Length - cThrust:Find(".") = 2 Set cThrust to cThrust + "0". } Else { Set cThrust to cThrust + ".00". }
      Set mThrust to Round(mThrust / ((Body:Mu / (Body:Radius + cAltitude)^2) * Mass), 2) + "". If mThrust:Contains(".") = True { If (mThrust + ""):Length - mThrust:Find(".") = 2 Set mThrust to mThrust + "0".  } Else { Set mThrust to mThrust + ".00". }
    } Else {
      Set cThrust to Round(cThrust / ((Body:Mu / (Body:Radius + cAltitude)^2) * Mass), 2).
      Set mThrust to Round(mThrust / ((Body:Mu / (Body:Radius + cAltitude)^2) * Mass), 2).
    }
    If TWRMode = "CUR" Return cThrust. If TWRMode = "MAX" Return mThrust.
  } Else { Return Min(Max((TWRMode * (Constant:G * ((Mass * Body:Mass) / ((cAltitude + Body:Radius)^2)))) / (AvailableThrust + 0.001), 0), 1). }
}

// .=================.
// | FORMAT DISTANCE |
// |=================|

Function FormatDistance {
  Parameter Value, Decimals is 0, Format is True.
  If Value < 1000 {
    Return Max(Round(Value), 0) + " m.".
  } Else If Value < 100000 {
    Local Result to Max(Round(Value / 1000, Decimals), 0) + "".
    If Format = True { If Decimals > 0 { If Result:Contains(".") = False { Return Result + "." + "0000000000":SubString(0, Decimals) + " km.". } Else { Return Result + " km.". }}}
    If Format = False { If Decimals > 0 { If Result:Contains(".") = False { Return Result + "." + "0000000000":SubString(0, Decimals) + "". } Else { Return Result + "". }}}
  } Else {
    If Format = True { Return Max(Round(Value / 1000), 0) + " km.". } Else { Return Max(Round(Value / 1000), 0) + "". } 
  }
}

// .=============.
// | SHIP HEIGHT |
// |=============|

Function GetShipHeight {
  Parameter cVessel is Ship.
  Set LowestPart to 0. Set HighestPart to 0. Lock R3 to cVessel:Facing:ForeVector. Set PartList to cVessel:Parts.
  For Part in PartList{ Set V to Part:Position. Set CurrentPart to R3:X * V:X + R3:Y * V:Y + R3:Z * V:Z. If CurrentPart > HighestPart Set HighestPart to CurrentPart. Else If CurrentPart < LowestPart Set LowestPart to CurrentPart. }
  Return HighestPart - LowestPart.
}

// .================.
// | VECTOR HEADING |
// |================|

Function VectorHeading { Parameter Input_Vector.
  Set UP_Vector to Ship:UP:Vector. Set North_Vector to Ship:North:Vector. Set East_Vector to VCRS(UP_Vector, North_Vector).
  Set East_Vel to vdot(Input_Vector, East_Vector). Set North_Vel to vdot(Input_Vector, North_Vector). Set Compass to ArcTan2(East_Vel, North_Vel).
  If Compass < 0 Set Compass to Compass + 360. Return Compass.
}

// .==========.
// | RCS Kill |
// |==========|

Function RCS_Kill { Parameter Mode is "HSpeed".
  Set cVector to Ship:Velocity:Surface * -1.
  If (Mode = "HSpeed" and GroundSpeed < 0.1) or (Mode = "VSPeed" and ABS(cVector:MAG) < 0.1) or Mode = "Release" Set cVector to V(0,0,0).
  If cVector:MAG > 1 Set cVector to cVector:Normalized.
  Set Ship:Control:Starboard to cVector * Ship:Facing:StarVector.
  Set Ship:Control:Top to cVector * Ship:Facing:TopVector.
  If Mode = "VSpeed" Set Ship:Control:Fore to cVector * Ship:Facing:ForeVector. //Up-Down
}

// .=========.
// | VARIOUS |
// |=========|

Function MaxVertDecel { Parameter LimitThrust is AvailableThrust. If Velocity:Surface:MAG = 0 or AvailableThrust = 0 { Return 0. } Else { Return VerticalSpeed^2 / (2 * ((LimitThrust / Mass) * (2 * (-VerticalSpeed / Velocity:Surface:MAG) + 1) / 3 - (Body:MU / Body:Radius^2))). }}
Function ShipStatus { If Ship:Status = "Landed" or Ship:Status = "PreLaunch" { Return "Landed". } Else If Ship:Status = "Splashed" { Return "Splashed". } Else { Return "Flying". }}
Function Spacer { Parameter StringLength, SpaceLength. Local Spaces is "                                                  ":Substring(0, Max(Min(SpaceLength, 50), 0)). If StringLength >= SpaceLength Return "". Return Spaces:Substring(StringLength, Spaces:Length - StringLength). }
Function RoundTens { Parameter Number. If Number < 10 { Return Round(Number / 10) * 10. } If Number < 100 { Return Round(Number / 100, 1) * 100. } If Number < 1000 { Return Round(Number / 1000, 2) * 1000. } If Number < 10000 { Return Round(Number / 10000, 3) * 10000. }}