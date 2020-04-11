module Ctrl

import dynamicalSystems

%access public export


--- Code by David I. Spivak and Anand Srinivasan
--- © 2020  


main : IO ()
main = pure ()

CtrlSignal : Type
CtrlSignal = Maybe Bool  --True = up , False = down

Temp : Type
Temp = Double

PlantArena : Arena
PlantArena = IOArena (CtrlSignal, Temp) Temp

CtrlArena : Arena
CtrlArena = IOArena Temp CtrlSignal

Outer : Arena
Outer = IOArena Temp Temp

WD : Lens (PlantArena & CtrlArena) Outer
WD = MkLens readout go
      where
        readout : (Temp, CtrlSignal) -> Temp
        go      : (p : (Temp, CtrlSignal)) -> Temp -> (dis (PlantArena & CtrlArena) p)
        readout = fst
        go p t  = (?a, ?c) --((snd p, t), fst p)

PlantDynam : Double -> Double -> DynSystem 
PlantDynam conduct furnPow = MkDynSystem Double PlantArena PlantBehavior
      where
        PlantBehavior : Lens (Self Temp) PlantArena
        PlantBehavior = MkLens id int
          where
            mix : Double -> Double -> Double
            mix inside outside = (1 - conduct) * inside + conduct * outside
            int : (inside : Temp) -> (CtrlSignal, Temp) -> Temp
            int inside (Nothing, outside)    = mix inside outside
            int inside (Just True, outside)  = (mix inside outside) + furnPow
            int inside (Just False, outside) = (mix inside outside) - furnPow

CtrlDynam : Double -> Double -> DynSystem
CtrlDynam setpoint threshold = MkDynSystem CtrlSignal CtrlArena CtrlBehavior
      where
        CtrlBehavior : Lens (Self CtrlSignal) CtrlArena
        CtrlBehavior = MkLens id int
          where
            int : (c : CtrlSignal) -> Temp -> CtrlSignal
            int _ temp = if (temp - setpoint) < - threshold
                         then Just True
                         else if (temp - setpoint) > threshold
                            then Just False
                            else Nothing


WholeSystem : DynSystem
WholeSystem = install (myPlant &&& myCtrl) Outer WD
        where 
          myPlant : DynSystem
          myPlant = PlantDynam 0.5 1.0
          myCtrl  : DynSystem
          myCtrl  = CtrlDynam 100 2.0















