module Ctrl

import dynamicalSystems
import Data.String

%access public export


--- Code by David I. Spivak and Anand Srinivasan
--- © 2020  


main : IO ()
main = pure ()

CtrlSignal : Type
CtrlSignal = Maybe Bool  --True = up , False = down

Temp : Type
Temp = Double

PlantBody : Arena
PlantBody = ArenaIO (CtrlSignal, Temp) Temp

CtrlBody : Arena
CtrlBody = ArenaIO Temp CtrlSignal

UserObservation : Arena
UserObservation = Motor $ IO Temp

Plant : Double -> Double -> DynSystem 
Plant conduct furnPow = MkDynSystem Double PlantBody PlantBehavior
      where
        PlantBehavior : Lens (Self Temp) PlantBody
        PlantBehavior = MkLens id int
          where
            mix : Double -> Double -> Double
            mix inside outside = (1 - conduct) * inside + conduct * outside
            int : (inside : Temp) -> (CtrlSignal, Temp) -> Temp
            int inside (Nothing, outside)    = mix inside outside
            int inside (Just True, outside)  = (mix inside outside) + furnPow
            int inside (Just False, outside) = (mix inside outside) - furnPow

Ctrl : Double -> Double -> DynSystem
Ctrl setpoint threshold = MkDynSystem CtrlSignal CtrlBody CtrlBehavior
      where
        CtrlBehavior : Lens (Self CtrlSignal) CtrlBody
        CtrlBehavior = MkLens id int
          where
            int : (c : CtrlSignal) -> Temp -> CtrlSignal
            int _ temp = if (temp - setpoint) < - threshold
                         then Just True
                         else if (temp - setpoint) > threshold
                            then Just False
                            else Nothing


Environment : DynSystem
Environment = IOMotor Temp (fromMaybe 100.0 . parseDouble)

{-
CtrlBody : Arena
CtrlBody = ArenaIO Temp CtrlSignal

PlantBody : Arena
PlantBody = ArenaIO (CtrlSignal, Temp) Temp

UserObservation : Arena
UserObservation = Motor $ IO Temp
-}
ThreeSystems : DynSystem
ThreeSystems = juxtapose [Environment, Ctrl 100.0 2.0, Plant 0.5 1.0]

ControlDiagram : Lens (body ThreeSystems) UserObservation
ControlDiagram = MkLens readout feed
      where
        readout : (IO Temp, CtrlSignal, Temp) -> IO Temp
        feed    : (p : (IO Temp, CtrlSignal, Temp)) -> () -> dis (body ThreeSystems) p
        readout (_, _, t) = pure t
        feed (envTemp, ctrlSig, plantTemp) () = X
          where
            X : ((), Temp, CtrlSignal, Temp)
            X = ?x

WholeSystem : DynSystem
WholeSystem = install ThreeSystems UserObservation ControlDiagram










