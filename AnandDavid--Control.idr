module Ctrl

import dynamicalSystems
import Data.String

%access public export


--- Code by David I. Spivak and Anand Srinivasan
--- © 2020  


main : IO ()
main = pure ()

CtrlSignal : Type
CtrlSignal = IO (Maybe Bool)  --True = up , False = down

Temp : Type
Temp = IO Double

PlantBody : Arena
PlantBody = ArenaIO (CtrlSignal, Temp) Temp

CtrlBody : Arena
CtrlBody = ArenaIO Temp CtrlSignal

UserObservation : Arena
UserObservation = Emitter Temp

Plant : Double -> Double -> DynSystem 
Plant conduct furnPow = MkDynSystem Temp PlantBody PlantBehavior
      where
        PlantBehavior : Lens (Self Temp) PlantBody
        PlantBehavior = MkLens id int
          where
            mix : Double -> Double -> Double
            mix inside outside = (1 - conduct) * inside + conduct * outside
            int : (inside : Temp) -> (CtrlSignal, Temp) -> Temp
            int inside (ctrl, outside) = do
              sig <- ctrl
              case sig of 
                Nothing => mix <$> inside <*> outside
                (Just True) => map (furnPow +) (mix <$> inside <*> outside)
                (Just False) => map (furnPow -) (mix <$> inside <*> outside)

Ctrl : Double -> Double -> DynSystem
Ctrl setpoint threshold = MkDynSystem CtrlSignal CtrlBody CtrlBehavior
      where
        CtrlBehavior : Lens (Self CtrlSignal) CtrlBody
        CtrlBehavior = MkLens id int
          where
            int : (c : CtrlSignal) -> Temp -> CtrlSignal
            int _ temp = do
              measured <- temp
              if (measured - setpoint) < - threshold
                then pure $ Just True
                else if (measured - setpoint) > threshold
                  then pure $ Just False
                  else pure Nothing


Environment : DynSystem
Environment = MkDynSystem Temp (Emitter Temp) passUserInput
          where 
            passUserInput : Lens (Self Temp) (Emitter Temp)
            passUserInput = MkLens id listen
            where
              listen : (old : Temp) -> () -> Temp
              listen _ _ = map (fromMaybe 100.0 . parseDouble) getLine

{-
CtrlBody : Arena
CtrlBody = ArenaIO Temp CtrlSignal

PlantBody : Arena
PlantBody = ArenaIO (CtrlSignal, Temp) Temp

UserObservation : Arena
UserObservation = Emitter Temp
-}
ThreeSystems : DynSystem
ThreeSystems = juxtapose [Environment, Ctrl 100.0 2.0, Plant 0.5 1.0]

ControlDiagram : Lens (body ThreeSystems) UserObservation
ControlDiagram = MkLens readout feed
      where
        readout : (Temp, CtrlSignal, Temp) -> Temp
        feed    : (p : (Temp, CtrlSignal, Temp)) -> () -> dis (body ThreeSystems) p
        readout (_, _, t) = t
        feed (envTemp, ctrlSig, plantTemp) () = X
          where
            X : ((), Temp, CtrlSignal, Temp)
            X = ((), plantTemp, ctrlSig, envTemp)

WholeSystem : DynSystem
WholeSystem = install ThreeSystems UserObservation ControlDiagram










