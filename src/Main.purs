module Main where


import Effect (Effect)
import Effect.Console (log)

import Oak

import Data.Tuple (Tuple(..))

import Prelude hiding (div)

data M1 = Counter {number1 :: Int, number2 :: Int}
data Msg1 = Inc | Dec | Square

instance defaultM1 :: Default M1 where
  default = Counter {number1: 0, number2: 0}

instance viewM1 :: View M1 (Either Msg1 Msg1) where
  view (Counter m) = div [] do
     div [] $ mapMsg Left v1
     div [] $ mapMsg Right v2
    where
    v1 = div [] do
      button [onClick Inc] do
        text "+"
      div [] do
        text m.number1
      button [onClick Dec] do
        text "-"
      button [onClick Square] do
        text "^2"
    v2 = div [] do
      button [onClick Inc] do
        text "+"
      div [] do
        text m.number2
      button [onClick Dec] do
        text "-"
      button [onClick Square] do
        text "^2"

{-- next1 :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit --}
{-- next1 msg mod h = mempty --}

{-- instance transferM1 :: Transfer Msg1 M1 M1 where --}
{--   transfer msg (Counter m) = case msg of --}
{--     Inc -> --} 
{--       Counter { number: m.number + 1 } --}
{--     Dec -> --} 
{--       Counter { number: m.number - 1 } --}
{--     Square -> --} 
{--       Counter { number: m.number * m.number} --}
    

instance transferM11 :: Transfer (Either Msg1 Msg1) M1 M1 where
  transfer msg (Counter m) = case msg of
    Left ms -> case ms of
      Inc -> 
        Counter { number1: m.number1 + 1, number2: m.number2 }
      Dec -> 
        Counter { number1: m.number1 - 1 , number2: m.number2}
      Square -> 
        Counter { number1: m.number1 * m.number1, number2: m.number2}
    Right ms -> case ms of
      Inc -> 
        Counter { number2: m.number2 + 1, number1: m.number1 }
      Dec -> 
        Counter { number2: m.number2 - 1, number1: m.number1 }
      Square -> 
        Counter { number2: m.number2 * m.number2, number1: m.number1}

main :: Effect Unit
main = do
  rootNode <- runApp (app_ :: App (Either Msg1 Msg1) M1) Nothing
  container <- getElementById "app"
  appendChildNode container rootNode

class Default a where
  default :: a

class Transfer msg m1 m2 where
  transfer :: msg -> m1 -> m2

class View m msg where
  view :: m -> View msg

class Next m msg where
  next :: msg -> m -> (msg -> Effect Unit) -> Effect Unit


app 
  :: forall m msg 
   . Default m 
  => Transfer msg m m
  => View m msg 
  => Next m msg 
  => App msg m
app = createApp 
  { init: default
  , view: view
  , update: transfer
  , next: next
  }

app_
  :: forall m msg
   . Default m 
  => Transfer msg m m  
  => View m msg 
  => App msg m
app_ = createApp 
  { init: default
  , view: view
  , update: transfer
  , next: const $ const $ const mempty
  }

instance productDefault 
  :: (Default a, Default b) 
  => Default (Tuple a b) 
  where
  default = Tuple default default

{-- instance productView --}
{--   :: (View m1 msg1, View m2 msg2) --}
{--   => View (Tuple m1 m2) (Tuple msg1 msg2) --}
