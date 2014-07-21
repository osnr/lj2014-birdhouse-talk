module CounterSecondGraph where

import Graphics.Input as Input
import Graphics.Collage (..)
import Text

nodeHeight = 50

stepOne : Signal Time
stepOne = every <| second
stepOneG = node 120 nodeHeight "Time" <~ stepOne
posOne = (0, 150)

stepTwo : Signal Int
stepTwo = count stepOne
stepTwoG = node 50 nodeHeight "Int" <~ stepTwo
posTwo = (0, 50)

stepThree : Signal String
stepThree = show <~ stepTwo
stepThreeG = node 70 nodeHeight "String" <~ stepThree
posThree = (0, -50)

type StatusUpdate = { status : String }
stepFour : Signal StatusUpdate
stepFour = (\text -> { status = text }) <~ stepThree
stepFourG = node 160 nodeHeight "BH.StatusUpdate {}" <~ stepFour
posFour = (0, -150)

sansSerifFonts = typeface ["helvetica","arial","sans-serif"]
monospaceFonts = typeface ["Menlo","Monaco","Consolas","Courier New","monospace"]

node : Int -> Int -> String -> a -> Element
node w h t x = 
  color black . container (w+2) (h+2) middle . 
  color white . container w h middle <|
    flow down [toText (show x) |> monospaceFonts |> Text.height 13 |> centered,
               toText (": " ++ t) |> sansSerifFonts |> Text.height 12 |> centered]

arrow : (Float, Float) -> (Float, Float) -> String -> String -> Form
arrow (x1, y1) (x2, y2) s t = group
  [segment (x1, y1 - nodeHeight / 2)
    (x2, y2 + nodeHeight / 2) |> traced (solid darkGrey),
   ngon 3 8 |> filled darkGrey |> rotate (degrees 30)
     |> move (x2, y2 + nodeHeight / 2 + 8),
   label ((x1 + x2) / 2, (y1 + y2) / 2 + 5) s t]

label : (Float, Float) -> String -> String -> Form
label (x, y) s t =
  let lbl = toText s |> monospaceFonts |> Text.height 13 |> centered
      tLbl = toText (": " ++ t) |> sansSerifFonts |> Text.height 12 |> centered in group
  [toForm lbl |> move (x, y),
   toForm tLbl |>
     move (x + 6 +
       0.5 * (toFloat (widthOf lbl) +
              toFloat (widthOf tLbl)),
       y)]

graph : Element -> Element -> Element -> Element -> Element
graph one two three four = collage 800 450
  [toForm one |> move posOne,
   toForm two |> move posTwo,
   toForm three |> move posThree,
   toForm four |> move posFour,
   label (0, 190) "every second" "Signal Time",
   arrow posOne posTwo "count" "Signal Time -> Signal Int",
   arrow posTwo posThree "show" "Int -> String (lifted to Signal Int -> Signal String)",
   arrow posThree posFour "BH.update" "String -> BH.StatusUpdate {}\n(lifted to Signal String -> Signal (BH.StatusUpdate {})",
   arrow posFour (0, -250) "" "port to Birdhouse/Twitter"]

main = graph <~ stepOneG ~ stepTwoG ~ stepThreeG ~ stepFourG
