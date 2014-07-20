module MousePositionStream where

preview : (Int, Int) -> Element
preview pos = container 500 60 middle
          <| color lightGray <| container 500 50 middle
               <| asText pos

previewStream : Signal Time -> Signal Element
previewStream xs = flow up <~ foldp (::) [] (preview <~ xs)

main = previewStream (every second)
