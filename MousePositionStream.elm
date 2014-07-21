module MousePositionStream where

import Mouse

preview : a -> Element
preview pos = container 500 60 middle
          <| color lightGray <| container 500 50 middle
               <| asText pos

previewStream : Signal a -> Signal Element
previewStream xs = flow down <~ foldp (::) [] (preview <~ xs)

main = previewStream Mouse.position
