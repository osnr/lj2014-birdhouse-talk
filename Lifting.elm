module Lifting where

import Mouse

preview : a -> Element
preview pos = container 250 60 middle
          <| color lightGray <| container 250 50 middle
               <| asText pos

previewStream : Signal Bool -> Signal Element
previewStream xs = flow down <~ foldp (::) [] (preview <~ xs)

label : String -> Signal Element -> Signal Element
label s se = above <~ (constant (width 290 <| plainText s)) ~ se

main = beside <~ (label "Mouse.isDown =" (previewStream Mouse.isDown))
               ~ (label "lift not Mouse.isDown =" (previewStream (lift not Mouse.isDown)))
