module Lifting where

import Mouse

preview : Bool -> Element
preview pos = container 340 60 middle
          <| color (case pos of 
                      True -> green
                      False -> red)
          <| container 250 50 middle
          <| asText pos

previewStream : Signal Bool -> Signal Element
previewStream xs = flow down <~ foldp (::) [] (preview <~ xs)

label : String -> Signal Element -> Signal Element
label s se = above <~ (constant (size 340 40 <| centered <| monospace <| toText s))
                    ~ se

main = beside <~ (label "Mouse.isDown =" (previewStream Mouse.isDown))
               ~ (label "lift not Mouse.isDown =" (previewStream (lift not Mouse.isDown)))
