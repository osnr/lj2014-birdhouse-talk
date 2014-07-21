module Count where

import Mouse

preview : a -> Element
preview pos = container 300 60 middle
          <| color lightGray <| container 300 50 middle
               <| asText pos

previewStream : Signal a -> Signal Element
previewStream xs = flow down <~ foldp (::) [] (preview <~ xs)

label : String -> Signal Element -> Signal Element
label s se = above <~ (constant (width 320 <| plainText s)) ~ se

main = beside <~ (label "every second =" (previewStream <| every second))
               ~ (label "count (every second) =" (previewStream <| count (every second)))
