{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Shpadoinkle                 (Html, JSM, h, textProp)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Run             (live, runJSorWarp, simple)


view :: () -> Html m ()
view _ = viewCanvas
-- view _ = h "div" [] [ text "bar" ]

viewCanvas :: Html m ()
viewCanvas = canvas [id' "myCanvas", ("width",  textProp "800"), ("height", textProp "800"), ("style", textProp "border:1px solid #000000;")] []

app :: JSM ()
app = simple runParDiff () view getBody


main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Frontend running on https://localhost:" ++ show port ++ "\n"
    runJSorWarp port app
