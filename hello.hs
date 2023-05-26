main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml = makeHtml "My page title" (append_ (h1_ "Page header") (p_ "My page content"))

newtype Html = Html String

newtype Structure = Structure String

makeHtml :: String -> Structure -> Html
makeHtml title content = html_ (head_ (append_ (title_ title) (body_ content)))

html_ :: Structure -> Html
html_ (Structure a) = case el "html" a of
  Structure a -> Html a

body_ :: Structure -> Structure
body_ (Structure a) = el "body" a

head_ :: Structure -> Structure
head_ (Structure a) = el "head" a

title_ :: String -> Structure
title_ = el "title"

p_ :: String -> Structure
p_ = el "p"

h1_ :: String -> Structure
h1_ = el "h1"

append_ :: Structure -> Structure -> Structure
append_ (Structure lhs) (Structure rhs) = Structure (lhs <> rhs)

render :: Html -> String
render (Html a) = a

el :: String -> String -> Structure
el tag content = Structure ("<" <> tag <> ">" <> content <> "</" <> tag <> ">")