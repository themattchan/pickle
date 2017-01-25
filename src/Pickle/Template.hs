{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pickle.Template
  ( myHeader
  ) where

import qualified Data.Text.Lazy as T

import Prelude hiding (head, id, div)
import Data.Monoid
import Data.Maybe

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, style, form)
import qualified Text.Blaze.Html5.Attributes as A
-- import qualified Text.Blaze.Html.Renderer.Text as B (renderHtml)

attrs = mconcat

cssImport link = string ("@import " <> show link <> ";")

script0 = script ""

mathJax  = "MathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]} });"
googAnalytics = "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', 'UA-22479172-1', 'auto');ga('send', 'pageview');"

myHead :: Html
myHead = head $ do
  title "my page title"
  link ! attrs [rel "icon", href "/favicon.ico", type_ "image/x-icon"]

  meta ! attrs [name "keywords", content "Matt Chan, Matthew Chan"]
  meta ! attrs [name "author"  , content "Matt Chan"]
  meta ! attrs [name "viewport", content "width=device-width, initial-scale=1.0"]

  meta ! attrs [httpEquiv "content-type"    , content "text/html; charset=utf-8"]
  meta ! attrs [httpEquiv "content-language", content "en-us"]
  meta ! attrs [httpEquiv "X-UA-Compatible" , content "IE=edge,chrome=1"]

  style ! attrs [type_ "text/css", media "screen", A.title "matt"]
    $ foldMap cssImport
    [ "//maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"
    , "/assets/matt.css"
    , "/assets/syntax.css"
    , "/assets/toc.css"
    ]

  script0 ! src "//code.jquery.com/jquery-latest.min.js"
  script0 ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js"
  script0 ! src "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  script0 ! src "/assets/matt.js"

  script ! type_ "text/x-mathjax-config" $ preEscapedString mathJax
  script $ preEscapedString googAnalytics

myHeader h sub = do
  header $ do
    h1 ! class_ "name pull-left" $ do
      string h <> small (string sub)
  div ! A.style "clear:both" $ ""

myBlogHeader = do
  header $ do
    h1 ! class_ "name pull-left" $ do
      string "Matt Chan" <> small (a ! href "/blog" $ string "Blog")
    h1 ! class_ "pull-right" $ do
      form ! attrs [ id "search"
                   , class_ "navbar-search"
                   , method "get"
                   , action "http://google.com/search"
                   ] $
        (do { p (do { input ! attrs [ type_ "hidden", name "q", value "site:themattchan.com"]
                     ; input ! attrs [ type_ "text", class_ "form-control", name "q", placeholder "search"]
                     })
            })
  div ! A.style "clear:both" $ ""

myBody0 content = body $ do
  div ! id "wrap" $ do
    div ! class_ "container" $ do
      content

myPage h sub content = do
  myHeader h sub
  myBody0 $ do
    div ! class_ "content" $ do
      content
    br; a ! href "../" $ preEscapedString "&larr; back";
    br;br;br

myBlogPost :: String -> Maybe String -> Html -> Html
myBlogPost blogTitle blogSubtitle date content = do
  myBlogHeader
  myBody0 $ do
    div ! class_ "content" $ do
      div ! class_ "post" $ do
        h1 (string blogTitle)
        fromMaybe mempty ((\s -> i (string s) <> br <> br) <$> blogSubtitle)
        p ! style "color:gray;" $ ""
-- TODO
