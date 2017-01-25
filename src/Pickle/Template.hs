{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pickle.Template
  ( myBlogPost
  , myPage
  , myIndexPage
  , assemble
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

import Pickle.Types
import Pickle.Pandoc

--------------------------------------------------------------------------------

attrs = mconcat

cssImport link = string ("@import " <> show link <> ";")

script0 = script ""

maybeRender f = fromMaybe mempty . fmap f

nbsp = preEscapedString "&nbsp;"

mathJax  = "MathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]} });"
googAnalytics = "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', 'UA-22479172-1', 'auto');ga('send', 'pageview');"

--------------------------------------------------------------------------------

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
      h <> nbsp <> sub
--      string h <> small (string sub)
  div ! A.style "clear:both" $ ""

myBlogHeader = do
  header $ do
    h1 ! class_ "name pull-left" $ do
      string "Matt Chan" <> nbsp <> small (a ! href "/blog" $ string "Blog")
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

-- | Render the whole html file, including all the extra crap
assemble pg = docTypeHtml (myHead >> body (myBody0 pg))

--------------------------------------------------------------------------------

myIndexPage :: Html -> Html -> Html -> Html
myIndexPage h sub content = do
  myHeader h sub
  div ! class_ "content" $ do
    content

myPage :: Html -> Html -> Html -> Html
myPage h sub content = do
  myHeader h sub
  div ! class_ "content" $ do
    content
  br; a ! href "../" $ preEscapedString "&larr; back";
  br;br;br

myBlogPost :: Post -> Html
myBlogPost post@Post{..} =
  let metaHtml k f = maybeRender (f . pandocUnmeta)
                   $ lookupMeta k (pandocMeta postContent)
  in do
  myBlogHeader
  div ! class_ "content" $ do
    div ! class_ "post" $ do
      metaHtml "title" (h1.string)
      metaHtml "subtitle" (\s -> i (string s) <> br <> br)
      p ! A.style "color:gray;" $ do
        metaHtml "date" string
        metaHtml "updated" (\s -> string $ "(updated " <> s <> ")")
      renderPandoc postContent
  -- TODO footer
