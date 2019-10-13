{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Data.Default (Default(def))
import Data.Monoid (mappend)
import Hakyll hiding (pandocCompiler)
import Text.Pandoc.Options
import Text.Pandoc.Highlighting (haddock, styleToCss)

main :: IO ()
main = hakyllWith myconf $ do
  match "templates/*" $ compile templateCompiler

  match "css/*" $ do
    route   idRoute
    compile $ compressCssCompiler

  create ["css/highlight.css"] $ do
    route idRoute
    compile $ makeItem $ styleToCss highlightStyle

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title
                `mappend` listField "posts" postCtx (return posts)
                `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  let postCtxWithTags = tagsField "tags" tags <> postCtx

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtxWithTags
      >>= loadAndApplyTemplate "templates/default.html" postCtxWithTags
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Archives"            <>
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      tagField <- renderTagCloud 100 120 tags
      let indexCtx =
            constField "tags" tagField               <>
            listField "posts" postCtx (return posts) <>
            constField "title" "Home"                <>
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  create ["tags.html"] $ do
    route idRoute
    compile $ do
      tagField <- renderTagCloud 100 120 tags
      let ctx =
            constField "tags" tagField <>
            defaultContext

      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx <>
                constField "description" "This is the post description"

        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        renderAtom myFeedConfiguration feedCtx posts

myFeedConfiguration =
  FeedConfiguration
  { feedTitle = "k32 home page"
  , feedDescription = ""
  , feedAuthorName = "k32"
  , feedAuthorEmail = ""
  , feedRoot = "https://erlang.moe"
  }

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

myconf = def {destinationDirectory = "docs"}

pandocCompiler = Hakyll.pandocCompilerWith read write
  where
    read = def
    write = def { writerListings = True
                , writerHighlightStyle = Just highlightStyle
                , writerEmailObfuscation = JavascriptObfuscation
                }

highlightStyle = haddock
