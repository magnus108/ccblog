--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (mplus)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Hakyll
import           Data.List (intersperse)
import           Data.List.Split

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs" }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "contact.markdown", "404.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    countries <- buildTagsWith getCountries "posts/*" (fromCapture "countries/*.html")

    authors <- buildTagsWith getAuthors "posts/*" (fromCapture "authors/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" postCtx (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules countries $ \country pattern -> do
        let title = "Posts from \"" ++ country ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" postCtx (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/country.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules authors $ \author pattern -> do
        let title = "Posts by \"" ++ author ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" postCtx (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/author.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"
                (postCtxWithTags authors countries tags)
            >>= loadAndApplyTemplate "templates/default.html"
                (postCtxWithTags authors countries tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home" <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    --listFieldWith "authors" authorCtx postAuthors <>
    defaultContext


postCtxWithTags :: Tags -> Tags -> Tags -> Context String
postCtxWithTags authors countries tags =
    authorsField "authors" authors <>
    countriesField "countries" countries <>
    tagsField "tags" tags <>
    postCtx


countriesField :: String -> Tags -> Context a
countriesField =
  tagsFieldWith getCountries simpleRenderLink (mconcat . intersperse ", ")


authorsField :: String -> Tags -> Context a
authorsField =
  tagsFieldWith getAuthors simpleRenderLink (mconcat . intersperse ", ")


simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag


getCountries :: MonadMetadata m => Identifier -> m [String]
getCountries identifier = do
    metadata <- getMetadata identifier
    return $ fromMaybe [] $
        (lookupStringList "countries" metadata) `mplus`
        (map trim . splitAll "," <$> lookupString "countries" metadata)


getAuthors :: MonadMetadata m => Identifier -> m [String]
getAuthors identifier = do
    metadata <- getMetadata identifier
    return $ fromMaybe [] $
        (lookupStringList "authors" metadata) `mplus`
        (map trim . splitAll "," <$> lookupString "authors" metadata)


--authorCtx :: Context String
--authorCtx =
--  defaultContext


--postAuthors :: Item String -> Compiler [Item String]
--postAuthors item = do
--    let self = itemIdentifier item
--    authorPaths <-
--        getMetadataField' self "authors"
--    bios <-
--        mapM (\path -> load (fromFilePath path)) (splitOn "," authorPaths)
--    return bios
