--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (mplus, forM)
import           Data.Maybe (fromMaybe, fromJust, isJust, catMaybes)
import           Data.Monoid ((<>))
import           Hakyll
import           Data.List (intersperse, isPrefixOf)
import           Text.Regex (subRegex, mkRegex)
import           Data.List.Split

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


import Debug.Trace

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
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            item <- load (fromFilePath ("tags/" ++ tag ++ ".markdown"))
            title <- getMetadataField' (itemIdentifier item) "title"
            image <- getMetadataField' (itemIdentifier item) "image"

            let ctx = constField "title" title <>
                      constField "image" image <>
                      constField "tag" (itemBody item) <>
                      listField "posts" postCtx (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules countries $ \country pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            item <- load (fromFilePath ("countries/" ++ country ++ ".markdown"))
            title <- getMetadataField' (itemIdentifier item) "title"
            image <- getMetadataField' (itemIdentifier item) "image"

            let ctx = constField "title" title <>
                      constField "image" image <>
                      constField "country" (itemBody item) <>
                      listField "posts" postCtx (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/country.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules authors $ \author pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            item <- load (fromFilePath ("authors/" ++ author ++ ".markdown"))
            title <- getMetadataField' (itemIdentifier item) "title"
            face <- getMetadataField (itemIdentifier item) "face"

            let ctx = constField "title" title <>
                      boolField "ifFace" (\_ -> isJust face) <>
                      constField "face" (fromMaybe "" face) <>
                      constField "author" (itemBody item) <>
                      listField "posts" postCtx (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/author.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= applyFilter youtubeFilter
            >>= applyFilter imgFilter
            >>= loadAndApplyTemplate "templates/post.html"
                (postCtxWithTags authors countries tags)
            >>= loadAndApplyTemplate "templates/default.html"
                (postCtxWithTags authors countries tags)
            >>= relativizeUrls

    match "countries/*" $ do
        compile getResourceBody

    match "tags/*" $ do
        compile getResourceBody

    match "authors/*" $ version "full" $ do
        compile getResourceBody

    match "authors/*" $ version "teaser" $ do
        compile $ do
          body <- getResourceBody
          return (fmap stripBody body)

    match "authors/*" $ do
        compile $ do
          body <- getResourceBody
          return (fmap stripTeaser body)

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
    defaultContext


postCtxWithTags :: Tags -> Tags -> Tags -> Context String
postCtxWithTags authors countries tags =
    authorsField "authors" authors <>
    countriesField "countries" countries <>
    customTagsField "tags" tags <>
    postCtx


stripBody :: String -> String
stripBody s = fromMaybe "" (needlePrefix "<!--more-->" s)


stripTeaser :: String -> String
stripTeaser = unlines
              . dropWhile ((/=) "<!--more-->")
              . lines


customTagsField :: String -> Tags -> Context a
customTagsField =
  tagsFieldWith''' getTags customSimpleRenderTag mconcat


countriesField :: String -> Tags -> Context a
countriesField =
  tagsFieldWith'' getCountries simpleRenderCountry mconcat


authorsField :: String -> Tags -> Context a
authorsField =
  tagsFieldWith' getAuthors simpleRenderAuthor mconcat

--This whole thing could be a list field
tagsFieldWith' :: (Identifier -> Compiler [String])
              -> (String -> (Maybe String) -> String -> (Maybe FilePath) -> Maybe H.Html)
              -> ([H.Html] -> H.Html)
              -> String
              -> Tags
              -> Context a
tagsFieldWith' getTags' renderLink cat key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag

        author <- loadAuthorTeaser tag
        title <- getMetadataField' (itemIdentifier author) "title"
        face <- getMetadataField (itemIdentifier author) "face"

        return $ renderLink title face (itemBody author) route'

    return $ renderHtml $ cat $ catMaybes $ links

--This is a doublicate.... had to do this since im pulling out diffrent fields like face is image..
tagsFieldWith'' :: (Identifier -> Compiler [String])
              -> (String -> String -> (Maybe FilePath) -> Maybe H.Html)
              -> ([H.Html] -> H.Html)
              -> String
              -> Tags
              -> Context a
tagsFieldWith'' getTags' renderLink cat key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag

        country <- loadCountry tag
        title <- getMetadataField' (itemIdentifier country) "title"
        image <- getMetadataField' (itemIdentifier country) "image"

        return $ renderLink title image route'

    return $ renderHtml $ cat $ catMaybes $ links

--This is a doublicate.... had to do this since im pulling out diffrent fields like face is image..
tagsFieldWith''' :: (Identifier -> Compiler [String])
              -> (String -> String -> (Maybe FilePath) -> Maybe H.Html)
              -> ([H.Html] -> H.Html)
              -> String
              -> Tags
              -> Context a
tagsFieldWith''' getTags' renderLink cat key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag

        thisTag <- loadTag tag
        title <- getMetadataField' (itemIdentifier thisTag) "title"
        image <- getMetadataField' (itemIdentifier thisTag) "image"

        return $ renderLink title image route'

    return $ renderHtml $ cat $ catMaybes $ links


loadAuthorTeaser :: String -> Compiler (Item String)
loadAuthorTeaser tag = load $ setVersion (Just "teaser") $
    fromFilePath ("authors/" ++ tag ++ ".markdown")


loadCountry :: String -> Compiler (Item String)
loadCountry tag = load (fromFilePath ("countries/" ++ tag ++ ".markdown"))


loadTag :: String -> Compiler (Item String)
loadTag tag = load (fromFilePath ("tags/" ++ tag ++ ".markdown"))


simpleRenderAuthor :: String -> (Maybe String) -> String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderAuthor title (Just face) teaser (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "card" $ do
    H.img ! A.class_ "card-img-top" ! A.src (toValue face) ! A.alt (toValue title)
    H.div ! A.class_ "card-body" $ do
      H.h5 ! A.class_ "card-title" $ (toHtml title)
      renderAuthorTeaser teaser
simpleRenderAuthor title Nothing teaser (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "card" $ do
    H.div ! A.class_ "card-body" $ do
      H.h5 ! A.class_ "card-title" $ (toHtml title)
      renderAuthorTeaser teaser
simpleRenderAuthor _ _ _ _       = Nothing


renderAuthorTeaser :: String -> H.Html
renderAuthorTeaser s = case s of
    "" -> mempty
    teaser -> H.p ! A.class_ "card-text" $ toHtml teaser


simpleRenderCountry :: String -> String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderCountry title image (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "card d-flex text-center text-dark" $ do
    H.img ! A.class_ "card-img img-thumbnail" ! A.src (toValue image) ! A.alt (toValue title)
    H.div ! A.class_ "card-img-overlay align-items-center d-flex justify-content-center" $ do
      H.p ! A.class_ "card-text font-weight-bold display-4" $ toHtml title
simpleRenderCountry _ _ _         = Nothing


customSimpleRenderTag :: String -> String -> (Maybe FilePath) -> Maybe H.Html
customSimpleRenderTag title image (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "card d-flex text-center text-dark" $ do
    H.img ! A.class_ "card-img img-thumbnail" ! A.src (toValue image) ! A.alt (toValue title)
    H.div ! A.class_ "card-img-overlay align-items-center d-flex justify-content-center" $ do
      H.p ! A.class_ "card-text font-weight-bold display-4" $ toHtml title
customSimpleRenderTag _ _ _         = Nothing


applyFilter :: (Monad m, Functor f) => (String-> String) -> f String -> m (f String)
applyFilter transformator str = return $ (fmap $ transformator) str


imgFilter :: String -> String
imgFilter x = subRegex regex x result
  where
    regex = mkRegex "<p>\\{\\s1x3\\s(.+)\\s(.+)\\s(.+)\\s(.+)\\s\\}</p>"
    result = renderHtml $
      H.div ! A.class_ "card-group" $ do
        H.a ! A.href "\\1" ! A.class_ "card" ! A.style "border:none;flex:3;" $ do
          H.img ! A.class_ "card-img" ! A.src "\\1" ! A.style "height:100%;padding: 10px 10px 10px 10px;"
          H.div ! A.class_ "card-img-overlay" $ mempty
        H.div ! A.style "flex:1;display:flex;flex-direction:column;" $ do
          H.a ! A.href "\\2" ! A.class_ "card" ! A.style "border:none;flex:1;" $ do
            H.img ! A.class_ "card-img" ! A.src "\\2" ! A.style "height:100%;padding: 10px 10px 10px 10px;"
            H.div ! A.class_ "card-img-overlay" $ mempty
          H.a ! A.href "\\3" ! A.class_ "card" ! A.style "border:none;flex:1;" $ do
            H.img ! A.class_ "card-img" ! A.src "\\3" ! A.style "height:100%;padding: 10px 10px 10px 10px;"
            H.div ! A.class_ "card-img-overlay" $ mempty
          H.a ! A.href "\\4" ! A.class_ "card" ! A.style "border:none;flex:1;" $ do
            H.img ! A.class_ "card-img" ! A.src "\\4" ! A.style "height:100%;padding: 10px 10px 10px 10px;"
            H.div ! A.class_ "card-img-overlay" $ mempty


youtubeFilter :: String -> String
youtubeFilter x = subRegex regex x result
  where
    regex = mkRegex "<p>https?://www\\.youtube\\.com/watch\\?v=([A-Za-z0-9_-]+)</p>"
    result = renderHtml $
        H.div ! A.class_ "embed-responsive embed-responsive-16by9" $ do
          H.iframe ! A.class_ "embed-responsive-item" ! A.src "//www.youtube.com/embed/\\1" $ mempty


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
