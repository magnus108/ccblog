--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (mplus, forM)
import           Data.Maybe (fromMaybe, fromJust, isJust, catMaybes)
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
            >>= loadAndApplyTemplate "templates/post.html"
                (postCtxWithTags authors countries tags)
            >>= loadAndApplyTemplate "templates/default.html"
                (postCtxWithTags authors countries tags)
            >>= relativizeUrls

    match "authors/*" $ do
        compile $ pandocCompiler

    match "tags/*" $ do
        compile $ pandocCompiler

    match "countries/*" $ do
        compile $ pandocCompiler

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
              -> (String -> (Maybe String) -> (Maybe FilePath) -> Maybe H.Html)
              -> ([H.Html] -> H.Html)
              -> String
              -> Tags
              -> Context a
tagsFieldWith' getTags' renderLink cat key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag

        author <- loadAuthor tag
        title <- getMetadataField' (itemIdentifier author) "title"
        face <- getMetadataField (itemIdentifier author) "face"

        return $ renderLink title face route'

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


loadAuthor :: String -> Compiler (Item String)
loadAuthor tag = load (fromFilePath ("authors/" ++ tag ++ ".markdown"))


loadCountry :: String -> Compiler (Item String)
loadCountry tag = load (fromFilePath ("countries/" ++ tag ++ ".markdown"))


loadTag :: String -> Compiler (Item String)
loadTag tag = load (fromFilePath ("tags/" ++ tag ++ ".markdown"))


simpleRenderAuthor :: String -> (Maybe String) -> (Maybe FilePath) -> Maybe H.Html
simpleRenderAuthor title (Just face) (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "card" $ do
    H.img ! A.class_ "card-img-top" ! A.src (toValue face) ! A.alt (toValue title)
    H.div ! A.class_ "card-body" $ do
      H.p ! A.class_ "card-text" $ toHtml title
simpleRenderAuthor title Nothing (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "card" $ do
    H.div ! A.class_ "card-body" $ do
      H.p ! A.class_ "card-text" $ toHtml title
simpleRenderAuthor _ _ _         = Nothing


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
