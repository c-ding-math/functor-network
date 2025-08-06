{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Sitemap where

import Import
import qualified Data.Set as Set
import Yesod.Sitemap
import Data.Conduit (yield)

getSitemapR :: Handler TypedContent
getSitemapR = do
  (pages, featuredEntries, profiles) <- runDB $ do
    pages <- selectList [EntryType ==. Page, EntryStatus ==. Publish] [Asc EntryInserted]
    featuredEntries <- selectList [EntryType ==. UserPost, EntryStatus ==. Publish, EntryFeatured ==. True] [Desc EntryUpdated]
    profiles <- selectList [EntryType ==. UserPage, EntryStatus ==. Publish] [Desc EntryUpdated]
    return (pages, featuredEntries, profiles)
  let userIds = Set.toList . Set.fromList $ map (entryUserId . entityVal) featuredEntries

  sitemap $ do

    yield $ SitemapUrl {
        sitemapLoc = HomeR
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Nothing
        , sitemapPriority = Just 1.0
    }
    yield $ SitemapUrl {
        sitemapLoc = EntriesR
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Just Daily
        , sitemapPriority = Just 0.9
    }
    yield $ SitemapUrl {
        sitemapLoc = UsersR
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Just Daily
        , sitemapPriority = Just 0.9
    }
    yield $ SitemapUrl {
        sitemapLoc = FeedbackR
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Nothing
        , sitemapPriority = Just 0.5
    }
    forM_ pages $ \(Entity entryId entry) -> do
        yield $ SitemapUrl {
            sitemapLoc = PageR (entryTitle entry)
            , sitemapLastMod = Just $ entryUpdated entry
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.6
        }
    forM_ profiles $ \(Entity entryId entry) -> do
        yield $ SitemapUrl {
            sitemapLoc = UserHomeR (entryUserId entry)
            , sitemapLastMod = Just $ entryUpdated entry
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.8
        }
    forM_ userIds $ \uid -> do
        yield $ SitemapUrl {
            sitemapLoc = UserEntriesR uid
            , sitemapLastMod = Nothing
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.8
        }
    forM_ featuredEntries $ \(Entity entryId entry) -> do
        yield $ SitemapUrl {
            sitemapLoc = UserEntryR (entryUserId entry) entryId
            , sitemapLastMod = Just $ entryUpdated entry
            , sitemapChangeFreq = Nothing
            , sitemapPriority = Just 0.7
        }
    yield $ SitemapUrl {
        sitemapLoc = EditHelpR "syntax"
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Nothing
        , sitemapPriority = Just 0.6
    }
    yield $ SitemapUrl {
        sitemapLoc = EditHelpR "format"
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Nothing
        , sitemapPriority = Just 0.5
    }
    yield $ SitemapUrl {
        sitemapLoc = ToolR "tex-to-svg"
        , sitemapLastMod = Nothing
        , sitemapChangeFreq = Nothing
        , sitemapPriority = Just 0.5
    }

