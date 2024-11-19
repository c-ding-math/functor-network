{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserHome where

import Import
import Handler.Tree (getRootEntryId)

data Activity = Activity {
    activityTime :: UTCTime,
    activityTitle :: Text,
    activityText :: Text,
    activityLink :: Text
}   

getUserHomeR :: UserId -> Handler Html
getUserHomeR authorId = do
    isAuthor <- fmap (== Just authorId) maybeAuthId
    urlRender <- getUrlRender
    (author, statics, activities, mAbout)  <- runDB $ do
        author <- get404 authorId
        entries <- selectList [EntryUserId ==. authorId, EntryStatus ==. Publish, EntryType <-. [UserPost, Category, Comment]] [Desc EntryInserted]
        activities <- forM entries $ \(Entity entryId entry) -> case entryType entry of
            UserPost -> return $ Activity (entryInserted entry) "Published post" (entryTitleHtml entry) (urlRender (UserEntryR (entryUserId entry) entryId))
            Category -> return $ Activity (entryInserted entry) "Created category" (entryTitleHtml entry) (urlRender (CategoriesR (entryUserId entry)) <> ("#entry-" <> toPathPiece entryId))
            Comment -> do
                rootEntryId <- getRootEntryId entryId
                rootEntry <- get404 rootEntryId
                return $ Activity (entryInserted entry) "Commented on" (entryTitleHtml rootEntry) (urlRender (UserEntryR (entryUserId rootEntry) rootEntryId) <> ("#entry-" <> toPathPiece entryId))
            _-> return $ Activity (entryInserted entry) "" "" ""
        statics <- do
            let publishedPosts = length [x | x <- entries, entryType (entityVal x) == UserPost]
            let comments = length [x | x <- entries, entryType (entityVal x) == Comment]
            categorizedPosts <- do 
                let categories = [x | x <- entries, entryType (entityVal x) == Category]
                trees <- forM categories $ \category -> do
                    tree <- selectList [EntryTreeParent ==. entityKey category] []
                    filterM (\(Entity _ x) -> do
                        entry <- get404 $ entryTreeNode x
                        return $ entryStatus entry == Publish && entryType entry == UserPost
                        ) tree

                return $ length $ concat $ trees
            return (publishedPosts, categorizedPosts, comments)
        mAbout <- selectFirst [EntryUserId ==. authorId, EntryType ==. UserPage] [Desc EntryInserted]
        return (author, statics, take 100 activities, mAbout)
                

    defaultLayout $ do
        setTitle $ toHtml $ userName author
        [whamlet|
            <div .page-header>
            <section style="border:1px solid #ccc; margin-bottom:2.5em;">
                <div .card>
                    <div .basics>
                        $maybe avatar <- userAvatar author
                            <img.avatar.img-rounded src=#{avatar} alt=#{userName author}>
                        $nothing
                            <img.avatar.img-rounded src=@{StaticR $ StaticRoute ["icons","default-avatar.svg"] []} alt=#{userName author}>
                        <h4 .name>#{userName author}
                        $if isAuthor
                                <a .text-muted href=@{SettingsR}#profile-setting>
                                    <svg xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" fill="currentColor" class="bi bi-pencil-square" viewBox="0 0 16 16"><path d="M15.502 1.94a.5.5 0 0 1 0 .706L14.459 3.69l-2-2L13.502.646a.5.5 0 0 1 .707 0l1.293 1.293zm-1.75 2.456-2-2L4.939 9.21a.5.5 0 0 0-.121.196l-.805 2.414a.25.25 0 0 0 .316.316l2.414-.805a.5.5 0 0 0 .196-.12l6.813-6.814z"/><path fill-rule="evenodd" d="M1 13.5A1.5 1.5 0 0 0 2.5 15h11a1.5 1.5 0 0 0 1.5-1.5v-6a.5.5 0 0 0-1 0v6a.5.5 0 0 1-.5.5h-11a.5.5 0 0 1-.5-.5v-11a.5.5 0 0 1 .5-.5H9a.5.5 0 0 0 0-1H2.5A1.5 1.5 0 0 0 1 2.5z"/></svg><!-- Copyright (c) 2011-2024 The Bootstrap Authors, Licensed under MIT (https://github.com/twbs/bootstrap/blob/main/LICENSE) -->

                    <div .statistics>
                      $with (publishedPosts, categorizedPosts, comments) <- statics
                        <ul.list-inline.text-lowercase>
                            <li>
                                <a href=@{UserEntriesR authorId}>
                                    _{MsgPublishedPosts}
                                <span.badge>#{publishedPosts}
                            <li>
                                <a href=@{CategoriesR authorId}>
                                    _{MsgCategorizedPosts}
                                <span.badge>#{categorizedPosts}
                            <li>
                                <a href=@{CommentsR authorId}>
                                    _{MsgPostedComments}
                                <span.badge>#{comments}
                <article #about .entry style="margin-bottom:0">
                    <div .entry-content style="margin-bottom:0">
                        <div .entry-content-wrapper>
                            $maybe about <- mAbout
                                #{preEscapedToMarkup (entryBodyHtml (entityVal about))}
                            $nothing
                                <div style="width:519.3906239999999px;">
                                    <p>_{MsgNoAbout}
                            $if isAuthor
                                
                                    <a .text-muted href=@{EditUserAboutR}>_{MsgEdit}
            <section>    
                <div .activities>
                    <div .or>
                        <span.text-muted.text-lowercase> _{MsgRecentActivities}
                    <ul>
                        $forall activity <- activities
                            <li>
                                <time .text-muted.pull-right>#{utcToDate (activityTime activity)}
                                <span .toLower>#{activityTitle activity} 
                                <a href=#{activityLink activity}>#{preEscapedToMarkup (activityText activity)}
                        <li>
                            <time .text-muted.pull-right>#{utcToDate (userInserted author)}
                            <span .toLower>Registered an account
                
        |]
        toWidget [lucius|
.card {
    text-align: center;
    border-bottom: 1px solid #ccc;
    background-color: black;
    color: white;
}
.card .badge {
    background-color: #f5f5f5;
    color: black;
}
.avatar {
    height:128px;
    max-width:100%;
}
.basics {
    padding: 2em 2em 0.5em;
    position: relative;
}
.basics .name {
    font-weight: bold;
}
.basics a {
    color: #ccc;
    position: absolute;
    right: 1em;
    top: 1em;
}
.basics a:hover {
    color: white;
}
.statistics {
    min-width:max-content;
}
.statistics ul>li {
    padding: 0 0.5em;
}
.statistics a{
    color: #00a000;
}
.statistics a:hover{
    color: #008000;
}
.activities ul>li {
    padding: 0.5em 1.5em 0.5em 0;
}
.page-header {
    border:none;
}
.entry-content{
    padding: 1em 1em 0.5em;
}
        |]