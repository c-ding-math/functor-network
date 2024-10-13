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
                trees <- forM categories $ \category -> selectList [EntryTreeParent ==. entityKey category] []
                return $ length $ concat $ trees
            return (publishedPosts, categorizedPosts, comments)
        mAbout <- selectFirst [EntryUserId ==. authorId, EntryType ==. UserPage] [Desc EntryInserted]
        return (author, statics, take 100 activities, mAbout)
                

    defaultLayout $ do
        setTitle $ toHtml $ userName author
        [whamlet|
            <div .page-header>
            <section style="border:1px solid #ccc; border-radius:5px; margin-bottom:2.5em;">
                <div .card>
                    <div .basics>
                        $maybe avatar <- userAvatar author
                            <img.avatar.img-rounded src=#{avatar} alt=#{userName author}>
                        $nothing
                            <img.avatar.img-rounded src=@{StaticR $ StaticRoute ["icons","default-avatar.svg"] []} alt=#{userName author}>
                        <h4 .name>#{userName author}
                        $if isAuthor
                            <div>
                                <a .text-muted href=@{SettingsR}#profile-setting>_{MsgSettings}

                    <div .statistics>
                      $with (publishedPosts, categorizedPosts, comments) <- statics
                        <ul.list-inline>
                            <li>
                                <a.text-muted href=@{UserEntriesR authorId}>
                                    _{MsgPublishedPosts}
                                <span>#{publishedPosts}
                            <li>
                                <a.text-muted href=@{CategoriesR authorId}>
                                    _{MsgCategorizedPosts}
                                <span>#{categorizedPosts}
                            <li>
                                <a.text-muted href=@{CommentsR authorId}>
                                    _{MsgPostedComments}
                                <span>#{comments}
                <article #about .entry style="margin-bottom:0">
                    <div .entry-content style="margin-bottom:0">
                        <div .entry-content-wrapper>
                            $maybe about <- mAbout
                                #{preEscapedToMarkup (entryBodyHtml (entityVal about))}
                            $nothing
                                <p>_{MsgNoAbout}
                            $if isAuthor
                                <div>
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
}
.avatar {
    height:128px;
    max-width:100%;
}
.basics {
    padding: 2em 2em 0.5em;
}
.basics .name {
    font-weight: bold;
}
.statistics ul>li {
    padding: 0 0.5em;
}
.statistics ul>li>span{
    font-size: 1.5em;
    font-weight: bold;
    font-family: Georgia, "Times New Roman", Times, serif;
    margin-left: 0.1em;
}
.activities ul>li {
    padding: 0.5em 1.5em 0.5em 0;
}

        |]