{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserHome where

import Import

getUserHomeR :: UserId -> Handler Html
getUserHomeR authorId = do
    mUserId <- maybeAuthId
    (author, mAbout) <- runDB $ do
        author <- get404 authorId
        mAbout <- selectFirst [EntryUserId ==. authorId, EntryType ==. UserPage] [Desc EntryInserted]
        return (author, mAbout)
    defaultLayout $ do
        setTitle $ toHtml $ userName author
        [whamlet|

            <div .page-header>
            <div.user>
                $maybe avatar <- userAvatar author
                    <img.img-rounded src=#{avatar} alt=#{userName author}>
                $nothing
                    <img.img-rounded src=@{StaticR $ StaticRoute ["icons","default-avatar.svg"] []} alt=#{userName author}>
                <h1 .user-name.entry-title>#{userName author}
                <div .user-id>No. #{toPathPiece authorId}
                $if mUserId == Just authorId
                    $if isJust mAbout
                        <a.edit-profile.pull-right.btn.btn-default href=@{EditUserAboutR}>_{MsgEdit}
                    $else
                        <a.edit-profile.pull-right.btn.btn-primary href=@{EditUserAboutR}>_{MsgEdit}

            <article #about .entry style="margin-bottom:0">
                <div .entry-content style="margin-bottom:0">
                    <div .entry-content-wrapper>
                        $maybe about <- mAbout
                            $if isJust (entryBody (entityVal about))
                                #{preEscapedToMarkup (entryBodyHtml (entityVal about))}
                            $else
                                <div style="width:519.3906239999999px;">
                                    <p>_{MsgNoAbout}
                        $nothing
                            <div style="width:519.3906239999999px;">
                                <p>_{MsgNoAbout}
        |]
        toWidget [lucius|
            .page-header {
                border:none;
            }

            .user {
                display:grid;
                align-items: center;
                column-gap: 1em;
                grid-template-columns: 128px auto auto;
                margin-bottom: 1em;
            }
            .user img {
                grid-row: 1/3;
                width: 128px;
            }
            .user-name {
                grid-column: 2/4;
            }
            .user-id {
                color: #777;
            }
            .edit-profile {
                justify-self: right;
            }
        |]
