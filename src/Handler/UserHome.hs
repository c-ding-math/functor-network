{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.UserHome where

import Import
import Data.Time
--import Data.Time.Calendar (addGregorianMonthsClip)
import Handler.Tree(getRootEntryId)
import Handler.NewUserSubscription (subscribeToUserWidget)
import Handler.UserEntry(shareWidget)

getUserHomeR :: UserId -> Handler Html
getUserHomeR authorId = do
    mUserId <- maybeAuthId
    (author, mAbout, postChart, commentChart) <- runDB $ do
        author <- get404 authorId
        mAbout <- selectFirst [EntryUserId ==. authorId, EntryType ==. UserPage] [Desc EntryInserted]
        --Chart data
        -- chart x axis is month, y axis is number of posts
        currentTime <- liftIO getCurrentTime
        let firstDayNextMonth = firstDayNextMonthUTC currentTime
        let chartX = [formatTime defaultTimeLocale "%b" (addMonth n currentTime) | n <- [-11..0]]
        chartYPost <- mapM (\n -> do
                let time = addMonth n firstDayNextMonth
                y <- count [EntryUserId ==. authorId, EntryType ==. UserPost, EntryStatus ==. Publish, EntryInserted <=. time]
                return y) [-11..0]
        chartYComment <- mapM (\n -> do
                let time = addMonth n firstDayNextMonth
                comments <- selectList [EntryUserId ==. authorId, EntryType ==. Comment, EntryStatus ==. Publish, EntryInserted <=. time] []
                comments' <- filterM (\(Entity commentId _) -> do
                    rootEntryId <- getRootEntryId commentId
                    e <- get404 rootEntryId
                    return $ entryStatus e == Publish
                    ) comments
                return (length comments')) [-11..0]
        let postChartData = zip chartX chartYPost
        let commentChartData = zip chartX chartYComment
        
        return (author, mAbout, postChartData, commentChartData)
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
                <div .user-actions>
                    $if (mUserId == Just authorId) && (isJust mAbout)
                            <a#share-link.btn.btn-primary href=# data-link=@{UserHomeR authorId}>_{MsgShare}
                    $else
                            <a#share-link.btn.btn-default href=# data-link=@{UserHomeR authorId}>_{MsgShare}
                    ^{subscribeToUserWidget authorId}
                    $if mUserId == Just authorId
                        $if isJust mAbout
                            <a.edit-profile.btn.btn-default href=@{EditUserAboutR}>_{MsgEdit}
                        $else
                            <a.edit-profile.btn.btn-primary href=@{EditUserAboutR}>_{MsgEdit}

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
            <div .or>
                <span.text-muted.text-lowercase> _{MsgStatistics}
            <div.charts>
                <a.stretched-link href=@{UserEntriesR authorId}">
                    <div .panel.panel-default>
                        <div .panel-body>
                            <h4>_{MsgPosts}
                            <div id="post-chart">
                                <canvas id="postsLineChart"></canvas>     
                <a.stretched-link href=@{CommentsR authorId}>
                    <div .panel.panel-default>
                        <div .panel-body>
                            <h4>_{MsgComments}
                            <div id="comment-chart">
                                <canvas id="commentsLineChart"></canvas>
                                   
        |]
        toWidgetHead [hamlet|<link rel="alternate" type="application/rss+xml" href=@{UserFeedR authorId} title=#{userName author}>|]
        shareWidget
        addScript $ StaticR js_chart_min_js
        toWidget [julius|
    const chartOptions = {
          responsive: true,
          maintainAspectRatio: true,
          animation: false,
          plugins: {
            legend: {
                display: false
                }
          },

        scales: {
          y: {
            beginAtZero: true,
            title: {
              display: true,
              text: 'total',
            },
            ticks: {
                stepSize: 1
            },
            grid: {
                display: false
            }
          },
          x: {
            title: {
              display: true,
              text: 'month',
            },
            grid: {
                display: false
            }
          }
        }
      }
    const postLabels = #{toJSON (map fst postChart)};
    const postData = #{toJSON (map snd postChart)};
    const ctxPost = document.getElementById('postsLineChart').getContext('2d');
    const postsLineChart = new Chart(ctxPost, {
      type: 'line',
      data: {
        labels: postLabels,
        datasets: [{
          //label: 'total posts',
          data: postData,
          pointBackgroundColor: '#333',
          pointBorderColor: '#333',
          borderColor: '#333',
          borderWidth: 1,
          //backgroundColor: 'rgba(75, 192, 192, 0.2)',
          //tension: 0.3, 
          //pointRadius: 5,
          //fill: true
        }]
      },
      options: chartOptions
    });
    const commentLabels = #{toJSON (map fst commentChart)};
    const commentData = #{toJSON (map snd commentChart)};
    const ctxComment = document.getElementById('commentsLineChart').getContext('2d');
    const commentsLineChart = new Chart(ctxComment, {
      type: 'line',
      data: {
        labels: commentLabels,
        datasets: [{
          //label: 'total comments',
          data: commentData,
          pointBackgroundColor: '#333',
          pointBorderColor: '#333',
          borderColor: '#333',
          borderWidth: 1,
          //backgroundColor: 'rgba(75, 192, 192, 0.2)',
          //tension: 0.3, 
          //pointRadius: 5,
          //fill: true
        }]
      },
      options: chartOptions
    });
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
            .user-actions {
                justify-self: right;
            }

            .or {
                margin-top: 3em;
                margin-bottom: 3em;
            }

            .charts {
                display: grid;
                grid-template-columns: auto auto;
                column-gap: 2px;
                justify-content: space-between;
            }
            .charts canvas {
                margin: auto;
                width: 300px;
            }
            @media (max-width: 768px) {
                .charts canvas {
                    margin: auto;
                    width: 200px;
                }
            }
        |]


-- | Subtract months from UTCTime
addMonth :: Integer -> UTCTime -> UTCTime
addMonth n utcTime=
  let day = utctDay utcTime
      timeOfDay = timeToTimeOfDay $ utctDayTime utcTime
      newDay = addGregorianMonthsClip n day
  in UTCTime newDay (timeOfDayToTime timeOfDay)

-- | Get the first day of the next month at 00:00:00 UTC
firstDayNextMonthUTC :: UTCTime -> UTCTime
firstDayNextMonthUTC now = 
  let today = utctDay now
      (year, month, _) = toGregorian today
      (newYear, newMonth) = if month == 12 then (year + 1, 1) else (year, month + 1)
      firstDay = fromGregorian newYear newMonth 1
  in UTCTime firstDay 0  