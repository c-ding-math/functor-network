{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.EditCategory where

import Import
import Yesod.Form.Bootstrap3
import Parse.Parser(texToHtmlSimple)
import Handler.Parse(parse,userTemporaryDirectory)

data CategoryInput = CategoryInput
    { categoryTitle :: Text
    }

categoryForm :: Maybe CategoryInput -> Form CategoryInput
categoryForm mInput = renderBootstrap3 BootstrapBasicForm $ CategoryInput
    <$> areq titleField titleSettings (categoryTitle <$> mInput)
    where
        titleField = check validateTitle textField
        maxTitleLength = 256
        validateTitle title = if length title <= maxTitleLength then Right title else Left (MsgTooLongByCharCount (length title - maxTitleLength))
        titleSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Just "title"
            , fsAttrs = [("autofocus", ""), ("class", "form-control"), ("placeholder", "keyword")]
            }

postNewCategoryR :: Handler Value
postNewCategoryR = do
    userId <- requireAuthId
    ((result, _), _) <- runFormPost $ categoryForm Nothing
    case result of
        FormSuccess categoryInput -> do
            let title = categoryTitle categoryInput
            userDir<-userTemporaryDirectory
            titleHtml <-liftIO $ parse Nothing userDir texToHtmlSimple title
            currentTime <- liftIO getCurrentTime

            categoryId <- runDB $ insert $ Entry
                            { entryUserId=userId
                            , entryType=Category
                            , entryStatus=Publish
                            , entryInserted=currentTime
                            , entryUpdated=currentTime
                            , entryTitle=title
                            , entryPreamble=Nothing
                            , entryFormat=Format "tex"
                            , entryBody=Nothing
                            , entryCitation=Nothing
                            , entryTitleHtml=titleHtml
                            , entryBodyHtml=""
                            , entryFeatured=False
                            }
            
            return $ object ["id" .= categoryId, "title" .= titleHtml]
        FormFailure msgs -> return $ object ["errors" .= msgs]
        _ -> return $ object ["error" .= ("Invalid form"::Text)]

postEditCategoryR :: EntryId -> Handler Value
postEditCategoryR entryId = do
    --(userId,user) <- requireAuthPair
    ((result, _), _) <- runFormPost $ categoryForm Nothing
    case result of
        FormSuccess categoryInput -> do
            let title = categoryTitle categoryInput
            userDir<-userTemporaryDirectory
            titleHtml <-liftIO $ parse Nothing userDir texToHtmlSimple title
            currentTime <- liftIO getCurrentTime
            runDB $ update entryId [EntryTitle=.title, EntryTitleHtml=.titleHtml, EntryUpdated=.currentTime]
            
            return $ object ["id" .= entryId, "title" .= titleHtml]
        FormFailure msgs -> return $ object ["errors" .= msgs]
        _ -> return $ object ["error" .= ("Invalid form"::Text)]

deleteEditCategoryR :: EntryId -> Handler Value
deleteEditCategoryR entryId = do
    runDB $ delete entryId
    return $ object ["deleted" .= True]