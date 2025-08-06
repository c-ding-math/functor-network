module Handler.Data where

import Import

--getDataR :: UserId -> Handler Html
getDataR userId = do 
    (Entity _ user) <- requireAuth
    when (userId /= userIdent user) $ permissionDenied "You can only access your own data."
    
    -- Here you would typically fetch the user's data from the database
    -- For demonstration, we will just render a simple message
    defaultLayout $ do
        setTitle "User Data"
        [whamlet|<h1>Your Data for #{userIdent user}|]
        [whamlet|<p>This is where your data would be displayed.|]
