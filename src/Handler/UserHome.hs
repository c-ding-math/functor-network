module Handler.UserHome where

import Import
import Handler.UserEntries (getUserEntriesR)

getUserHomeR :: UserId -> Handler Html
getUserHomeR = getUserEntriesR
