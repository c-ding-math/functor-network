module Handler.Home where

import Import
import Handler.Entries (getEntriesR)

getHomeR :: UserId -> Handler Html
getHomeR = getEntriesR
