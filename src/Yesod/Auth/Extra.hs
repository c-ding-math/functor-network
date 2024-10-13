
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Extra where

import Data.Text                     (Text)
--import Data.Aeson 

data ForgotPasswordForm = ForgotPasswordForm { _forgotEmail :: Text }
data PasswordForm = PasswordForm { _passwordCurrent :: Text, _passwordNew :: Text, _passwordConfirm :: Text }
data UserForm = UserForm { _userFormEmail :: Text }
data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }

{-data PluginUser = PluginUser {pluginUserName::Text}
instance FromJSON PluginUser where
  parseJSON = withObject "PluginUser" $ \o -> do
    name <- o .: "name"
    return $ PluginUser name-}
