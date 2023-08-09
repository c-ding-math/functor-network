
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Extra where

import Data.Text                     (Text)
import Data.Aeson 

data ForgotPasswordForm = ForgotPasswordForm { _forgotEmail :: Text }
data PasswordForm = PasswordForm { _passwordCurrent :: Text, _passwordNew :: Text, _passwordConfirm :: Text }
data UserForm = UserForm { _userFormEmail :: Text }
data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }

data GoogleUser = GoogleUser {googleUserName::Text}
instance FromJSON GoogleUser where
  parseJSON = withObject "GoogleUser" $ \o -> do
    name <- o .: "name"
    return $ GoogleUser name
