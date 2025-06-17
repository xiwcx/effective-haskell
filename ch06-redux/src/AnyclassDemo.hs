{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AnyclassDemo where

class Redacted a where
  redacted :: a -> String
  default redacted :: (Show a) => a -> String
  redacted = show

newtype UserName = UserName String deriving (Show)

instance Redacted UserName where
  redacted (UserName user) = "Username: " <> user

newtype AdminUser = AdminUser UserName
  deriving stock (Show)
  deriving newtype (Redacted)

-- newtype Password = Password String deriving (Redacted)