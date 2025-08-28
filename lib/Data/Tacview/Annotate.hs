{-# LANGUAGE ScopedTypeVariables #-}
module Data.Tacview.Annotate where

import Control.Exception.Safe

-- Waiting not-so-patiently for GHC 9.12
data ExceptionAnnotation e = Annotation String (ExceptionAnnotation e) | Ex e
    deriving anyclass (Exception)

instance (Show e) => Show (ExceptionAnnotation e) where
    show (Annotation s ea) = s <> ":\n" <> show ea
    show (Ex e) = show e

whileIO :: String -> IO a -> IO a
whileIO ctxt f = f `catchAny` c where
    c :: SomeException -> IO a
    c ex = case fromException ex of
        Just (ea :: ExceptionAnnotation SomeException) -> throwIO $ Annotation ctxt ea
        Nothing -> throwIO $ Annotation ctxt $ Ex ex
