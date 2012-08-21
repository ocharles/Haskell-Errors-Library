-- | This module exports miscellaneous error-handling functions.

module Control.Error.Util (
    -- * Conversion functions
    -- $conversion
    hush,
    hushT,
    note,
    noteT,
    hoistMaybe,
    -- * Either functions
    -- $either
    isLeft,
    isRight,
    fmapR,
    -- * EitherT functions
    -- $eitherT
    fmapRT
    ) where

import Control.Monad (liftM)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))

-- For Documentation
import Data.EitherR (fmapL, fmapLT)

{- $conversion
    Use these functions to convert between 'Maybe', 'Either', 'MaybeT', and
    'EitherT'.

    Note that 'hoistEither' is provided by the @either@ package.
-}
-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush e = case e of
    Left  _ -> Nothing
    Right b -> Just b

-- | Suppress the 'Left' value of an 'EitherT'
hushT :: (Monad m) => EitherT a m b -> MaybeT m b
hushT = MaybeT . liftM hush . runEitherT

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a m = case m of
    Nothing -> Left  a
    Just b  -> Right b

-- | Tag the 'Nothing' value of a 'MaybeT'
noteT :: (Monad m) => a -> MaybeT m b -> EitherT a m b
noteT a = EitherT . liftM (note a) . runMaybeT

-- | Lift a 'Maybe' to the 'MaybeT' monad
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return

{- $either
    Utility functions for 'Either'
-}
-- | Returns whether argument is a 'Left'
isLeft :: Either a b -> Bool
isLeft e = case e of
    Left  _ -> True
    Right _ -> False

-- | Returns whether argument is a 'Right'
isRight :: Either a b -> Bool
isRight e = case e of
    Left  _ -> False
    Right _ -> True

-- | 'fmap' specialized to 'Either', given a name symmetric to 'fmapL'
fmapR :: (a -> b) -> Either l a -> Either l b
fmapR = fmap

{- $eitherT
    Utility functions for 'EitherT'
-}
-- | 'fmap' specialized to 'EitherT', given a name symmetric to 'fmapLT'
fmapRT :: (Functor m) => (a -> b) -> EitherT l m a -> EitherT l m b
fmapRT = fmap
