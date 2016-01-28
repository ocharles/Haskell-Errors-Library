-- | This module exports error- and failure-handling combinators that are
-- defined polymorphically. This means that when you use these combinators
-- you don't commit to a specific monad transformer stack in your return
-- type.

module Control.Error.Class
       ( -- * Throwing errors and failing
         throwError, liftEither, mapWithError, mzero, liftMaybe,
         -- * Converting between 'MonadPlus' and 'MonadError'
         note, (??), noteM, (!?), hushM,
         -- ** Eliminating 'MonadError' constraints
         exceptT, catchError,
         -- ** Eliminating 'MonadPlus' constraints
         maybeT, succeeds, fails)
       where

import Control.Monad ((>=>), MonadPlus, mzero)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Maybe (isJust)

-- TODO ExceptT is currently capable of eliminating the 'MonadPlus' constraint,
-- which seems unexpected under this library. I need to explore this and
-- determine if it really is surprising, and if so introduce a new handler.

-- | Fold an 'ExceptT' by providing one continuation for each constructor.
--
-- This can be used to eliminate a 'MonadError' constraint from a computation.
exceptT :: Monad m => (e -> m b) -> (a -> m b) -> ExceptT e m a -> m b
exceptT f g = runExceptT >=> either f g
{-# INLINEABLE exceptT #-}

-- | Eliminate a 'MonadError' constraint by providing a continuation should
-- an error be thrown.
--
-- This is similar to 'Control.Monad.Error.Class.catchError' from "Control.Monad.Error.Class", but
-- <https://www.schoolofhaskell.com/user/dolio/monad-transformers-and-static-effect-scoping#throw-catch-confusion less confusing>.
catchError :: Monad m => ExceptT e m a -> (e -> m a) -> m a
catchError e k = exceptT k return e
{-# INLINEABLE catchError #-}

maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeT mb kb (MaybeT ma) = ma >>= maybe mb kb
{-# INLINEABLE maybeT #-}

-- | Map over a computation that can throw errors, rethrowing errors by applying
-- a transformation.
mapWithError :: (MonadError f m) => (e -> f) -> (a -> b) -> ExceptT e m a -> m b
mapWithError f g (ExceptT m) = m >>= either (throwError . f) (return . g)
{-# INLINEABLE mapWithError #-}

-- | Lift a 'Maybe' into any 'MonadPlus'.
liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
{-# INLINEABLE liftMaybe #-}

-- | Lift an 'Either' value into any monadic computation by calling
-- 'throwError' on 'Left' values and 'return'ing 'Right' values.
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return
{-# INLINEABLE liftEither #-}

-- | Rewrite a 'MonadError' constraint into a failure under 'MonadPlus' by
-- rewriting 'throwError' @_@ as 'mzero'
hushM :: (MonadPlus m) => ExceptT e m a -> m a
hushM = runExceptT >=> either (const mzero) return
{-# INLINEABLE hushM #-}

-- | Tag the 'Nothing' value of a 'Maybe'.
--
-- See '??' for an infix version of 'note'.
note :: (MonadError e m) => e -> Maybe a -> m a
note a = maybe (throwError a) return
{-# INLINEABLE note #-}

-- | Rewrite a 'MonadPlus' constraint into a 'MonadError' constraint by throwing
-- an error if the computation fails with 'mzero'.
--
-- See '!?' for an infix version of 'noteM'.
noteM :: (MonadError e m) => e -> MaybeT m a -> m a
noteM a = runMaybeT >=> maybe (throwError a) return
{-# INLINEABLE noteM #-}

-- | Lift a 'Maybe' value into any monad by calling 'throwError' if it is
-- 'Nothing'.
--
-- This is the infix definition of 'note'.
(??) :: (MonadError e m) => Maybe a -> e -> m a
(??) = flip note
{-# INLINE (??) #-}

-- | Handle failure in a computation by throwing an error if it fails.
--
-- This is the infix definition of 'noteM'.
(!?) :: MonadError e m => MaybeT m a -> e -> m a
(!?) = flip noteM
{-# INLINE (!?) #-}

-- | Analogous to 'Data.Maybe.isJust', but for 'MaybeT'
succeeds :: (Monad m) => MaybeT m a -> m Bool
succeeds = runMaybeT >=> return . isJust
{-# INLINABLE succeeds #-}

-- | Analogous to 'Data.Maybe.isNothing', but for 'MaybeT'
fails :: (Monad m) => MaybeT m a -> m Bool
fails = fmap not . succeeds
{-# INLINABLE fails #-}
