{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ask (Has (..), claim) where

import GHC.Generics
import Relude hiding (Alt)

class Has a s where
  for :: s -> a
  default for :: (Generic s, GHas (HasTypePSym a) (Rep s) a) => s -> a
  for = genericFor

claim :: forall a r m. (MonadReader r m, Has a r) => m a
claim = asks for

instance (Generic s, GHas (HasTypePSym a) (Rep s) a) => Has a s where
  for = genericFor
  {-# INLINE for #-}

genericFor :: forall a s. (Generic s, GHas (HasTypePSym a) (Rep s) a) => s -> a
genericFor = gFor @(HasTypePSym a) . from
{-# INLINE genericFor #-}

type Pred = TyFun (Type -> Type) Bool

type TyFun a b = a -> b -> Type

data HasTypePSym :: Type -> (TyFun (Type -> Type) Bool)

type family Eval (f :: TyFun a b) (x :: a) :: b

type instance Eval (HasTypePSym t) tt = HasTypeP t tt

type family HasTypeP (typ :: Type) (f :: Type -> Type) :: Bool where
  HasTypeP typ (S1 _ (K1 _ typ)) = 'True
  HasTypeP typ (S1 _ (K1 _ f)) = HasTypeP typ (Rep f)
  HasTypeP typ (l :*: r) = Alt (HasTypeP typ l) (HasTypeP typ r)
  HasTypeP typ (l :+: r) = Both (HasTypeP typ l) (HasTypeP typ r)
  HasTypeP typ (S1 _ _) = 'False
  HasTypeP typ (C1 _ f) = HasTypeP typ f
  HasTypeP typ (D1 _ f) = HasTypeP typ f
  HasTypeP typ (K1 _ _) = 'False
  HasTypeP typ U1 = 'False
  HasTypeP typ V1 = 'False

type family Both (b1 :: Bool) (b2 :: Bool) :: Bool where
  Both 'True 'True = 'True

type family Alt (m1 :: Bool) (m2 :: Bool) :: Bool where
  Alt 'True _ = 'True
  Alt _ b = b

class GHas (p :: Pred) (s :: Type -> Type) a where
  gFor :: s x -> a

instance {-# OVERLAPPING #-} GHas p (K1 m a) a where
  gFor = unK1
  {-# INLINE gFor #-}

instance
  {-# OVERLAPPABLE #-}
  (Generic s, GHas p (Rep s) a) =>
  GHas p (K1 m s) a
  where
  gFor (K1 s) = gFor @p (from s)
  {-# INLINE gFor #-}

instance GHas p f a => GHas p (M1 m meta f) a where
  gFor = gFor @p . unM1
  {-# INLINE gFor #-}

instance GProductHas (Eval p l) p l r a => GHas p (l :*: r) a where
  gFor = gpGet @(Eval p l) @p
  {-# INLINE gFor #-}

class GProductHas (left :: Bool) (p :: Pred) l r a where
  gpGet :: (l :*: r) x -> a

instance GHas p l a => GProductHas 'True p l r a where
  gpGet (l :*: _) = gFor @p l
  {-# INLINE gpGet #-}

instance GHas p r a => GProductHas 'False p l r a where
  gpGet (_ :*: r) = gFor @p r
  {-# INLINE gpGet #-}
