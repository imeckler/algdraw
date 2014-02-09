{-# LANGUAGE   ScopedTypeVariables
             , RecordWildCards
             , NamedFieldPuns
             , TypeOperators
             , DataKinds #-}

module Alg where

import qualified Data.Vector as V
import           Data.Vector ((!))
import           Data.Modular
import           GHC.TypeLits

type Perm = V.Vector Int

data Group a
  = Group
  { identity :: a
  , invert   :: a -> a
  , mul      :: a -> a -> a
  }

instance (Enum a, Bounded a, Enum b, Bounded b) => Enum (a, b) where
  toEnum n = let (q, r) = divMod n (size (undefined :: a)) in (toEnum q, toEnum r)

  fromEnum (x, y) = q * (size (undefined :: a)) + r
    where q = fromEnum x
          r = fromEnum y


prod :: Group a -> Group b -> Group (a, b)
prod g h = Group 
         { identity = (identity g, identity h)
         , invert   = \(x, y) -> (invert g x, invert h y)
         , mul      = \(x, y) (a, b) -> (mul g x a, mul h y b)
         }

size :: (Enum a, Bounded a) => a -> Int
size x = fromEnum (maxBound `asTypeOf` x) - fromEnum (minBound `asTypeOf` x) + 1

caley :: (Enum a) => Group a -> a -> (Int -> Int)
caley (Group {..}) x = fromEnum . mul x . toEnum

reify :: (Enum a, Bounded a) => Group a -> a -> Perm
reify g x = V.generate (size x) (caley g x)

compose :: Perm -> Perm -> Perm
p `compose` q = V.map (p !) q

bGrp :: Group Bool
bGrp = Group { identity = False, invert = id, mul }
  where mul False x = x
        mul True  True = False
        mul True False = True

zModN :: SingI n => Group (Int `Mod` n)
zModN = Group { identity = toMod 0, invert = (\n -> 0 - n), mul = (+) }

z17 :: Group (Int `Mod` 17)
z17 = zModN

z17sq :: Group (Int/17, Int/17)
z17sq = prod z17 z17
