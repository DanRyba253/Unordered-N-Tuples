{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import           Prelude hiding (and, or)
import           Unsafe.Coerce (unsafeCoerce)

-- Nats and Naturals

data Nat = Z | S Nat

data Natural (n :: Nat) where
    NZ :: Natural Z
    NS :: Natural n -> Natural (S n)

natToInt :: Natural n -> Int
natToInt NZ     = 0
natToInt (NS n) = 1 + natToInt n

natAdd :: Natural n -> Natural m -> Natural (n :+: m)
natAdd NZ m = m
natAdd (NS n) m = NS (natAdd n m)

-- U and USplit

data USplit (l :: Nat) a where
    US :: U n a -> U m a -> USplit (n :+: m) a

instance Functor (USplit n) where
    fmap f (US u v) = US (fmap f u) (fmap f v)

data U (n :: Nat) a where
    U0 :: U Z a
    U1 :: a -> U (S Z) a
    UM :: Natural (S (S n)) -> ((a -> Bool) -> USplit (S (S n)) a) -> U (S (S n)) a

instance Functor (U n) where
    fmap _ U0       = U0
    fmap f (U1 a)   = U1 (f a)
    fmap f (UM s g) = UM s \h -> f <$> g (h . f)

-- :+: type family

type family (n :: Nat) :+: (m :: Nat) :: Nat where
    Z :+: m = m
    S n :+: m = S (n :+: m)

-- operations on U and USplit

unatlen :: U n a -> Natural n
unatlen U0 = NZ
unatlen (U1 _) = NS NZ
unatlen (UM n _) = n

ulen :: U n a -> Int
ulen = natToInt . unatlen

unull :: U n a -> Bool
unull U0 = True
unull _  = False

upush :: a -> U n a -> U (S n) a
upush a U0       = U1 a
upush a u@(U1 b) = UM (NS (NS NZ)) \g -> case (g a, g b) of
    (True, True) -> US (upush a u) U0
    (True, False) -> US (U1 a) (U1 b)
    (False, True) -> US (U1 b) (U1 a)
    (False, False) -> US U0 (upush a u)
upush a (UM n f) = UM (NS n) \g -> case (f g, g a) of
    (US ts fs, True) -> US (upush a ts) fs
    (US ts fs, False) -> unsafeCoerce $ US ts (upush a fs)

umerge :: U n a -> U m a -> U (n :+: m) a
umerge U0 u = u
umerge u U0 = unsafeCoerce u
umerge (U1 a) u = upush a u
umerge u (U1 a) = unsafeCoerce $ upush a u
umerge (UM n f) (UM m g) = UM (natAdd n m) \h -> case (f h, g h) of
    (US ts fs, US ts' fs') -> unsafeCoerce $ US (umerge ts ts') (umerge fs fs')

ufold :: b -> (a -> b) -> (a -> Bool) -> (USplit n a -> b) -> U n a -> b
ufold b a2b a2bool split2b u = case u of
    U0                -> b
    U1 a              -> a2b a
    UM _ a2bool2split -> split2b (a2bool2split a2bool)

-- Vec type

data Vec (n :: Nat) a where
    VNil :: Vec Z a
    (:>) :: a -> Vec n a -> Vec (S n) a
infixr 5 :>

-- "forgetful" functions

unorder :: Vec n a -> U n a
unorder VNil = U0
unorder (a :> as) = upush a (unorder as)

unlength :: (forall n . Vec n a -> b) -> [a] -> b
unlength f [] = f VNil
unlength f (a : as) = unlength (f . (a :>)) as

unfuck :: (forall n . U n a -> b) -> [a] -> b
unfuck f = unlength (f . unorder)

-- examples

add :: [Int] -> Int
add = unfuck addU
  where
    addU :: U n Int -> Int
    addU = ufold 0 id (< 0) \(US neg nonNeg) ->
        addNonNeg nonNeg - addPos (negate <$> neg)
    
    addNonNeg :: U n Int -> Int
    addNonNeg = ufold 0 id (== 0) \(US _ p) -> addPos p

    addPos :: U n Int -> Int
    addPos = ufold 0 id even \(US e o) ->
        let e' = (`div` 2) <$> e
            o' = (`div` 2) <$> o
        in 2 * addNonNeg e' + ulen o + 2 * addNonNeg o'

-- >>> add [1, 2, 3, 4 , 5]
-- 15

andU :: U n Bool -> Bool
andU = ufold True id id \(US ts fs) -> unull fs

and2 :: U (S (S Z)) Bool -> Bool
and2 = andU

-- >>> and2 $ unorder (True :> False :> VNil)
-- False

and :: [Bool] -> Bool
and = unfuck andU

orU :: U n Bool -> Bool
orU = ufold False id id \(US ts fs) -> not (unull ts)

or2 :: U (S (S Z)) Bool -> Bool
or2 = orU

-- >>> or2 $ unorder (True :> False :> VNil)
-- True

or :: [Bool] -> Bool
or = unfuck orU

main :: IO ()
main = return ()
