{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Metric (
    Metric(..),
    closest,
) where


import Data.Proxy
import Data.Ratio
import Data.Word


class Metric a where
    dist :: (Fractional n) => a -> a -> n


closest :: (Fractional n, Ord n, Metric a) => Proxy n -> [a] -> a -> a
closest p xs target = case xs of
    [] -> error "closest: empty list"
    best : xs' -> closest' p target xs' best


closest' :: forall n a. (Fractional n, Ord n, Metric a) => Proxy n -> a -> [a] -> a -> a
closest' p target xs best = case xs of
    [] -> best
    x : xs' -> let
        dx = dist target x :: n
        db = dist target best :: n
        in closest' p target xs' $ case dx < db of
            True -> x
            False -> best


instance Metric Int where
    dist x y = realToFrac $ if x > y
        then x - y
        else y - x


instance Metric Integer where
    dist x y = realToFrac $ if x > y
        then x - y
        else y - x


instance Metric Word8 where
    dist x y = realToFrac $ if x > y
        then x - y
        else y - x


instance Metric Word where
    dist x y = realToFrac $ if x > y
        then x - y
        else y - x


instance Metric Float where
    dist x y = realToFrac $ if x > y
        then x - y
        else y - x


instance Metric Double where
    dist x y = realToFrac $ if x > y
        then x - y
        else y - x


instance Metric (Ratio Integer) where
    dist x y = realToFrac $ if x > y
        then x - y
        else y - x


