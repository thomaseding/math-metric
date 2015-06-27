{-# LANGUAGE FlexibleInstances #-}

module Data.Metric (
      Metric(..)
    , closest
    ) where


import Data.Ratio
import Data.Word


class Metric a where
    dist :: (Fractional n) => a -> a -> n


closest :: (Metric a) => [a] -> a -> a
closest xs target = case xs of
    [] -> error "closest: empty list"
    best : xs' -> closest' target xs' best


closest' :: (Metric a) => a -> [a] -> a -> a
closest' target xs best = case xs of
    [] -> best
    x : xs' -> closest' target xs' $ if dist target x < dist target best
        then x
        else best





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


