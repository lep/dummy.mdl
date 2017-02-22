{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Data.List
import Data.Fixed

import Data.Aeson

import Text.Printf

import Data.ByteString.Lazy (writeFile)

newtype Quat = Quat (Float, Float, Float, Float)

data Anim = Anim String Float Float
    deriving (Show)

startFrame :: Anim -> Float
startFrame (Anim _ n _) = n

instance ToJSON Anim where
    toJSON (Anim name start end) = object [ "name" .= name, "start" .= p start, "end" .= p end ]

instance ToJSON Quat where
    toJSON = toJSON . showAsInMdl

data Rotation = Rotation Float Quat Quat

instance ToJSON Rotation where
    toJSON (Rotation frame rot spline) =
        object [ "frame" .= p frame
               , "quat" .= rot ]
               -- , "spline" .= spline]

mkAnim :: Int -> Anim
mkAnim n =
    let start =    2*(fromIntegral n)
        end   = 1+ 2*(fromIntegral n)
        name  = toBase n
    in Anim name start end
  where
    charmap = ['a'..'z']
    toBase n
        | n < length charmap = [charmap !! n]
        | otherwise = let (d, r) = n `divMod` length charmap
                      in charmap !! r : toBase d

p :: Float -> String
p = printf "%.6f"

showAsInMdl (Quat (w, x, y, z)) = p x ++", "++p y++", "++p z++", "++p w
showAsInWolfram (Quat (w, x, y, z)) = show w++", "++show x ++", "++show y++", "++show z

instance Show Quat where
    show = showAsInMdl


quat [x,y,z] a =
    let a' = a/180*pi
    in Quat (cos(a'/2), x*sin(a'/2), y*sin(a'/2), z*sin(a'/2))

add (Quat (w1, x1, y1, z1)) (Quat (w2, x2, y2, z2)) =
    let a = (w1 + x1) * (w2 + x2)
        b = (z1 - y1) * (y2 - z2)
        c = (w1 - x1) * (y2 + z2)
        d = (y1 + z1) * (w2 - x2)
        e = (x1 + z1) * (x2 + y2)
        f = (x1 - z1) * (x2 - y2)
        g = (w1 + y1) * (w2 - z2)
        h = (w1 - y1) * (w2 + z2)
    in Quat (b+((-e)-f+g+h)/2, a-(e+f+g+h)/2, c+(e-f+g-h)/2, d+(e-f-g+h)/2)


mkModel r =
    object [ "anims" .= anims
           , "rotations" .= rotations
           , "num" .= length r ]
  where
    (anims, rotations) = unzip $ zipWith go [0..] r
    go index rotation =
        let anim = mkAnim index
            spline = rotation
        in (anim, Rotation (startFrame anim) rotation spline)


main = do
    let interval = [-90, -90+15 .. 90 ]
    let rotations = [add (quat [1,0,0] a) (quat [0,1,0] b) | a <- interval, b <- interval]
    Data.ByteString.Lazy.writeFile "dummy.json" . encode $ mkModel rotations
