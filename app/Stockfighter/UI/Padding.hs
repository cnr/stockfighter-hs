{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Stockfighter.UI.Padding
  ( hPad
  , vPad
  , HPadded(..)
  , VPadded(..)
  ) where

import Brick
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.Reader
import Graphics.Vty.Image


---- Padded container types

-- A vertically-padded container, with its width
-- fixed at that of the widest `Fixed`-width child
newtype VPadded a = VPadded { runVPadded :: [a] }

-- A horizontally-padded container, with its height
-- fixed at that of the tallest `Fixed`-height child
newtype HPadded a = HPadded { runHPadded :: [a] }


---- Padded typeclass

-- A class of data types that can be rendered, as well as:
-- 1. padTo
--   1a. crop this component to a certain size, if larger than desired
--   1b. insert padding as appropriate to reach the desired size
-- 2. prerender
--   2a. determine the smallest area required to render this component
class Padded b where
    prerender ::               b -> RenderM PSize
    padTo     :: Int -> Int -> b -> RenderM Result

-- The smallest fixed size of a given component
-- (or Nothing if the component is Greedy)
data PSize = PSize { sWidth  :: Maybe Int
                   , sHeight :: Maybe Int
                   }

-- Widgets are Padded.
-- 1. padTo
--   1a. Render this component with limits set to the desired size
--   1b. If the rendered Image is smaller than desired, add equal
--       to each side to center it
-- 2. prerender
--   2a. If the widget is Fixed on an axis, the smallest area required
--       to render can be easily determined
instance Padded Widget where
    padTo width height w = do -- TODO: translate result
        Result preimage _ _ <- render . hLimit width . vLimit height $ w
        return $ Result (centerImageV height (centerImageH width preimage)) [] []

    prerender w = do
        rendered <- image <$> render w

        let width = case hSize w of
                Fixed  -> Just $ imageWidth rendered
                Greedy -> Nothing
            height = case vSize w of
                Fixed  -> Just $ imageHeight rendered
                Greedy -> Nothing
        return $ PSize width height

-- Padded containers are Padded
-- 1. padTo (See `padWith`)
-- 2. prerender (See `listPrerender`)

instance Padded b => Padded (VPadded b) where
    padTo     = listPadTo runVPadded vPadding
    prerender = listPrerender runVPadded maximum sum

instance Padded b => Padded (HPadded b) where
    padTo     = listPadTo runHPadded hPadding
    prerender = listPrerender runHPadded sum maximum


listPadTo :: Padded b
          => (c -> [b]) -- toList (of Padded components)
          -> (forall x. Padded x => [x] -> RenderM Result) -- see padWith
          -> Int -> Int -> c -> RenderM Result
listPadTo toListC padWithC width height =
    withReaderT offset . padWithC . toListC
    where
    offset :: Context -> Context
    offset c = c & availWidthL  .~ width
                 & availHeightL .~ height


listPrerender :: Padded b
              => (c -> [b]) -- toList (of Padded components)
              -> ([Int] -> Int) -- Width algebra
              -> ([Int] -> Int) -- Height algebra
              -> c -> RenderM PSize
listPrerender toListC widthA heightA c = do
    prerendered <- traverse prerender (toListC c)
    let width  = widthA  <$> sequence (prerendered ^.. folded . to sWidth)
        height = heightA <$> sequence (prerendered ^.. folded . to sHeight)
    return $ PSize width height


---- Padding functions

vPad :: Padded b => [b] -> Widget -- TODO: fix vertical size?
vPad xs = Widget Fixed Fixed (vPadding xs) -- TODO: greedy when appropriate?

vPadding :: Padded b => [b] -> RenderM Result
vPadding = padWith vertPadOpts

vertPadOpts :: PadOpts
vertPadOpts = PadOpts { psizeP       = sHeight
                      , psizeS       = sWidth
                      , ctxP         = availHeight
                      , imgSizeP     = imageHeight
                      , centerImageP = centerImageV
                      , imgJoinP     = vertJoin
                      , padP         = pad 0 0 0
                      , padToS       = padTo
                      }

hPad :: Padded b => [b] -> Widget -- TODO: fix horizontal size?
hPad xs = Widget Fixed Fixed (hPadding xs) -- TODO: greedy when appropriate?

hPadding :: Padded b => [b] -> RenderM Result
hPadding = padWith horizPadOpts

horizPadOpts :: PadOpts
horizPadOpts = PadOpts { psizeP       = sWidth
                       , psizeS       = sHeight
                       , ctxP         = availWidth
                       , imgSizeP     = imageWidth
                       , centerImageP = centerImageH
                       , imgJoinP     = horizJoin
                       , padP         = \amount -> pad 0 0 amount 0
                       , padToS       = flip padTo
                       }

-- Padding options.
-- The "primary" dimension is the one in which padding is being inserted.
-- Fields ending in "P" are for the primary dimension; "S" otherwise.
data PadOpts = PadOpts { -- PSize getters for each dimension
                         psizeP       :: PSize   -> Maybe Int
                       , psizeS       :: PSize   -> Maybe Int
                         -- Context `remaining size` getter
                       , ctxP         :: Context -> Int
                         -- Image size getter
                       , imgSizeP     :: Image   -> Int
                         -- Center an image on the primary dimension
                       , centerImageP :: Int     -> Image -> Image
                         -- Join two images on the primary dimension
                       , imgJoinP     :: Image   -> Image -> Image
                         -- Padding function for the primary dimension
                       , padP         :: Int     -> Image -> Image
                         -- padTo with a corrected argument order
                         -- (ordinarily `width -> height -> ...`)
                       , padToS       :: Int -> forall b. Padded b => Int -> b -> RenderM Result
                       }

-- Pad a container with the given options.
-- The options are used to parameterize on the dimension we're padding.
--
-- To describe in concrete terms, this is what happens when we
-- pad on the vertical axis:
--
-- 1. Determine the fixed size of our secondary dimension (width).
-- - This is equivalent to the largest `Fixed`-width child.
-- 2. Determine if all children are `Fixed` on our primary dimension.
-- - If so, insert padding for our children (see below).
-- - Otherwise, split remaining height between Greedy components.
--
-- 3. Insert padding
-- - If we only have one child, center the child component vertically.
-- - If we have several children, repace each cons with equal padding.
padWith :: Padded b => PadOpts -> [b] -> RenderM Result
padWith PadOpts{..} xs = do
    prerendered <- traverse prerender xs

    let primary = maximum (prerendered ^.. folded . to psizeS . _Just)

    case sequence (prerendered ^.. folded . to psizeP) of
        Just secondaries -> insertPadding =<< zipWithM (padToS primary) secondaries xs
        Nothing          -> expandGreedy primary (zip (map psizeP prerendered) xs)

    where

    insertPadding :: [Result] -> RenderM Result
    insertPadding [result] = do
        avail <- asks ctxP
        return (Result (centerImageP avail (image result)) [] []) -- TODO: translate
    insertPadding results = do
        availP <- asks ctxP

        let totalS  = sum (results ^.. folded . to image . to imgSizeP)
            padPize = availP - totalS

            padBetween :: Int -> [Image] -> Image
            padBetween padRem [r,s]    = imgJoinP (padP padRem r) s
            padBetween padRem (r:s:ts) = imgJoinP (padP p r) (padBetween (padRem - p) (s:ts))
                where
                p = padRem `div` (length (r:s:ts) - 1)
            padBetween _ _ = emptyImage

        return (Result (padBetween padPize (map image results)) [] []) -- TODO: translate

    expandGreedy :: Padded b
                 => Int -- primary direction for padTo
                 -> [(Maybe Int,b)] -- secondary, item
                 -> RenderM Result
    expandGreedy = undefined -- TODO: implement


---- Helpful (Misc)

centerImageH :: Int -> Image -> Image
centerImageH width img
  | imageWidth img >= width = img
  | otherwise               = pad padl 0 padr 0 img
    where
    padl = (width - imageWidth img) `div` 2
    padr = width - imageWidth img - padl

centerImageV :: Int -> Image -> Image
centerImageV height img
  | imageHeight img >= height = img
  | otherwise                 = pad 0 padt 0 padb img
    where
    padt = (height - imageHeight img) `div` 2
    padb = height - imageHeight img - padt
