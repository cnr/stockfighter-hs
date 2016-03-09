{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Stockfighter.UI.Padding
  ( hPad
  , vPad
  , HPadded(..)
  , VPadded(..)

  , leftJustify
  , leftJustifyAttr
  , rightJustify
  , rightJustifyAttr
  , fullJustify
  , fullJustifyAttr
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
    padTo     = listPadTo runVPadded vertPadOpts
    prerender = listPrerender runVPadded maximum sum

instance Padded b => Padded (HPadded b) where
    padTo     = listPadTo runHPadded horizPadOpts
    prerender = listPrerender runHPadded sum maximum


listPadTo :: Padded b
          => (c -> [b]) -- toList (of Padded components)
          -> PadOpts
          -> Int -> Int -> c -> RenderM Result
listPadTo toListC padOpts width height =
    withReaderT offset . padWith padOpts . toListC
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

vPad :: Padded b => [b] -> Widget
vPad = listPad vertPadOpts

vertPadOpts :: PadOpts
vertPadOpts = PadOpts { psizeP       = sHeight
                      , psizeS       = sWidth
                      , ctxPL        = availHeightL
                      , imgSizeP     = imageHeight
                      , centerImageP = centerImageV
                      , imgJoinP     = vertJoin
                      , padP         = pad 0 0 0
                      , padToS       = padTo
                      }

hPad :: Padded b => [b] -> Widget
hPad = listPad horizPadOpts

horizPadOpts :: PadOpts
horizPadOpts = PadOpts { psizeP       = sWidth
                       , psizeS       = sHeight
                       , ctxPL        = availWidthL
                       , imgSizeP     = imageWidth
                       , centerImageP = centerImageH
                       , imgJoinP     = horizJoin
                       , padP         = \amount -> pad 0 0 amount 0
                       , padToS       = flip padTo
                       }

listPad :: Padded b => PadOpts -> [b] -> Widget
listPad opts@PadOpts{..} xs = Widget Fixed Fixed $ do -- TODO: Greedy?
    -- Constrain our primary dimension to the greatest `Fixed` child's,
    -- then delegate to padWith
    prerendered <- traverse prerender xs

    let maybePrimaries = sequence (prerendered ^.. folded . to psizeP)

    let limit = case maybePrimaries of
          Just primaries -> withReaderT (ctxPL .~ sum primaries)
          Nothing        -> id

    limit (padWith opts xs)

-- Padding options.
-- The "primary" dimension is the one in which padding is being inserted.
-- Fields ending in "P" are for the primary dimension; "S" otherwise.
data PadOpts = PadOpts { -- PSize getters for each dimension
                         psizeP       :: PSize   -> Maybe Int
                       , psizeS       :: PSize   -> Maybe Int
                         -- Context `remaining size` getter
                       , ctxPL        :: Lens' Context Int
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
                       , padToS       :: forall b. Padded b => Int -> Int -> b -> RenderM Result
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

    let secondary = maximum (prerendered ^.. folded . to psizeS . _Just)

    case sequence (prerendered ^.. folded . to psizeP) of
        Just primaries -> insertPadding =<< zipWithM (padToS secondary) primaries xs
        Nothing        -> expandGreedy secondary (zip (map psizeP prerendered) xs)

    where

    insertPadding :: [Result] -> RenderM Result
    insertPadding [result] = do
        avail <- asks (^. ctxPL)
        return (Result (centerImageP avail (image result)) [] []) -- TODO: translate
    insertPadding results = do
        availP <- asks (^. ctxPL)

        let totalP  = sum (results ^.. folded . to image . to imgSizeP)
            padSize = availP - totalP

            padBetween :: Int -> [Image] -> Image
            padBetween padRem [r,s]    = imgJoinP (padP padRem r) s
            padBetween padRem (r:s:ts) = imgJoinP (padP p r) (padBetween (padRem - p) (s:ts))
                where
                p = padRem `div` (length (r:s:ts) - 1)
            padBetween _ _ = emptyImage

        return (Result (padBetween padSize (map image results)) [] []) -- TODO: translate

    expandGreedy :: Padded b
                 => Int -- secondary size for padTo
                 -> [(Maybe Int,b)] -- maybe primary, item
                 -> RenderM Result
    expandGreedy secondary sizes = do
        availP <- asks (^. ctxPL)

        let totalP    = sum (sizes ^.. folded . _1 . _Just)
            padSize   = availP - totalP
            numGreedy = length (sizes ^.. folded . _1 . _Nothing)

            growGreedy :: Padded b
                       => Int -- Number of greedy components remaining
                       -> Int -- Remaining size for greedy components
                       -> [(Maybe Int, b)] -- maybe primary, item
                       -> RenderM Image
            growGreedy _ _ [] = return emptyImage
            growGreedy numRem padRem ((maybePrimary,b):rest) =
                imgJoinP <$> (image <$> padToS secondary primary b)
                         <*> growGreedy numRem' padRem' rest
                where
                (primary,numRem',padRem') = case maybePrimary of
                    Just prim -> (prim  , numRem    , padRem)
                    Nothing   -> (greedy, numRem - 1, padRem - greedy)
                    where
                    greedy = padRem `div` numRem
        image <- growGreedy numGreedy padSize sizes
        return (Result image [] []) -- TODO: translate


---- Justified text
-- Insert padding as appropriate to horizontally-justify text in a
-- vertically-padded container

leftJustify :: [String] -> VPadded Widget
leftJustify = leftJustifyAttr . map ((,) mempty)

leftJustifyAttr :: [(AttrName, String)] -> VPadded Widget
leftJustifyAttr = justifyAttr (padRight Max)

rightJustify :: [String] -> VPadded Widget
rightJustify = rightJustifyAttr . map ((,) mempty)

rightJustifyAttr :: [(AttrName, String)] -> VPadded Widget
rightJustifyAttr = justifyAttr (padLeft Max)

justifyAttr :: (Widget -> Widget) -> [(AttrName, String)] -> VPadded Widget
justifyAttr padder xss =
    let maxLen = maximum (xss ^.. folded . _2 . to length)
     in VPadded [padder' (withAttr attr (str xs))
                        | (attr,xs) <- xss
                        , let padder' = if length xs == maxLen
                                            then id
                                            else padder]

fullJustify :: [String] -> VPadded Widget
fullJustify = VPadded . map str

fullJustifyAttr :: [(AttrName, String)] -> VPadded Widget
fullJustifyAttr = VPadded . map (uncurry withAttr . over _2 str)


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
