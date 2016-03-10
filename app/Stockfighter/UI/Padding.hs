{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Stockfighter.UI.Padding
  ( Axis(..)
  , Layout(..)
  , rowP
  , colP
  , row
  , col
  , reifyRow
  , reifyCol

  , leftJustify
  , leftJustifyAttr
  , rightJustify
  , rightJustifyAttr
  , fullJustify
  , fullJustifyAttr
  ) where

import Brick hiding (row)
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.Reader
import Data.Maybe (fromJust)
import Graphics.Vty.Image


---- Padded layouts
-- Layouts are padded (Greedy) on a primary axis, either Horiz(ontal)
-- or Vert(ical). The secondary axis of a Layout is Fixed in size
-- equivalent to the widest/talltest (as appropriate) child
--
-- More concretely: a "row" is padded vertically, fixed horizontally
--                  a "col" is padded horizontally, fixed vertically

-- Layouts can be padded on one of two axes
data Axis = Horiz | Vert

-- OtherAxis is used to ensure that we don't nest two layouts sharing
-- a primary axis.
type family OtherAxis (a :: Axis) :: Axis where
    OtherAxis 'Horiz = 'Vert
    OtherAxis 'Vert  = 'Horiz

otherAxis :: Axis -> Axis
otherAxis Horiz = Vert
otherAxis Vert  = Horiz

data Layout (axis :: Axis) a
      = Leaf a
      | Padded [Layout (OtherAxis axis) a]


-- Combinators
colP :: [Layout 'Horiz a] -> Layout 'Vert a
colP = Padded

rowP :: [Layout 'Vert a] -> Layout 'Horiz a
rowP = Padded

row :: [a] -> Layout 'Horiz a
row = Padded . map Leaf

col :: [a] -> Layout 'Vert a
col = Padded . map Leaf


reifyCol :: Layout 'Vert Widget -> Widget
reifyCol = reifyAxis Vert

reifyRow :: Layout 'Horiz Widget -> Widget
reifyRow = reifyAxis Horiz


---- Prerendering
-- A single-pass (pre)render is performed to determine the minimum size
-- required to fit all components
--
-- If a particular Widget is Fixed on both axes, we can keep
-- the rendered result to avoid rendering a second time.
--
-- We also reify the axes into concrete types for later rendering.

data Prerendered a = RLeaf   Axis MinSize (Either Widget Result)
                   | RPadded Axis MinSize [Prerendered a]

-- The smallest fixed size of a given component
-- (or Nothing if the component is Greedy)
data MinSize = MinSize { minWidth  :: Maybe Int
                       , minHeight :: Maybe Int
                       } deriving Show

prerender' :: Axis -> Layout a Widget -> RenderM (Prerendered Widget)
prerender' axis (Leaf widget) = do
    rendered <- render widget

    let img   = image rendered
        width = case hSize widget of
            Fixed  -> Just $ imageWidth img
            Greedy -> Nothing
        height = case vSize widget of
            Fixed  -> Just $ imageHeight img
            Greedy -> Nothing

        result = if vSize widget == Fixed && hSize widget == Fixed
                    then Right rendered
                    else Left widget
    return $ RLeaf axis (MinSize width height) result

prerender' axis (Padded xs) = do
    prerendered <- traverse (prerender' (otherAxis axis)) xs
    let PadOpts{..} = axisOpts axis
        sizes       = prerendered ^.. folded . to preMinSize
        width       = widthA  (sizes ^.. folded . to minWidth  . _Just)
        height      = heightA (sizes ^.. folded . to minHeight . _Just)
    return $ RPadded axis (MinSize (Just width) (Just height)) prerendered

preMinSize :: Prerendered a -> MinSize
preMinSize (RLeaf   _ size _) = size
preMinSize (RPadded _ size _) = size


---- Rendering

-- RIP type safety
-- Initiate the prerender pass and determine the size of our primary
-- axis. If all of our children are greedy on this axis, so are we.
-- (otherwise, constrain the render to the smallest child on this axis)
reifyAxis :: Axis -> Layout a Widget -> Widget
reifyAxis axis layout = Widget Fixed Fixed $ do
     prerendered <- prerender' axis layout

     let PadOpts{..}  = axisOpts axis
         maybePrimary = minSizeP (preMinSize prerendered)

     let limit = case maybePrimary of
           -- Fixed
           Just primary -> withReaderT (ctxPL .~ primary)
           -- Greedy
           Nothing      -> id

     limit (reify prerendered)


-- Take the information we gathered while prerendering to render a final
-- result.
--
-- -- Widgets
-- Take our render result from earlier (or render again if it's greedy
-- on either axis) and pad it (centered) to the desired size.
--
-- -- Containers
-- 1. Constrain further renders to the smallest child size on the
-- secondary axis. (e.g., the Horiz axis for Vert Layouts)
--
-- 2. Determine if all children are `Fixed` on the primary axis
-- - If so, insert whitespace padding (see below)
-- - Otherwise, split remaining space between Greedy components
--
-- 3. Insert padding
-- - If we only have one child, center the child component
-- - If we have several children, replace each cons with equal padding
reify :: Prerendered Widget -> RenderM Result
reify (RLeaf _ _ leaf) = do
    result <- either render return leaf
    width  <- asks availWidth
    height <- asks availHeight
    return (centerImageV height (centerImageH width result))
reify (RPadded axis size rest) = do
    let opts@PadOpts{..} = axisOpts axis
        secondary        = fromJust (minSizeS size) -- ew
        maybePrimaries   = sequence (rest ^.. folded . to preMinSize . to minSizeP)

    case maybePrimaries of
        -- All children are Fixed on the primary axis
        Just primaries  ->
            insertPadding opts =<< zipWithM (\primary cmp ->
                                      limit ctxSL secondary
                                    . limit ctxPL primary
                                    . reify $ cmp) primaries rest
        -- Some or all of the children are Greedy on the primary axis
        Nothing -> limit ctxSL secondary (expandGreedy opts rest)

    where

    limit :: Lens' Context Int -> Int -> ReaderT Context m a -> ReaderT Context m a
    limit l x = withReaderT (l .~ x)

    expandGreedy :: PadOpts -> [Prerendered Widget] -> RenderM Result
    expandGreedy opts@PadOpts{..} widgets = do
        availP <- asks (^. ctxPL)

        let sizes      = widgets ^.. folded . to preMinSize . to minSizeP
            usedP      = sum    (sizes ^.. folded . _Just)
            greedCount = length (sizes ^.. folded . _Nothing)

            pads       = buildPadding (availP - usedP) greedCount

            growGreedy :: [Prerendered Widget] -> RenderM [Result]
            growGreedy cmps = reverse . fst <$> foldM go ([], pads) cmps
                where
                go (rs,ps) prerendered = case minSizeP (preMinSize prerendered) of
                    Just prim -> do
                        rendered <- limit ctxPL prim (reify prerendered)
                        return (rendered : rs, ps)
                    Nothing   -> do
                        let (p':ps') = ps
                        rendered <- limit ctxPL p' (reify prerendered)
                        return (rendered : rs, ps')

        joinResults opts <$> growGreedy widgets

    insertPadding :: PadOpts -> [Result] -> RenderM Result
    insertPadding PadOpts{..} [result] = do
        avail <- asks (^. ctxPL)
        return (centerImageP avail result)

    insertPadding opts@PadOpts{..} results = do
        availP <- asks (^. ctxPL)

        let totalP = sum (results ^.. folded . to image . to imgSizeP)
            pads   = buildPadding (availP - totalP) (length results - 1)

            paddedResults :: [Result]
            paddedResults = zipWith (\pad' res' -> res' & imageL %~ padP pad')
                                    pads results
                                ++ [last results]
        return (joinResults opts paddedResults)

    buildPadding :: Int -> Int -> [Int]
    buildPadding 0     0    = []
    buildPadding total count = amount : buildPadding remaining (count - 1)
        where
        amount    = total `div` count
        remaining = total - amount

    joinResults :: PadOpts -> [Result] -> Result
    joinResults PadOpts{..} = foldl go (Result emptyImage [] [])
        where
        go :: Result -> Result -> Result
        go acc res = acc & imageL   %~ (`imgJoinP` image res')
                         & cursorsL %~ (++ cursors res')
                         & visibilityRequestsL %~ (++ visibilityRequests res')
            where
            res' :: Result
            res' = addResultOffset (mkOnOriginS (acc ^. imageL . to imgSizeP)) res

-- Padding options.
-- The "primary" dimension is the one in which padding is being inserted.
-- Fields ending in "P" are for the primary dimension; "S" otherwise.

axisOpts :: Axis -> PadOpts
axisOpts Vert = vertPadOpts
axisOpts Horiz = horizPadOpts

vertPadOpts  :: PadOpts
vertPadOpts  = PadOpts { minSizeP     = minHeight
                       , minSizeS     = minWidth
                       , ctxPL        = availHeightL
                       , ctxSL        = availWidthL
                       , imgSizeP     = imageHeight
                       , centerImageP = centerImageV
                       , imgJoinP     = vertJoin
                       , padP         = pad 0 0 0
                       , mkOnOriginS  = \p -> Location (0,p)
                       , widthA       = maximum
                       , heightA      = sum
                       }

horizPadOpts :: PadOpts
horizPadOpts = PadOpts { minSizeP     = minWidth
                       , minSizeS     = minHeight
                       , ctxPL        = availWidthL
                       , ctxSL        = availHeightL
                       , imgSizeP     = imageWidth
                       , centerImageP = centerImageH
                       , imgJoinP     = horizJoin
                       , padP         = \amount -> pad 0 0 amount 0
                       , mkOnOriginS  = \p -> Location (p,0)
                       , widthA       = sum
                       , heightA      = maximum
                       }

data PadOpts = PadOpts { -- MinSize getters for each dimension
                         minSizeP     :: MinSize -> Maybe Int
                       , minSizeS     :: MinSize -> Maybe Int
                         -- Context `remaining size` lens
                       , ctxPL        :: Lens' Context Int
                       , ctxSL        :: Lens' Context Int
                         -- Image size getter
                       , imgSizeP     :: Image -> Int
                         -- Center an image on the primary dimension
                       , centerImageP :: Int   -> Result -> Result
                         -- Join two images on the primary dimension
                       , imgJoinP     :: Image -> Image  -> Image
                         -- Padding function for the primary dimension
                       , padP         :: Int   -> Image  -> Image
                         -- Create a point on the secondary
                         -- axis' origin, with the specified primary
                         -- coordinate
                       , mkOnOriginS  :: Int   -> Location
                         -- Height and width algebras for determining
                         -- size when prerendering a container
                       , widthA       :: [Int] -> Int
                       , heightA      :: [Int] -> Int
                       }


---- Justified text
-- Insert padding as appropriate to horizontally-justify text in a
-- vertically-padded container

leftJustify :: [String] -> Layout 'Vert Widget
leftJustify = leftJustifyAttr . map ((,) mempty)

leftJustifyAttr :: [(AttrName, String)] -> Layout 'Vert Widget
leftJustifyAttr = justifyAttr (padRight Max)

rightJustify :: [String] -> Layout 'Vert Widget
rightJustify = rightJustifyAttr . map ((,) mempty)

rightJustifyAttr :: [(AttrName, String)] -> Layout 'Vert Widget
rightJustifyAttr = justifyAttr (padLeft Max)

justifyAttr :: (Widget -> Widget) -> [(AttrName, String)] -> Layout 'Vert Widget
justifyAttr padder xss =
    let maxLen = maximum (xss ^.. folded . _2 . to length)
     in col [padder' (withAttr attr (str xs))
                    | (attr,xs) <- xss
                    , let padder' = if length xs == maxLen
                                        then id
                                        else padder]

fullJustify :: [String] -> Layout 'Vert Widget
fullJustify = col . map str

fullJustifyAttr :: [(AttrName, String)] -> Layout 'Vert Widget
fullJustifyAttr = col . map (uncurry withAttr . over _2 str)


---- Internally helpful (Misc)

centerImageH :: Int -> Result -> Result
centerImageH width res
  | imageWidth img >= width = res
  | otherwise = addResultOffset (Location (padl, 0))
                                (res & imageL %~ pad padl 0 padr 0)
    where
    img = image res

    padl = (width - imageWidth img) `div` 2
    padr = width - imageWidth img - padl

centerImageV :: Int -> Result -> Result
centerImageV height res
  | imageHeight img >= height = res
  | otherwise = addResultOffset (Location (0,padt))
                                (res & imageL %~ pad 0 padt 0 padb)
    where
    img = image res

    padt = (height - imageHeight img) `div` 2
    padb = height - imageHeight img - padt
