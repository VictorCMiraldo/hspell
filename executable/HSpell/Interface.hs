{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module HSpell.Interface where

import qualified Data.Text as T
import           Control.Arrow ((***))
import           Control.Monad.State
import           Control.Monad.Reader

import Lens.Micro
import Lens.Micro.TH

import qualified Brick.Main           as B
import qualified Brick.Types          as B
import qualified Brick.AttrMap        as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Core   as B
import qualified Brick.Widgets.Center as B

import qualified Graphics.Vty as V

------------------------------
-- * LineSelection Widget * --
------------------------------

-- |A line consists of chunks of text and different attributes to
-- be used. Whenever we render a @Left True@ value we lookup
-- the attribute for 'linesSelectionSelectAttr' and display text using it until a new
-- @Left False@ is found, closing the selection.
--
-- It handles no event as this is purely for displaying information.
type LinesSelectionLine = [Either Bool T.Text]

-- |Displays lines with line numbers with the option of switching between
-- attributes to present parts of the text differently. Will wrap lines
-- and grows greedily horizontally.
data LinesSelection n = LinesSelection
  { _lsName   :: n
  , _lsLineNo :: Int
  , _lsLines  :: [LinesSelectionLine]
  }

makeLenses ''LinesSelection

-- |Returns the amount of columns necessary to display a line; attributes
-- count for 0 columns.
lslTextWidth :: LinesSelectionLine -> Int
lslTextWidth = sum . map (either (const 0) B.textWidth)

-- |Splits a line in a given position.
lslSplitAt :: Int -> LinesSelectionLine -> (LinesSelectionLine , LinesSelectionLine)
lslSplitAt _ [] = ([] , [])
lslSplitAt 0 t  = ([] , t)
lslSplitAt n (Left attr : ts) = ((Left attr :) *** id) $ lslSplitAt n ts
lslSplitAt n (Right t   : ts)
  | B.textWidth t > n = let (bef , aft) = T.splitAt n t
                         in ([Right bef] , Right aft : ts)
  | otherwise         = ((Right t :) *** id) $ lslSplitAt (n - B.textWidth t) ts


-- TODO: Make a decent wrapper that tries not to break words.

-- |Wraps a line into chunks of at most @nCols@ columns.
lslWrap :: Int -> LinesSelectionLine -> [LinesSelectionLine]
lslWrap nCols t =
  if lslTextWidth t <= nCols
  then [t]
  else uncurry (:) . (id *** lslWrap nCols) $ lslSplitAt (nCols - 1) t

-- |Base attribute
linesSelectionAttr :: B.AttrName
linesSelectionAttr = "linesSelection"

-- |How to display the line numbers
linesSelectionLineNoAttr :: B.AttrName
linesSelectionLineNoAttr = linesSelectionAttr <> "lineNo"

-- |How to display regular text
linesSelectionTextAttr :: B.AttrName
linesSelectionTextAttr = linesSelectionAttr <> "text"

-- |How to display /selected/ text
linesSelectionSelectAttr :: B.AttrName
linesSelectionSelectAttr = linesSelectionAttr <> "select"

-- |I'm redefining 'B.Render' here because I need to pass it
-- as an argument to 'StateT'; but the definition on the Brick library
-- has two parameters.
type RenderM' n = ReaderT B.Context (State (B.RenderState n))

-- |Renders a 'LinesSelection' into a 'B.Widget'.
renderLinesSelection :: LinesSelection n -> B.Widget n
renderLinesSelection ls = B.Widget B.Greedy B.Fixed $ do
  -- How big is the largest number we'll need to display as a line number?
  let linenoLen = B.textWidth (show $ ls^.lsLineNo + length (ls^.lsLines) - 1)
  c <- B.getContext
  -- Hence, we'll have tWidth columns to display text.
  let tWidth = (c ^. B.availWidthL) - linenoLen - 1
  -- Next, we wrap all lines and add their line number together then
  -- process them into images.
  let aux    = map (id *** lslWrap tWidth) $ zip [ls^.lsLineNo ..] (ls^.lsLines)

  -- Now we look for the necessary attributes,
  txtAttr <- B.lookupAttrName linesSelectionTextAttr
  lnAttr  <- B.lookupAttrName linesSelectionLineNoAttr
  -- And render each line; keeping track of the the /current/ txtAttr
  -- inside a state monad.
  res <- evalStateT (mapM (uncurry (mkLineImage lnAttr linenoLen)) aux) txtAttr
  let final = V.vertCat $ map (V.resizeWidth (c ^. B.availWidthL)) res
  return $ B.emptyResult & B.imageL .~ final
 where
   mkLineImage :: V.Attr -> Int -> Int -> [LinesSelectionLine]
               -> StateT V.Attr (RenderM' n) V.Image
   mkLineImage _  _   _ []     = return $ V.emptyImage
   mkLineImage la pad ln [line] = mkLineImage1 la pad ln True line
   mkLineImage la pad ln (l:ll) = V.vertCat <$> ((:) <$> mkLineImage1 la pad ln True l
                                                     <*> mapM (mkLineImage1 la pad ln False) ll)

   mkLineImage1 :: V.Attr -> Int -> Int -> Bool -> LinesSelectionLine 
                -> StateT V.Attr (RenderM' n) V.Image
   mkLineImage1 la pad ln showLn line =
     let lineNoTxt = if showLn then T.pack (take pad $ show ln ++ repeat ' ')
                               else T.pack (replicate pad ' ')
         lineNoImg = V.text' la lineNoTxt 
         sepImg    = V.text' V.defAttr (T.singleton ' ')
      in V.horizCat . ([lineNoImg , sepImg] ++) <$> mapM mkLineElemImg line

   mkLineElemImg :: Either Bool T.Text -> StateT V.Attr (RenderM' n) V.Image
   mkLineElemImg (Left beginSel) = do
     let attr = if beginSel then linesSelectionSelectAttr else linesSelectionTextAttr 
     newAttr <- lift $ B.lookupAttrName attr
     put newAttr
     return V.emptyImage
   mkLineElemImg (Right txt) = do
     attr <- get
     return $ V.text' attr txt

----------------------------
-- * Button Grid Widget * --
----------------------------

data ButtonGrid n = ButtonGrid
  { _bgButtons :: [(Char, (T.Text , n))]
  , _bgName    :: n
  }
makeLenses ''ButtonGrid

buttonGridAttr :: B.AttrName
buttonGridAttr = "buttonGrid"

buttonGridKeyAttr :: B.AttrName
buttonGridKeyAttr = buttonGridAttr <> "key"

buttonGridLabelAttr :: B.AttrName
buttonGridLabelAttr = buttonGridAttr <> "label"

buttonGridSelectedAttr :: B.AttrName
buttonGridSelectedAttr = buttonGridAttr <> "selected"

renderButtonGrid' :: (Eq n) => (V.Attr -> V.Attr -> (T.Text , n) -> V.Image)
                  -> ButtonGrid n -> B.Widget n
renderButtonGrid' sel bg = B.Widget B.Greedy B.Fixed $ do
  -- Usual shenanigans: get available width and understand
  -- how many buttons we'll place in each row based on
  -- the largest button. We add 8 to the maximum witdh to confortably
  -- fit spaces, the character label and its separator.
  ctx <- B.getContext
  let w = ctx ^. B.availWidthL
  let maxW    = 8 + maximum (map (\(_ , (t , _)) -> B.textWidth t) $ bg ^. bgButtons)
  -- We now want to get the button attributes:
  keyAttr <- B.lookupAttrName buttonGridKeyAttr
  lblAttr <- B.lookupAttrName buttonGridLabelAttr 
  selAttr <- B.lookupAttrName buttonGridSelectedAttr 
  -- And render the buttons:
  let buttons = flip map (bg ^. bgButtons) $ \(c , lbl) ->
        V.resizeWidth maxW $
        V.horizCat [ V.text' keyAttr (T.singleton c)
                   , V.text' V.defAttr (T.pack ") ")
                   , sel lblAttr selAttr lbl
                   ]
  
  -- Group them by chunks and render:
  let final = V.vertCat . map (V.resizeHeight 2 . V.horizCat) $ chunksOf (w `div` maxW) buttons
  return $ B.emptyResult & B.imageL .~ final
 where
   chunksOf _ [] = []
   chunksOf n l  = let (cl , ls) = splitAt n l
                    in cl : chunksOf n ls

-- |The event handler for the 'ButtonGrid' is quite simplistic. It returns
-- whether a button was pressed and that's it. This enables us to catch
-- a single button press from the user quite easily.
handleButtonGridEvent :: V.Event -> ButtonGrid n -> B.EventM n (Maybe n)
handleButtonGridEvent ev bg = return $
  case ev of
    V.EvKey (V.KChar c) [] -> fmap snd $ lookup c (bg ^. bgButtons)
    _                      -> Nothing

----------------------------------
-- * The Suggestion Interface * --
----------------------------------

newtype Auxiliar t = Aux { unAux :: t }
  deriving (Show)

instance Eq (Auxiliar t) where
  (==) _ _ = True
instance Ord (Auxiliar t) where
  compare _ _ = EQ

data SugButtonName
  = SugBut_AcceptSession
  | SugBut_Insert
  | SugBut_ReplaceFor (Auxiliar T.Text)
  | SugBut_Sug Char
  deriving (Eq , Show , Ord)

data SugName
  = Sug_LineSelection
  | Sug_ButtonGrid
  | Sug_Button SugButtonName
  deriving (Eq , Show , Ord)

data SugUI = SugUI
  { _suiLinesSelection :: LinesSelection SugName
  , _suiButtonGrid     :: ButtonGrid SugName
  , _suiUserResponse   :: Maybe SugButtonName
  }
makeLenses ''SugUI

drawSugUI :: SugUI -> [B.Widget SugName]
drawSugUI sui = [ w ]
  where
    w = B.vBox [ B.border $ renderLinesSelection (sui ^. suiLinesSelection)
               , renderButtonGrid' dynLabelFun (sui ^. suiButtonGrid)
               ]

    dynLabelFun :: V.Attr -> V.Attr -> (T.Text , SugName) -> V.Image
    dynLabelFun lblAttr selAttr (lbl , n)
      | Just n == fmap Sug_Button (sui ^. suiUserResponse) =
          V.text' selAttr $ case sui ^. suiUserResponse of
                              Just (SugBut_ReplaceFor (Aux t)) -> t
                              _ -> lbl
      | otherwise = V.text' lblAttr lbl

eventSugUI :: SugUI -> B.BrickEvent SugName () -> B.EventM SugName (B.Next SugUI)
eventSugUI sui (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt sui
eventSugUI sui (B.VtyEvent vev) = do
  mbut <- handleButtonGridEvent vev (sui ^. suiButtonGrid)
  case mbut of
    Just (Sug_Button but) -> B.continue (sui & suiUserResponse .~ Just but)
    _                     -> B.continue sui
eventSugUI sui _ = B.continue sui

        
attrMapSugUI :: B.AttrMap
attrMapSugUI = B.attrMap V.defAttr
  [ (linesSelectionLineNoAttr , V.defAttr `V.withStyle` V.italic
                                          `V.withStyle` V.dim)
  , (linesSelectionSelectAttr , V.defAttr `V.withStyle` V.standout)
  , (buttonGridSelectedAttr   , V.defAttr `V.withStyle` V.italic
                                          `V.withStyle` V.underline)
  , (buttonGridKeyAttr        , V.defAttr `V.withStyle` V.dim)
  ]

app :: B.App SugUI () SugName
app = B.App
  { B.appDraw         = drawSugUI
  , B.appChooseCursor = \_ _ -> Nothing
  , B.appHandleEvent  = eventSugUI
  , B.appStartEvent   = return
  , B.appAttrMap      = const attrMapSugUI
  }

initialLines :: LinesSelection SugName
initialLines = LinesSelection
  { _lsName   = Sug_LineSelection
  , _lsLineNo = 98
  , _lsLines  = [line1,line2,line3,line4,line5]
  } where
      line1 = [Right "Some text on the first line with no arkup"]
      line2 = [Right "on the second " , Left True , Right "we start to add some markup"]
      line3 = [Right "then we go on and on and on"]
      line4 = [Right "and finally " , Left False , Right " we stop the markup"]
      line5 = [Right "and finish the text"]

initialButtons :: ButtonGrid SugName
initialButtons = ButtonGrid
  { _bgName = Sug_ButtonGrid
  , _bgButtons = baseButtons ++ map mkButton "1234567890"
  } where
    baseButtons =
      [('a' , ("acccept" , Sug_Button SugBut_AcceptSession))
      ,('i' , ("insert"  , Sug_Button SugBut_Insert))
      ,('r' , ("replace" , Sug_Button (SugBut_ReplaceFor (Aux ""))))
      ]
      
    mkButton c = (c , (T.pack $ "Button " ++ show c , Sug_Button (SugBut_Sug c)))

initialSt :: SugUI
initialSt = SugUI
  { _suiButtonGrid = initialButtons
  , _suiLinesSelection = initialLines
  , _suiUserResponse = Nothing
  }

tester :: IO ()
tester = do
  _finalState <- B.defaultMain app initialSt
  return ()
