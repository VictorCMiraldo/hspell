{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module HSpell.Interface.Suggestion (askSuggestion) where

import qualified Data.Text as T
import qualified Data.Set  as S
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
import qualified Brick.Widgets.Edit   as B

import qualified Graphics.Vty as V
------------------------------
import Text.HSpell.Base
import Text.HSpell.Util
------------------------------
import HSpell.Env

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

renderButtonGrid' :: (n -> Bool) -> ButtonGrid n -> B.Widget n
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
  let buttons = flip map (bg ^. bgButtons) $ \(c , (lbl , n)) ->
        V.resizeWidth maxW $
        V.horizCat [ V.text' keyAttr (T.singleton c)
                   , V.text' V.defAttr (T.pack ") ")
                   , V.text' (if sel n then selAttr else lblAttr) lbl
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
  = SugBut_Accept
  | SugBut_Insert
  | SugBut_Replace
  | SugBut_Sug Char
  deriving (Eq , Show , Ord)

data SugUserResponse
  = SUR_Accept
  | SUR_Insert
  | SUR_Replace (Maybe T.Text)
  | SUR_Option Char
  deriving (Eq , Show , Ord)

convertToSUR :: SugName -> Maybe SugUserResponse
convertToSUR (Sug_Button SugBut_Accept)  = Just $ SUR_Accept
convertToSUR (Sug_Button SugBut_Insert)  = Just $ SUR_Insert
convertToSUR (Sug_Button SugBut_Replace) = Just $ SUR_Replace Nothing
convertToSUR (Sug_Button (SugBut_Sug c)) = Just $ SUR_Option c
convertToSUR _                           = Nothing

btnNameWasResponse :: SugButtonName -> SugUserResponse -> Bool
btnNameWasResponse SugBut_Accept  SUR_Accept = True
btnNameWasResponse SugBut_Insert  SUR_Insert = True
btnNameWasResponse SugBut_Replace (SUR_Replace _) = True
btnNameWasResponse (SugBut_Sug c) (SUR_Option c') = c == c'
btnNameWasResponse _ _ = False

data SugName
  = Sug_LineSelection
  | Sug_ButtonGrid
  | Sug_Editor
  | Sug_Button SugButtonName
  deriving (Eq , Show , Ord)

data SugUI = SugUI
  { _suiLinesSelection :: LinesSelection SugName
  , _suiButtonGrid     :: ButtonGrid SugName
  , _suiInputField     :: B.Editor T.Text SugName
  , _suiUserResponse   :: Maybe SugUserResponse
  }
makeLenses ''SugUI

-- |We switch focus to the editor if the user presses 'r'
sugUIEditorFocus :: SugUI -> Bool
sugUIEditorFocus sui = sui ^. suiUserResponse == Just (SUR_Replace Nothing)

drawSugUI :: SugUI -> [B.Widget SugName]
drawSugUI sui = [ B.joinBorders w ]
  where
    w = B.vBox [ B.border $ renderLinesSelection (sui ^. suiLinesSelection)
               , buttonsBorder $ B.padRight B.Max $ renderButtonGrid' isSelected (sui ^. suiButtonGrid)
               , editorBorder  $ B.renderEditor (B.txt . T.unlines) True (sui ^. suiInputField)
               ]

    mkLabel focus text = B.padRight B.Max
                       $ (if focus then B.withAttr buttonGridSelectedAttr else id)
                       $ B.str text

    buttonsLabel = mkLabel (not $ sugUIEditorFocus sui) "Options:"
    editorLabel  = mkLabel (sugUIEditorFocus sui) "Input:"

    buttonsBorder e = B.border $ B.vLimit 10 (B.hLimit 14 (B.vCenter buttonsLabel) B.<+> B.vBorder B.<+> e)
    editorBorder e  = B.border $ B.vLimit 3  (B.hLimit 14 (B.vCenter editorLabel) B.<+> B.vBorder B.<+> e)

    isSelected :: SugName -> Bool
    isSelected (Sug_Button n) = (btnNameWasResponse n <$> sui ^. suiUserResponse) == Just True
    isSelected _              = False
   
eventSugUI :: SugUI -> B.BrickEvent SugName e
           -> B.EventM SugName (B.Next SugUI)
eventSugUI sui (B.VtyEvent ev)
  | sugUIEditorFocus sui =
      case ev of
        -- Enter exit's the editor
        V.EvKey V.KEnter []
          -> B.halt (sui & suiUserResponse .~ Just (SUR_Replace (Just . T.unwords $ B.getEditContents (sui ^. suiInputField)))) 

        -- A shift+enter gets sent to the editor as enter
        -- TODO: Terminal doesn't send shift enter... lol
        V.EvKey V.KEnter [V.MShift]
          -> B.continue =<< B.handleEventLensed sui suiInputField B.handleEditorEvent (V.EvKey V.KEnter [])

        -- otherwise, process it as part of the editor input
        _ -> B.continue =<< B.handleEventLensed sui suiInputField B.handleEditorEvent ev

  -- If the editor is not focused, process options
  | otherwise = case ev of
      V.EvKey (V.KChar 'q') []
        -> B.halt sui
      _ -> do
        mbut <- handleButtonGridEvent ev (sui ^. suiButtonGrid)
        case mbut >>= convertToSUR of
          Nothing                    -> B.continue sui
          Just (SUR_Replace Nothing) -> B.continue (sui & suiUserResponse .~ Just (SUR_Replace Nothing))
          Just but                   -> B.halt     (sui & suiUserResponse .~ Just but)
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

chooseCursorSugUI :: SugUI -> [B.CursorLocation SugName] -> Maybe (B.CursorLocation SugName)
chooseCursorSugUI sug ls 
  | sugUIEditorFocus sug = case filter (\c -> c ^. B.cursorLocationNameL == Just Sug_Editor) ls of
                            [r] -> Just r
                            _   -> Nothing
  | otherwise = Nothing                           

app :: B.App SugUI e SugName
app = B.App
  { B.appDraw         = drawSugUI
  , B.appChooseCursor = chooseCursorSugUI
  , B.appHandleEvent  = eventSugUI
  , B.appStartEvent   = return
  , B.appAttrMap      = const attrMapSugUI
  }

sugUI :: Int                  -- ^ LineNo
      -> [LinesSelectionLine] -- ^ Lines to display
      -> S.Set Text           -- ^ Options for buttons
      -> SugUI
sugUI ln ls opts = SugUI
  { _suiButtonGrid = ButtonGrid (mkOptions opts) Sug_ButtonGrid
  , _suiUserResponse = Nothing
  , _suiInputField = B.editorText Sug_Editor (Just 3) T.empty
  , _suiLinesSelection = LinesSelection Sug_LineSelection ln ls
  }
 where
   baseButtons =
      [('a' , ("acccept" , Sug_Button SugBut_Accept))
      ,('i' , ("insert"  , Sug_Button SugBut_Insert))
      ,('r' , ("replace" , Sug_Button SugBut_Replace))
      ]

   mkOptions :: S.Set Text -> [(Char , (Text , SugName))]
   mkOptions s = baseButtons
              ++ map (\(c , t) -> (c , (t , Sug_Button (SugBut_Sug c))))
                     (zip buttonLabels (S.toList s))

   buttonLabels = "1234567890qwetyuopsdfghjkl"

makeSugUI :: (Monad m) => Suggest -> ReaderT HSpellEnv m SugUI
makeSugUI (Suggest sect opts) = do
  f <- asks envInput
  let ls = fileSectToLinesSelect f
  let ln = max 0 (lLine (fst sect) - ctxLines)
  return $ sugUI ln ls opts
 where
   ctxLines = 3 -- TODO: configure?
   
   fileSectToLinesSelect :: HSpellInFile -> [LinesSelectionLine]
   fileSectToLinesSelect f =
     let (bef , (prf , x , suf) , aft) = getFileSectionWithCtx ctxLines sect f
      in (\mid -> (map ((:[]) . Right) bef) ++ mid ++ (map ((:[]) . Right) aft))
       $ case x of
           []      -> error "Empty selection?"
           (x0:xs) -> case snoc xs of
                        Nothing         -> [[Right prf , Left True , Right x0
                                           , Left False , Right suf]]
                        Just (xs' , xN) -> [[Right prf , Left True , Right x0]]
                                        ++ map ((:[]) . Right) xs'
                                        ++ [[Right xN , Left False , Right suf]]

convertResult :: SugUI -> SugUserResponse -> Maybe SugResult
convertResult _ SUR_Accept = Just SugAccept
convertResult _ SUR_Insert = Just SugInsert
convertResult _ (SUR_Replace (Just r)) = Just $ SugReplaceFor r
convertResult u (SUR_Option c) =
  case lookup c (u ^. suiButtonGrid ^. bgButtons) of
    Nothing      -> Nothing
    Just (t , _) -> Just $ SugReplaceFor t
convertResult _ _ = Nothing

askSuggestion :: Suggest -> ReaderT HSpellEnv IO (Either String SugResult)
askSuggestion sug = do
  st  <- makeSugUI sug
  res <- lift $ B.defaultMain app st
  return $ case res ^. suiUserResponse of
    Nothing -> Left "no user response"
    Just ur -> maybe (Left "no valid result") Right $ convertResult st ur

