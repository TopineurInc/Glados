{-
-- EPITECH PROJECT, 2025
-- glados
-- File description:
-- Builtins
-}

module Builtins
  ( builtins
  , builtinAdd
  , builtinSub
  , builtinMul
  , builtinDiv
  , builtinMod
  , builtinEq
  , builtinLt
  , builtinGt
  , builtinPrint
  , builtinDisplay
  , builtinInput
  , builtinReadLine
  , builtinStringToNumber
  , builtinNumberToString
  , builtinStringLength
  , builtinStringAppend
  , builtinSubstring
  , builtinNot
  , builtinAnd
  , builtinOr
  , builtinFormat
  ) where

import AST
import qualified Data.Map as Map
import Control.Monad (void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Word (Word8)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified SFML.Graphics.Color as SFColor
import qualified SFML.Graphics.RenderWindow as RW
import qualified SFML.Graphics.RectangleShape as Rect
import qualified SFML.Graphics.CircleShape as Circle
import SFML.Graphics.Types (RenderWindow, RectangleShape, CircleShape)
import qualified SFML.System.Vector2 as Vec2
import qualified SFML.Window.Event as Event
import qualified SFML.Window.VideoMode as VideoMode
import qualified SFML.Window.Window as Window

builtins :: Map.Map Name Value
builtins = Map.fromList
  [ -- Common Lisp constants
    ("t", VBool True)
  , ("nil", VBool False)
  -- Builtins
  , ("+", VBuiltin "+" builtinAdd)
  , ("-", VBuiltin "-" builtinSub)
  , ("*", VBuiltin "*" builtinMul)
  , ("div", VBuiltin "div" builtinDiv)
  , ("mod", VBuiltin "mod" builtinMod)
  , ("eq?", VBuiltin "eq?" builtinEq)
  , ("<", VBuiltin "<" builtinLt)
  , (">", VBuiltin ">" builtinGt)
  , ("print", VBuiltin "print" builtinPrint)
  , ("display", VBuiltin "display" builtinDisplay)
  , ("input", VBuiltin "input" builtinInput)
  , ("read-line", VBuiltin "read-line" builtinReadLine)
  , ("string->number", VBuiltin "string->number" builtinStringToNumber)
  , ("number->string", VBuiltin "number->string" builtinNumberToString)
  , ("string-length", VBuiltin "string-length" builtinStringLength)
  , ("string-append", VBuiltin "string-append" builtinStringAppend)
  , ("substring", VBuiltin "substring" builtinSubstring)
  , ("not", VBuiltin "not" builtinNot)
  , ("and", VBuiltin "and" builtinAnd)
  , ("or", VBuiltin "or" builtinOr)
  , ("format", VBuiltin "format" builtinFormat)
  , ("sfml-create-window", VBuiltin "sfml-create-window" builtinSfmlCreateWindow)
  , ("sfml-draw-square", VBuiltin "sfml-draw-square" builtinSfmlDrawSquare)
  , ("sfml-close-window", VBuiltin "sfml-close-window" builtinSfmlCloseWindow)
  , ("sfml-create-rectangle", VBuiltin "sfml-create-rectangle" builtinSfmlCreateRectangle)
  , ("sfml-create-circle", VBuiltin "sfml-create-circle" builtinSfmlCreateCircle)
  , ("sfml-shape-set-position", VBuiltin "sfml-shape-set-position" builtinSfmlShapeSetPosition)
  , ("sfml-shape-set-fill-color", VBuiltin "sfml-shape-set-fill-color" builtinSfmlShapeSetFillColor)
  , ("sfml-window-clear", VBuiltin "sfml-window-clear" builtinSfmlWindowClear)
  , ("sfml-window-draw", VBuiltin "sfml-window-draw" builtinSfmlWindowDraw)
  , ("sfml-window-display", VBuiltin "sfml-window-display" builtinSfmlWindowDisplay)
  , ("sfml-window-is-open", VBuiltin "sfml-window-is-open" builtinSfmlWindowIsOpen)
  ]

type WindowId = Int
type RectangleId = Int
type CircleId = Int

{-# NOINLINE windowRegistry #-}
windowRegistry :: IORef (Int, Map.Map WindowId RenderWindow)
windowRegistry = unsafePerformIO $ newIORef (0, Map.empty)

{-# NOINLINE rectangleRegistry #-}
rectangleRegistry :: IORef (Int, Map.Map RectangleId RectangleShape)
rectangleRegistry = unsafePerformIO $ newIORef (0, Map.empty)

{-# NOINLINE circleRegistry #-}
circleRegistry :: IORef (Int, Map.Map CircleId CircleShape)
circleRegistry = unsafePerformIO $ newIORef (0, Map.empty)

allocateWindow :: RenderWindow -> IO WindowId
allocateWindow window =
  atomicModifyIORef' windowRegistry $ \(nextId, registry) ->
    let newId = nextId + 1
        registry' = Map.insert newId window registry
    in ((newId, registry'), newId)

allocateRectangle :: RectangleShape -> IO RectangleId
allocateRectangle shape =
  atomicModifyIORef' rectangleRegistry $ \(nextId, registry) ->
    let newId = nextId + 1
        registry' = Map.insert newId shape registry
    in ((newId, registry'), newId)

allocateCircle :: CircleShape -> IO CircleId
allocateCircle shape =
  atomicModifyIORef' circleRegistry $ \(nextId, registry) ->
    let newId = nextId + 1
        registry' = Map.insert newId shape registry
    in ((newId, registry'), newId)

lookupWindow :: WindowId -> IO RenderWindow
lookupWindow ident = do
  (_, registry) <- readIORef windowRegistry
  case Map.lookup ident registry of
    Just window -> return window
    Nothing -> error $ "Unknown SFML window handle: " ++ show ident

lookupRectangle :: RectangleId -> IO RectangleShape
lookupRectangle ident = do
  (_, registry) <- readIORef rectangleRegistry
  case Map.lookup ident registry of
    Just shape -> return shape
    Nothing -> error $ "Unknown SFML rectangle handle: " ++ show ident

lookupCircle :: CircleId -> IO CircleShape
lookupCircle ident = do
  (_, registry) <- readIORef circleRegistry
  case Map.lookup ident registry of
    Just shape -> return shape
    Nothing -> error $ "Unknown SFML circle handle: " ++ show ident

removeWindow :: WindowId -> IO (Maybe RenderWindow)
removeWindow ident =
  atomicModifyIORef' windowRegistry $ \(nextId, registry) ->
    let (maybeWindow, registry') =
          Map.updateLookupWithKey (\_ _ -> Nothing) ident registry
    in ((nextId, registry'), maybeWindow)

toInt :: String -> Integer -> Int
toInt label n
  | n < fromIntegral (minBound :: Int) || n > fromIntegral (maxBound :: Int) =
      error $ "Value out of bounds for " ++ label ++ ": " ++ show n
  | otherwise = fromInteger n

toFloat :: String -> Integer -> Float
toFloat label = fromIntegral . toInt label

toWord8 :: String -> Integer -> Word8
toWord8 label n
  | n < 0 || n > 255 = error $ "Color component out of range for " ++ label ++ ": " ++ show n
  | otherwise = fromInteger n

drainWindowEvents :: RenderWindow -> IO ()
drainWindowEvents window = do
  event <- RW.pollEvent window
  case event of
    Just Event.SFEvtClosed -> RW.close window >> drainWindowEvents window
    Just _ -> drainWindowEvents window
    Nothing -> return ()

ensureShape :: Value -> IO (Either RectangleShape CircleShape)
ensureShape (VSFMLRectangle ident) = Left <$> lookupRectangle ident
ensureShape (VSFMLCircle ident) = Right <$> lookupCircle ident
ensureShape _ = error "Type error: expected an SFML shape"

makeColor :: Integer -> Integer -> Integer -> SFColor.Color
makeColor r g b = SFColor.Color (toWord8 "color red" r) (toWord8 "color green" g) (toWord8 "color blue" b) 255

-- Arithmetic operations
builtinAdd :: [Value] -> IO Value
builtinAdd [VInt a, VInt b] = return $ VInt (a + b)
builtinAdd _ = error "Type error: + expects two integers"

builtinSub :: [Value] -> IO Value
builtinSub [VInt a, VInt b] = return $ VInt (a - b)
builtinSub _ = error "Type error: - expects two integers"

builtinMul :: [Value] -> IO Value
builtinMul [VInt a, VInt b] = return $ VInt (a * b)
builtinMul _ = error "Type error: * expects two integers"

builtinDiv :: [Value] -> IO Value
builtinDiv [VInt a, VInt b]
  | b == 0 = error "Division by zero"
  | otherwise = return $ VInt (a `div` b)
builtinDiv _ = error "Type error: div expects two integers"

builtinMod :: [Value] -> IO Value
builtinMod [VInt a, VInt b]
  | b == 0 = error "Division by zero"
  | otherwise = return $ VInt (a `mod` b)
builtinMod _ = error "Type error: mod expects two integers"

-- Comparison operations
builtinEq :: [Value] -> IO Value
builtinEq [VInt a, VInt b] = return $ VBool (a == b)
builtinEq [VBool a, VBool b] = return $ VBool (a == b)
builtinEq [VString a, VString b] = return $ VBool (a == b)
builtinEq _ = return $ VBool False

builtinLt :: [Value] -> IO Value
builtinLt [VInt a, VInt b] = return $ VBool (a < b)
builtinLt _ = error "Type error: < expects two integers"

builtinGt :: [Value] -> IO Value
builtinGt [VInt a, VInt b] = return $ VBool (a > b)
builtinGt _ = error "Type error: > expects two integers"

builtinPrint :: [Value] -> IO Value
builtinPrint [val] = do
  putStrLn $ showValue val
  return val
builtinPrint _ = error "Type error: print expects one argument"

builtinDisplay :: [Value] -> IO Value
builtinDisplay [val] = do
  putStr $ showValue val
  hFlush stdout
  return val
builtinDisplay _ = error "Type error: display expects one argument"

builtinInput :: [Value] -> IO Value
builtinInput [VString prompt] = do
  putStr prompt
  hFlush stdout
  line <- getLine
  return $ VString line
builtinInput [] = do
  line <- getLine
  return $ VString line
builtinInput _ = error "Type error: input expects zero or one argument (prompt)"

builtinReadLine :: [Value] -> IO Value
builtinReadLine [] = do
  line <- getLine
  return $ VString line
builtinReadLine _ = error "Type error: read-line expects no arguments"

builtinStringToNumber :: [Value] -> IO Value
builtinStringToNumber [VString s] =
  case readMaybe s of
    Just n -> return $ VInt n
    Nothing -> error $ "Type error: cannot convert '" ++ s ++ "' to number"
builtinStringToNumber _ = error "Type error: string->number expects a string"

builtinNumberToString :: [Value] -> IO Value
builtinNumberToString [VInt n] = return $ VString (show n)
builtinNumberToString _ = error "Type error: number->string expects an integer"

builtinStringLength :: [Value] -> IO Value
builtinStringLength [VString s] = return $ VInt (toInteger $ length s)
builtinStringLength _ = error "Type error: string-length expects a string"

builtinStringAppend :: [Value] -> IO Value
builtinStringAppend [VString a, VString b] = return $ VString (a ++ b)
builtinStringAppend _ = error "Type error: string-append expects two strings"

builtinSubstring :: [Value] -> IO Value
builtinSubstring [VString s, VInt start, VInt end] =
  let s' = take (fromInteger $ end - start) $ drop (fromInteger start) s
  in return $ VString s'
builtinSubstring _ = error "Type error: substring expects (string, start, end)"

builtinNot :: [Value] -> IO Value
builtinNot [VBool b] = return $ VBool (not b)
builtinNot _ = error "Type error: not expects a boolean"

builtinAnd :: [Value] -> IO Value
builtinAnd [VBool a, VBool b] = return $ VBool (a && b)
builtinAnd _ = error "Type error: and expects two booleans"

builtinOr :: [Value] -> IO Value
builtinOr [VBool a, VBool b] = return $ VBool (a || b)
builtinOr _ = error "Type error: or expects two booleans"

-- Format function (Common Lisp style)
builtinFormat :: [Value] -> IO Value
builtinFormat (dest:VString fmt:args) =
  let formatted = processFormatString fmt args
  in case dest of
       VBool True -> do  -- t means stdout
         putStr formatted
         hFlush stdout
         return $ VBool False  -- nil in Common Lisp
       VBool False -> return $ VString formatted  -- nil means return string
       _ -> error "Type error: format destination must be t or nil"
builtinFormat _ = error "Type error: format expects (destination format-string ...)"

builtinSfmlCreateWindow :: [Value] -> IO Value
builtinSfmlCreateWindow [VInt width, VInt height, VString title] = do
  let w = toInt "window width" width
      h = toInt "window height" height
  when (w <= 0 || h <= 0) $
    error "sfml-create-window: width and height must be positive"
  let videoMode = VideoMode.VideoMode
        { VideoMode.windowWidth = w
        , VideoMode.windowHeight = h
        , VideoMode.windowBPP = 32
        }
  window <- RW.createRenderWindow videoMode title [Window.SFDefaultStyle] Nothing
  RW.setFramerateLimit window 60
  ident <- allocateWindow window
  return $ VSFMLWindow ident
builtinSfmlCreateWindow _ =
  error "Type error: sfml-create-window expects (width height title)"

builtinSfmlDrawSquare :: [Value] -> IO Value
builtinSfmlDrawSquare
  [ VSFMLWindow ident
  , VInt size
  , VInt posX
  , VInt posY
  , VInt colorR
  , VInt colorG
  , VInt colorB
  ] = do
    window <- lookupWindow ident
    let sizeVal = toInt "square size" size
    when (sizeVal <= 0) $
      error "sfml-draw-square: size must be positive"
    let positionX = toFloat "square position x" posX
        positionY = toFloat "square position y" posY
        side = fromIntegral sizeVal
        fillColor = SFColor.Color
          (toWord8 "square color red" colorR)
          (toWord8 "square color green" colorG)
          (toWord8 "square color blue" colorB)
          255
    rectangleResult <- Rect.createRectangleShape
    rectangle <- case rectangleResult of
      Left err -> error $ "sfml-draw-square: unable to create rectangle shape: " ++ show err
      Right shape -> return shape
    Rect.setSize rectangle (Vec2.Vec2f side side)
    Rect.setFillColor rectangle fillColor
    Rect.setPosition rectangle (Vec2.Vec2f positionX positionY)
    let renderLoop = do
          open <- RW.isWindowOpen window
          when open $ do
            drainWindowEvents window
            RW.clearRenderWindow window SFColor.black
            RW.drawRectangle window rectangle Nothing
            RW.display window
            renderLoop
    renderLoop
    Rect.destroy rectangle
    void $ removeWindow ident
    RW.destroy window
    return $ VBool True
builtinSfmlDrawSquare _ =
  error "Type error: sfml-draw-square expects (window size x y r g b)"

builtinSfmlCloseWindow :: [Value] -> IO Value
builtinSfmlCloseWindow [VSFMLWindow ident] = do
  maybeWindow <- removeWindow ident
  case maybeWindow of
    Nothing -> error $ "sfml-close-window: unknown window handle " ++ show ident
    Just window -> do
      RW.close window
      RW.destroy window
      return $ VBool True
builtinSfmlCloseWindow _ =
  error "Type error: sfml-close-window expects a window value"

builtinSfmlCreateRectangle :: [Value] -> IO Value
builtinSfmlCreateRectangle [VInt width, VInt height] = do
  let w = toInt "rectangle width" width
      h = toInt "rectangle height" height
  when (w <= 0 || h <= 0) $
    error "sfml-create-rectangle: width and height must be positive"
  rectangleResult <- Rect.createRectangleShape
  rectangle <- case rectangleResult of
    Left err -> error $ "sfml-create-rectangle: unable to create rectangle shape: " ++ show err
    Right shape -> return shape
  Rect.setSize rectangle (Vec2.Vec2f (fromIntegral w) (fromIntegral h))
  ident <- allocateRectangle rectangle
  return $ VSFMLRectangle ident
builtinSfmlCreateRectangle _ =
  error "Type error: sfml-create-rectangle expects (width height)"

builtinSfmlCreateCircle :: [Value] -> IO Value
builtinSfmlCreateCircle [VInt radius] = do
  let r = toInt "circle radius" radius
  when (r <= 0) $
    error "sfml-create-circle: radius must be positive"
  circleResult <- Circle.createCircleShape
  circle <- case circleResult of
    Left err -> error $ "sfml-create-circle: unable to create circle shape: " ++ show err
    Right shape -> return shape
  Circle.setRadius circle (fromIntegral r)
  ident <- allocateCircle circle
  return $ VSFMLCircle ident
builtinSfmlCreateCircle _ =
  error "Type error: sfml-create-circle expects (radius)"

builtinSfmlShapeSetPosition :: [Value] -> IO Value
builtinSfmlShapeSetPosition [shapeVal, VInt posX, VInt posY] = do
  let position = Vec2.Vec2f (toFloat "shape position x" posX) (toFloat "shape position y" posY)
  shape <- ensureShape shapeVal
  case shape of
    Left rectangle -> Rect.setPosition rectangle position
    Right circle -> Circle.setPosition circle position
  return shapeVal
builtinSfmlShapeSetPosition _ =
  error "Type error: sfml-shape-set-position expects (shape x y)"

builtinSfmlShapeSetFillColor :: [Value] -> IO Value
builtinSfmlShapeSetFillColor [shapeVal, VInt r, VInt g, VInt b] = do
  let color = makeColor r g b
  shape <- ensureShape shapeVal
  case shape of
    Left rectangle -> Rect.setFillColor rectangle color
    Right circle -> Circle.setFillColor circle color
  return shapeVal
builtinSfmlShapeSetFillColor _ =
  error "Type error: sfml-shape-set-fill-color expects (shape r g b)"

builtinSfmlWindowClear :: [Value] -> IO Value
builtinSfmlWindowClear [VSFMLWindow ident, VInt r, VInt g, VInt b] = do
  window <- lookupWindow ident
  drainWindowEvents window
  let color = makeColor r g b
  RW.clearRenderWindow window color
  return $ VSFMLWindow ident
builtinSfmlWindowClear _ =
  error "Type error: sfml-window-clear expects (window r g b)"

builtinSfmlWindowDraw :: [Value] -> IO Value
builtinSfmlWindowDraw [VSFMLWindow ident, shapeVal] = do
  window <- lookupWindow ident
  shape <- ensureShape shapeVal
  case shape of
    Left rectangle -> RW.drawRectangle window rectangle Nothing
    Right circle -> RW.drawCircle window circle Nothing
  return shapeVal
builtinSfmlWindowDraw _ =
  error "Type error: sfml-window-draw expects (window shape)"

builtinSfmlWindowDisplay :: [Value] -> IO Value
builtinSfmlWindowDisplay [VSFMLWindow ident] = do
  window <- lookupWindow ident
  RW.display window
  return $ VSFMLWindow ident
builtinSfmlWindowDisplay _ =
  error "Type error: sfml-window-display expects a window"

builtinSfmlWindowIsOpen :: [Value] -> IO Value
builtinSfmlWindowIsOpen [VSFMLWindow ident] = do
  window <- lookupWindow ident
  open <- RW.isWindowOpen window
  return $ VBool open
builtinSfmlWindowIsOpen _ =
  error "Type error: sfml-window-is-open expects a window"

processFormatString :: String -> [Value] -> String
processFormatString [] _ = []
processFormatString ('~':'%':rest) args = '\n' : processFormatString rest args
processFormatString ('~':'a':rest) (arg:args') = showValue arg ++ processFormatString rest args'
processFormatString ('~':'s':rest) (arg:args') = showValue arg ++ processFormatString rest args'
processFormatString ('~':'A':rest) (arg:args') = showValue arg ++ processFormatString rest args'
processFormatString ('~':'S':rest) (arg:args') = showValue arg ++ processFormatString rest args'
processFormatString (c:rest) args = c : processFormatString rest args

showValue :: Value -> String
showValue (VInt n) = show n
showValue (VBool True) = "#t"
showValue (VBool False) = "#f"
showValue (VString s) = s
showValue (VBuiltin name _) = "<builtin:" ++ name ++ ">"
showValue (VClosure name _) = "<closure:" ++ name ++ ">"
showValue (VSFMLWindow ident) = "<sfml-window:" ++ show ident ++ ">"
showValue (VSFMLRectangle ident) = "<sfml-rectangle:" ++ show ident ++ ">"
showValue (VSFMLCircle ident) = "<sfml-circle:" ++ show ident ++ ">"
