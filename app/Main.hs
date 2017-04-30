module Main where

import Codec.Picture
import Codec.Picture.Types (computeLuma, convertPixel, pixelFold)
import Data.Array.Unboxed
import qualified Data.Map.Strict as Map
import Data.List (dropWhileEnd)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import Data.Word (Word64)
import System.Environment(getArgs)

type Intensity = Word64

-- 'fontFile' is the image containing the font to use. The font image's width
-- must be a multiple of 'charWidth', and the image height must equal
-- 'charHeight'
-- TODO: enforce these constraits
fontFile = "font.png"
charWidth = 15
charHeight = 30

-- TODO: crop font image
main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> putStrLn "error: no input file specified"
    (filename:_) -> do
      maybeSrcImg  <- open filename "source image"
      maybeFontImg <- open fontFile "font image"
      fromMaybe (pure ())
        $   ( \srcImg fontImg -> writeOut filename . asciify fontImg $ crop
              (imageWidth srcImg `quot` charWidth * charWidth)
              (imageHeight srcImg `quot` charHeight * charHeight)
              srcImg
            )
        <$> maybeSrcImg
        <*> maybeFontImg
 where
  writeOut filename img = do
    putStrLn "Generating image..."
    writePng (removeExtension filename ++ "_ascii.png") img
    putStrLn "Successfully generated image"

  open filename name = do
    eitherDynImg <- readImage filename
    case eitherDynImg of
      Left err
        -> putStrLn
            (  "Error opening "
            ++ name
            ++ " '"
            ++ filename
            ++ "'. Reason: "
            ++ err
            )
        *> pure Nothing
      Right dynImg -> case dynImgToPixel8 dynImg of
        Left  err       -> putStrLn ("error: " ++ err) *> pure Nothing
        Right pixel8Img -> pure $ Just pixel8Img

  removeExtension filePath = case dropWhileEnd (/='.') filePath of
    [] -> filePath
    xs -> init xs

-- Attempts to convert a 'DynamicImage' to an 8-bit greyscale image.
dynImgToPixel8 :: DynamicImage -> Either String (Image Pixel8)
dynImgToPixel8 dynImg = case dynImg of
  ImageRGB8   img -> Right $ pixelMap computeLuma img
  ImageRGBA8  img -> Right $ pixelMap computeLuma img
  ImageY8     img -> Right img
  ImageY16    _   -> unsupported "ImageY16"
  ImageYF     _   -> unsupported "ImageYF"
  ImageYA8    img -> Right $ pixelMap computeLuma img
  ImageYA16   _   -> unsupported "ImageYA16"
  ImageRGB16  _   -> unsupported "ImageRGB16"
  ImageRGBF   _   -> unsupported "ImageRGBF"
  ImageRGBA16 _   -> unsupported "ImageRGBA16"
  ImageYCbCr8 img -> Right $ pixelMap computeLuma img
  ImageCMYK8  img -> Right
    $ pixelMap (computeLuma . (convertPixel :: PixelCMYK8 -> PixelRGB8)) img
  ImageCMYK16 _   -> unsupported "ImageCMYK16"
 where
  unsupported colorspace = Left $ "unsupported image colorspace " ++ colorspace

-- TODO: eliminate 'fromJust'
asciify :: Image Pixel8 -> Image Pixel8 -> Image Pixel8
asciify fontImg srcImg = generateImage f
                                       (imageWidth srcImg)
                                       (imageHeight srcImg)
 where
  f :: Int -> Int -> Pixel8
  f x y =
    pixelAt
        fontImg
        ( (+x `rem` charWidth)
        . (*charWidth)
        . fromJust
        . lookupClosest
            (intensity srcImg ! (x `quot` charWidth, y `quot` charHeight))
        $ fontIntensityMap fontImg
        )
      $     y
      `rem` charHeight

lookupClosest :: (Num k, Ord k) => k -> Map.Map k v -> Maybe v
lookupClosest k m = case (Map.lookupLE k m, Map.lookupGE k m) of
  (Nothing    , Nothing    ) -> Nothing
  (Just (_, v), Nothing    ) -> Just v
  (Nothing    , Just (_, v)) -> Just v
  (Just (smallKey, smallVal), Just (bigKey, bigVal)) ->
    Just $ if k - smallKey < bigKey - k then smallVal else bigVal


-- Takes a greyscale image and returns an array where each element is the sum of
-- the values in the image in an area the size of a character. The larger the
-- character size, the smaller the returned array is.
intensity :: Image Pixel8 -> UArray (Int, Int) Intensity
intensity img =
  accumArray (\acc x -> acc + fromIntegral x) 0 arrayBounds
    $ pixelFold appendToIntensityList [] img
 where
  appendToIntensityList acc x y pix =
    ((x `quot` charWidth, y `quot` charHeight), pix) : acc
  arrayBounds =
    ((0, 0), (imageWidth img `quot` charWidth - 1, imageHeight img
      `quot` charHeight
      -      1))

-- Takes a font image, and returns a map from intensity values to font indexes.
fontIntensityMap :: Image Pixel8 -> Map.Map Intensity Int
fontIntensityMap = transpose . pixelFold f Map.empty
 where
  transpose = Map.fromList . fmap swap . Map.toList
  f acc x _y pix = Map.alter incrementOrInitialize (x `quot` charWidth) acc
   where
    incrementOrInitialize Nothing  = Just 0
    incrementOrInitialize (Just n) = Just $ n + fromIntegral pix

crop :: Pixel a => Int -> Int -> Image a -> Image a
crop width height img = generateImage (pixelAt img) width height

--
-- Debugging stuff
--

fromRight (Right x) = x

fnt :: IO (Image Pixel8)
fnt = fromRight . dynImgToPixel8 . fromRight <$> readImage fontFile

generateIntensityImage :: Image Pixel8 -> Image Pixel8
generateIntensityImage img = generateImage
  ( \x y ->
    fromIntegral
      . (`quot`(fromIntegral $ charWidth * charHeight))
      $ computedIntesity
      ! (x `quot` charWidth, y `quot` charHeight)
  )
  (imageWidth img)
  (imageHeight img)
 where
  computedIntesity = intensity img
