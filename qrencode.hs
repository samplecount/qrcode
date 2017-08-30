{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
import           Control.Applicative
import qualified Data.QRCode as QR
import qualified Data.Version as Version
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude
import qualified Diagrams.QRCode as QR
import qualified Paths_qrencode as Package
import           System.Environment
import		 System.Exit (exitFailure)

-- | Convery mm to PDF points (assuming 72 dpi).
mm2pt :: Double -> Double
mm2pt x = x / 25.4 * 72

-- | Size of a rectangular area.
data Size = Size {
    sizeWidth :: Double
  , sizeHeight :: Double
  } deriving (Eq, Show)

-- | Aspect ratio (width / height).
aspectRatio :: Size -> Double
aspectRatio (Size w h) = w / h

-- | Return a 'Size' with width and height flipped.
flipSize :: Size -> Size
flipSize (Size w h) = Size h w

-- | Return a new size that is smaller by a margin at all four sides.
inset :: Double -> Size -> Size
inset x (Size w h) = Size (max 0 (w - 2*x)) (max 0 (h - 2*x))

-- | Scale a 'Size' proportionally to a given width.
scaleWidthTo :: Double -> Size -> Size
scaleWidthTo w' s = Size w' (w' / aspectRatio s)

-- | Convert a 'Size' to a 'SizeSpec2D'.
sizeSpec :: Size -> SizeSpec2D
sizeSpec x = Dims (sizeWidth x) (sizeHeight x)

-- | Use a 'Size' for something transformable.
withSize :: (Transformable a, Enveloped a, V a ~ R2) => Size -> a -> a
withSize = sized . sizeSpec

-- | A4 paper size.
a4 :: Size
a4 = Size (mm2pt 210) (mm2pt 297)

-- | Sticker data.
data Sticker = Sticker {
    stickerSize :: Size     -- ^ Sticker size.
  , bleed :: Double         -- ^ Bleed margin to be trimmed before printing.
  , qrVersion :: Int        -- ^ Which QR version to use.
  } deriving (Show)

-- Maximum soundcloud track id is 9007199254740992 (16 characters)
-- see http://stackoverflow.com/questions/307179/what-is-javascripts-max-int-whats-the-highest-integer-value-a-number-can-go-t
--
-- ilms base URL is http://ilikemysound.org/qr/ (27 characters)
--
-- Maximum ilms URL length is 27 + 16 = 43 characters.
--
-- Minimum QR version for alphanumeric at error correction level M is 3.
-- see http://www.qrcode.com/en/vertable1.html
--
-- TODO: Implement error handling for URLs that are too long!

-- | Square sticker.
stickerSquare :: Sticker
stickerSquare =
  Sticker {
    stickerSize = (Size (mm2pt 23) (mm2pt 23))
  , bleed = (mm2pt 0.5)
  , qrVersion = 3 }

-- | Rectangular sticker.
stickerRect :: Sticker
stickerRect =
  Sticker {
    stickerSize = (Size (mm2pt 86) (mm2pt 57))
  , bleed = (mm2pt 1)
  , qrVersion = 3 }

-- | Mapping from QR code version to number of modules.
qrModules :: [(Int,Int)]
qrModules = [
    (1, 21)
  , (2, 25)
  , (3, 29)
  , (4, 33)
  , (5, 37)
  , (6, 41)
  , (7, 45)
  , (8, 49)
  , (9, 53)
  , (10, 57)
  ]

-- | Width of one QR module.
stickerPadding :: Sticker -> Double
stickerPadding = maybe 0 (recip.fromIntegral) . flip lookup qrModules . qrVersion

-- | Logo image size
-- TODO: Get this from the image
logoSize :: Size
logoSize = Size 5800 2153

qrencode input sticker =
    QR.encodeString input
                    (Just (qrVersion sticker))
                    QR.QR_ECLEVEL_M
                    QR.QR_MODE_EIGHT
                    True

drawQRCode = centerXY
           . sizedAs (square 1 :: D R2)
           . bg white
           . fc black
           . lw 0
           . stroke
           . QR.pathMatrix
           . QR.toMatrix

main :: IO ()
main = do
  args <- getArgs
  case args of
    [   input
      , logoFile
      , webPdfFile
      , webPngFile
      , stickerSquarePdfFile
      , stickerSquarePngFile
      , stickerRectPdfFile
      , stickerRectPngFile ] -> do

        stickerSquareQR <- drawQRCode <$> qrencode input stickerSquare
        stickerRectQR   <- drawQRCode <$> qrencode input stickerRect

        let squareContent = stickerSquareQR
            logo = image logoFile 1 (recip (aspectRatio logoSize))
            background s = withSize s $ stroke (rect 1 (recip (aspectRatio s))) # lw 0 # bg white
            -- showFrame = False
            -- frame = stroke (rect (sizeWidth canvasSize) (sizeHeight canvasSize)) # lw (if showFrame then mm2pt 0.01 else 0)
            rectContent = pad (1 + 8 * stickerPadding stickerRect) . centerXY $
                            rotateBy (-1/4) stickerRectQR
                            ===
                            strutY (2 * stickerPadding stickerRect)
                            ===
                            (logo # scale 1.0125 # translateX 0.0025)
            stickerSquareDia = withSize (inset (bleed stickerSquare) (stickerSize stickerSquare))
                                        (pad (1 + 4 * stickerPadding stickerSquare) squareContent)
                                        <> background (stickerSize stickerSquare)
            stickerRectDia = withSize (inset (bleed stickerRect) (stickerSize stickerRect))
                                      (rectContent # rotateBy (1/4))
                                      <> background (stickerSize stickerRect)
            renderFile file format size = fst . renderDia Cairo (CairoOptions file (sizeSpec size) format False)

        -- Render images
        let webSize = 512
        renderFile webPngFile PNG (Size webSize webSize) squareContent
        renderFile webPdfFile PDF a4 rectContent
        let size = scaleWidthTo webSize (stickerSize stickerSquare)
          in renderFile stickerSquarePngFile PNG size (withSize size stickerSquareDia)
        renderFile stickerSquarePdfFile PDF (stickerSize stickerSquare) stickerSquareDia
        let size = scaleWidthTo webSize (flipSize (stickerSize stickerRect))
          in renderFile stickerRectPngFile PNG size (withSize size (rotateBy (-1/4) stickerRectDia))
        renderFile stickerRectPdfFile PDF (stickerSize stickerRect) stickerRectDia
    _ -> do
	putStrLn $ "qrencode " ++ Version.showVersion Package.version
	putStrLn $ "Usage: qrencode URL LOGO_FILE WEB_PDF_FILE WEB_PNG_FILE STICKER_SQUARE_PDF_FILE STICKER_SQUARE_PNG_FILE STICKER_RECT_PDF_FILE STICKER_RECT_PNG_FILE"
	exitFailure

