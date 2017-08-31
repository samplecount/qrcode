import           Data.List.Split (chunksOf)
import qualified Data.QRCode as QR
import qualified Data.Version as Version
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude
import qualified Diagrams.QRCode as QR
import qualified Paths_qrencode as Package
import           System.Directory (removeFile)
import           System.Environment
import           System.Exit (exitFailure)
import           System.FilePath
import           System.Process (callProcess)
import           Text.Printf (printf)

-- | Convery mm to PDF points (assuming 72 dpi).
mm2pt :: Double -> Double
mm2pt x = x / 25.4 * 72

type SizeSpec2D = SizeSpec V2 Double

-- | A4 paper size.
a4 :: SizeSpec2D
a4 = dims2D (mm2pt 210) (mm2pt 297)

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
stickerPadding :: Int -> Double
stickerPadding = maybe 0 (recip.fromIntegral) . flip lookup qrModules

qrencode qrVersion input =
    QR.encodeString input
                    (Just qrVersion)
                    QR.QR_ECLEVEL_M
                    QR.QR_MODE_EIGHT
                    True

drawQRCode = centerXY
           . sizedAs (square 1 :: D V2 Double)
           . bg white
           . fc black
           . lw 0
           . stroke
           . QR.pathMatrix
           . QR.toMatrix

pageFileName :: String -> Int -> Int -> String
pageFileName fileName d i =
  let (f, e) = splitExtension fileName
  in printf ("%s_%0" ++ show d ++ "d%s") f i e

-- paginate :: Int -> Int -> [D V2 Double] -> [[[D V2 Double]]]
paginate nr nc =
    map (take nr . (++ repeat (replicate nc s)))
  . chunksOf nr
  . map (take nc . (++ repeat s))
  . chunksOf nc
  where
    s = strut 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    [   linksFile
      , cols
      , rows
      , output ] -> do

      links <- lines <$> readFile linksFile
      qrs <- mapM ((drawQRCode <$>) . qrencode 3) links

      let numColumns = read cols
          numRows = read rows

      let pages = flip map (paginate numRows numColumns qrs) $
            padY 1.05 . centerY . vsep 0.1 . map (padX 1.01 . centerX . hsep 0.1)
      let renderFile file = fst . renderDia Cairo (CairoOptions file a4 PDF False)

      case pages of
        [page] -> renderFile output page
        _ -> do
          let d = length (show (length pages))
              outputs = map (\(i, p) -> (pageFileName output d i, p)) . zip [1..] $ pages
          mapM_ (uncurry renderFile) outputs
          callProcess "pdfunite" $ map fst outputs ++ [output]
          mapM_ (removeFile . fst) outputs
    _ -> do
      putStrLn $ "qrencode " ++ Version.showVersion Package.version
      putStrLn $ "Usage: qrencode URL_FILE ROWS COLS OUTPUT_FILE"
      exitFailure
