import qualified Data.QRCode as QR
import qualified Data.Version as Version
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude
import qualified Diagrams.QRCode as QR
import qualified Paths_qrencode as Package
import           System.Directory
import           System.Environment
import           System.Exit (exitFailure)
import           System.FilePath

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [   linksFile
      , pngSize
      , outputDirectory ] -> do

      links <- lines <$> readFile linksFile
      let qrFiles = map (\link -> outputDirectory </> "qr_" ++ takeFileName link ++ ".png") links
      qrs <- mapM ((drawQRCode <$>) . qrencode 3) links

      let width = read pngSize
          dims = dims2D width width
          renderFile file = fst . renderDia Cairo (CairoOptions file dims PNG False)

      createDirectoryIfMissing True outputDirectory

      sequence_ $ zipWith renderFile qrFiles qrs
      writeFile (outputDirectory </> "links.csv") $
        unlines $ zipWith (\link file -> link ++ "," ++ takeFileName file)
                          links qrFiles
    _ -> do
      putStrLn $ "qrencode-links " ++ Version.showVersion Package.version
      putStrLn $ "Usage: qrencode-links URL_FILE PNG_SIZE OUTPUT_DIRECTORY"
      exitFailure
