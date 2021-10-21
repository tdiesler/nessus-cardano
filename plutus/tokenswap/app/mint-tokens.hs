
import           Prelude
import           System.Environment

import           Cardano.Api
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString.Short as SBS
import           Astor.Payout.MintTokens (mintTokensSBS, mintTokensSerialised)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = if nargs > 0 then head args else "result.plutus"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusScript scriptname mintTokensSerialised mintTokensSBS


writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS []
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
