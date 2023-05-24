{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as LC
import Data.ByteString.Streaming.Char8 qualified as S
import Data.Char
import Data.Conduit
import Data.Conduit.Binary qualified as CB
import Data.Monoid
import Lens.Simple
import Pipes
import Pipes.ByteString qualified as PB
import Pipes.Group qualified as PG
import System.Environment qualified as IO
import System.IO qualified as IO

main =
    IO.withFile "txt/words3b.txt" IO.ReadMode $ \hIn ->
        IO.withFile "txt/words3c.txt" IO.WriteMode $ \hOut ->
            do
                xs <- IO.getArgs
                case xs of
                    x : _ ->
                        case x of
                            "lazy" -> L.hGetContents hIn >>= L.hPut hOut
                            "streaming" -> S.hPut hOut (S.hGetContents hIn)
                            "conduit" -> CB.sourceHandle hIn $$ CB.sinkHandle hOut
                            "pipes" -> runEffect $ PB.fromHandle hIn >-> PB.toHandle hOut
                            _ -> info
                    [] -> info
  where
    info = putStrLn "lazy conduit pipe streaming"
