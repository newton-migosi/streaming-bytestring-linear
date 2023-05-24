{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as LC
import Data.ByteString.Streaming.Char8 qualified as S
import Data.Char
import Data.Conduit
import Data.Conduit.Binary qualified as CB
import Data.Conduit.List qualified as CL
import Data.Monoid
import Lens.Simple
import Pipes
import Pipes.ByteString qualified as PB
import Pipes.Group qualified as PG
import Stream.Prelude (maps)
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
                            "lazy" -> L.hGetContents hIn >>= L.hPut hOut . l
                            "streaming" -> S.hPut hOut (s (S.hGetContents hIn))
                            "conduit" -> CB.sourceHandle hIn $$ CB.sinkHandle hOut
                            "pipes" -> runEffect $ p (PB.fromHandle hIn) >-> PB.toHandle hOut
                            _ -> info
                    [] -> info
  where
    info = putStrLn "lazy conduit pipe streaming"

s x = S.unlines (maps (S.Chunk "!") (S.lines x))
l x = LC.unlines (map ("!" <>) (LC.lines x))
p = over PB.lines (PG.maps (Pipes.yield "!" >>))
c = CB.lines =$= go
  where
    go = do
        m <- await
        case m of
            Nothing -> return ()
            Just x -> do
                yield "\n"
                yield x
                go

-- ss  = S.unlines . S.zipWithList S.Chunk titles . S.lines
-- ll = LC.unlines . zipWith (\x y -> LC.fromStrict x <> y) titles . LC.lines

titles = map title [1 ..]
  where
    title n =
        let str = show n
            len = length str
            pad = take (10 - len) (repeat ' ')
         in pack (str <> pad)

--
-- s' = S.map toLower . S.intercalate  (S.yield "!\n") . S.lines
-- l' = LC.map toLower . LC.intercalate "!\n"  . LC.lines
