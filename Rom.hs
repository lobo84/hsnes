module Rom
       (Rom,
        Header,
        sizePrgRom,
        chrData,
        prgData,
        header,
        parse
       ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Int
import Data.Word
import Text.Printf
import Data.Bits as B
import Control.Monad

data TvSystem = NTSC
              | PAL

data Header = Header {
  sizePrgRom :: Int,
  sizeChrRom :: Int,
  sizePrgRam :: Int,
  flags6 :: Word8,
  flags7 :: Word8,
  flags9 :: Word8,
  flags10 :: Word8,
  mapperNr :: Word8
  } deriving (Eq)

instance Show Header where
  show (Header sizePrgRom sizeChrRom sizePrgRam flags6 flags7 flags9 flags10 mapper) =
    unlines [ "sizePrgRom " ++ show sizePrgRom
            , "sizeChrRom " ++ show sizeChrRom
            , "sizePrgRam " ++ show sizePrgRam
            , "flags6     " ++ showB flags6
            , "flags7     " ++ showB flags7
            , "flags9     " ++ showB flags9
            , "flags10    " ++ showB flags10
            , "mapperNr   " ++ showB mapper
            ]

showB v = printf "%02X" v
showW v = printf "%04X" v

data Rom = Rom {
  header :: Header,
  trainer :: L.ByteString,
  prgData :: L.ByteString,
  chrData :: L.ByteString
  } deriving (Eq)

instance Show Rom where
  show (Rom header trainer prgData chrData) = unlines [
    "-- Header:",
    (show header),
    "-- Data:",
    "prgData bytes " ++ (show (L.length prgData)),
    "chrData bytes " ++ (show (L.length chrData)),
    "prgData [0:10] " ++ (show (L.unpack (L.take 10 prgData))),
    "chrData [0:10] " ++ (show (L.unpack (L.take 10 chrData)))
    ]

data ParseState = ParseState {
  string :: L.ByteString,
  offset :: Int64
  } deriving (Show)

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
  }



matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
  | prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise = Nothing

matchBytes2 :: L.ByteString -> Parse ()
matchBytes2 prefix = getState ==> \initState -> case () of
  _ | L8.isPrefixOf prefix (string initState) -> putState newState
    | otherwise -> bail "Could not match bytes"
    where
    newState = initState {string = newString, offset = newOffset}
    newString = L8.dropWhile isSpace (L.drop (L.length prefix) (string initState))
    newOffset = offset initState + (L.length prefix)


matchByte :: Word8 -> L.ByteString -> Maybe L.ByteString
matchByte byte str | byte == L.head str = Just (L.tail str)
                   | otherwise = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num <= 0    -> Nothing
                 | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse  (\_ -> Right ((),s) )

getByteNum :: L.ByteString -> Maybe (Int, L.ByteString)
getByteNum s = getBytes 1 s >>? \(s1,s2) -> Just (fromIntegral (L.head s1),s2)

parseBytes :: Int64 -> Parse L.ByteString
parseBytes count = getState ==> \initState ->
  case L.splitAt count (string initState) of
    (bytes,remainder) ->
      if (L.length bytes) == count then
           putState newState ==> \_ -> identity bytes
      else
           bail "not enough bytes left"
      where newState = initState {string = remainder,
                                  offset = newOffset}
            newOffset = offset initState + count

parseBytesCond :: Bool -> Int64 -> Parse L.ByteString
parseBytesCond True count = parseBytes count
parseBytesCond False _ = getState ==> \initState ->
  putState initState ==> \_ -> identity L.empty

parseByte :: Parse Word8
parseByte = do
  s <- parseBytes 1
  return (L.head s)

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState =
          case runParse firstParser initState of
            Left errMessage ->
              Left errMessage
            Right (firstResult, newState) ->
              runParse (secondParser firstResult) newState

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just a >>? f = f a

identity :: a -> Parse a
identity a = Parse (\s -> Right (a,s))

instance Monad Parse where
  return = identity
  (>>=) = (==>)
  (>>) = (==>&)
  fail = bail

instance Applicative Parse where
  pure = return
  (<*>) = ap

instance Functor Parse where
  fmap = liftM

toByteCount :: Int -> Word8 -> Int
toByteCount c b = c * 1024 * (fromIntegral b)

zeroes :: Int -> [Word8]
zeroes num = take num (repeat 0)

trainerBit :: Word8 -> Bool
trainerBit flag = B.testBit flag 2

parseP :: L.ByteString -> Parse Rom
parseP bytes = do
  matchBytes2 (L8.pack "NES\SUB")
  prgSizeRom <- parseByte
  chrSizeRom <- parseByte
  flags6 <- parseByte
  flags7 <- parseByte
  prgSizeRam <- parseByte
  flags9 <- parseByte
  flags10 <- parseByte
  parseBytes 5
  trainer <- parseBytesCond (trainerBit flags6) 512
  let prgByteCount = (fromIntegral (toByteCount 16 prgSizeRom ))
      chrByteCount = (fromIntegral (toByteCount 8 chrSizeRom ))
  prg <- parseBytes prgByteCount
  chr <- parseBytes chrByteCount

  return (Rom (Header
               (toByteCount 16 prgSizeRom)
               (toByteCount 8 chrSizeRom)
               (toByteCount 8 prgSizeRam)
               flags6
               flags7
               flags9
               flags10
               (calcMapper flags6 flags7)
              ) trainer prg chr)


calcMapper :: Word8 -> Word8 -> Word8
calcMapper fl6 fl7 = (B..|.) high low
  where low  = (B.shiftR) fl6 4
        high = (B..&.) fl7 0xF0


parse :: L.ByteString -> Either String Rom
parse bytes = case runParse (parseP bytes) (ParseState bytes 0) of
  Left errorMsg -> Left errorMsg
  Right (rom,_) -> Right rom
