module Rom where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char 
import Data.Int
import Data.Word

data Header = Header {
  sizePrgRom :: Int,
  sizeChrRom :: Int,
  sizePrgRam :: Int
  } deriving (Eq, Show)

data Rom = Rom {
  header :: Header,
  trainer :: L.ByteString,
  prgData :: L.ByteString,
  chrData :: L.ByteString
  } deriving (Eq, Show)

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
  fail = bail
  
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = case runParse parser (ParseState initState 0) of
  Left err-> Left err
  Right (result, _) -> Right result
                  

parseP :: L.ByteString -> Parse Rom
parseP bytes = matchHeader2 (L8.pack "NES\SUB") s ==>
               \_ -> parseByte ==> \s -> identity 

{--

parseP :: L.ByteString -> Maybe Rom
parseP s = matchHeader (L8.pack "NES\SUB") s >>= 
           getByteNum                       >>=
           \(prgSize, s3) -> getByteNum s3  >>=
           \(chrSize, s4) -> Just (Rom (Header prgSize chrSize 0) L.empty L.empty L.empty)
   ---}        
                             {-
parseP :: L.ByteString -> Maybe (Rom, L.ByteString)                                                         
parseP s = case matchHeader (L8.pack "NES") s of 
  Nothing -> Nothing
  Just s1 ->
    case matchByte 0x1A s1 of
      Nothing -> Nothing
      Just s2 ->
        case getByte s2 of
          Nothing -> Nothing
          Just (prgRomSize,s3) -> Nothing
-}