{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TypeOperators   #-}

module Animals (main) where

import           Control.Exception           (catch)
import           Control.HException          ((:^:), Both, Only)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (ExceptT (ExceptT), catchE,
                                              runExceptT)
import           Control.Monad.Trans.HExcept (HExcept, HExcept1, HExceptT,
                                              HandlerT, extend, hExceptT,
                                              hThrowE, handler1, orElse, value)
import           Data.Char                   (isAlpha)
import           System.Environment          (getArgs)
import           System.Exit                 (exitFailure)
import           System.IO.Error             (IOError, isDoesNotExistError,
                                              isPermissionError)

data CatBreed = BritishShortHair | Burmese | Siamese deriving (Show, Eq, Ord)

data DogBreed = Cavalier | GoldenRetriever | Husky deriving (Show, Eq, Ord)

type Name = String

data DogType = GuideDog | PetDog | PoliceDog deriving (Show, Eq, Ord)

data Cat = Cat {
  catName  :: !Name,
  catBreed :: !CatBreed
} deriving (Show, Eq, Ord)

data Dog = Dog {
  dogName  :: !Name,
  dogBreed :: !DogBreed,
  dogType  :: !DogType
} deriving (Show, Eq, Ord)

newtype InvalidName = InvalidName String deriving (Show, Eq, Ord)

parseName :: String -> HExcept1 InvalidName Name
parseName name
  | null name || any (not . isAlpha) name = hThrowE $ InvalidName name
  | otherwise = return name

newtype InvalidCatBreed = InvalidCatBreed String deriving (Show, Eq, Ord)

parseCatBreed :: String -> HExcept1 InvalidCatBreed CatBreed
parseCatBreed breed = case breed of
  "british short hair" -> return BritishShortHair
  "burmese"            -> return Burmese
  "siamese"            -> return Siamese
  _                    -> hThrowE $ InvalidCatBreed breed

newtype InvalidDogBreed = InvalidDogBreed String deriving (Show, Eq, Ord)

parseDogBreed :: String -> HExcept1 InvalidDogBreed DogBreed
parseDogBreed breed = case breed of
  "cavalier"         -> return Cavalier
  "golden retriever" -> return GoldenRetriever
  "husky"            -> return Husky
  _                  -> hThrowE $ InvalidDogBreed breed

newtype InvalidDogType = InvalidDogType String deriving (Show, Eq, Ord)

parseDogType :: String -> HExcept1 InvalidDogType DogType
parseDogType dType = case dType of
  "guide"  -> return GuideDog
  "pet"    -> return PetDog
  "police" -> return PoliceDog
  _        -> hThrowE $ InvalidDogType dType

newtype InvalidCatFormat = InvalidCatFormat String deriving (Show, Eq, Ord)

type CatParseError = InvalidCatBreed :^: InvalidCatFormat :^: '[]

parseCat :: String -> HExcept (InvalidName :^: CatParseError) Cat
parseCat str = case lines str of
  [name, breed] -> do
    n <- extend $ parseName name
    b <- extend $ parseCatBreed breed
    return $ Cat n b
  _ -> hThrowE $ InvalidCatFormat str

newtype InvalidDogFormat = InvalidDogFormat String deriving (Show, Eq, Ord)

type DogParseError = InvalidDogBreed :^: InvalidDogType :^: InvalidDogFormat :^: '[]

parseDog :: String -> HExcept (InvalidName :^: DogParseError) Dog
parseDog str = case lines str of
  [name, breed, dType] -> do
    n <- extend $ parseName name
    b <- extend $ parseDogBreed breed
    t <- extend $ parseDogType dType
    return $ Dog n b t
  _ -> hThrowE $ InvalidDogFormat str

newtype FileDoesNotExistError = FileDoesNotExistError FilePath deriving (Show, Eq, Ord)
newtype FilePermissionError = FilePermissionError FilePath deriving (Show, Eq, Ord)
data GeneralFileReadError = GeneralFileReadError FilePath IOError deriving (Show, Eq)
newtype FileLengthError = FileLengthError FilePath deriving (Show, Eq, Ord)

type FileError = FileDoesNotExistError :^: FilePermissionError :^: GeneralFileReadError :^: FileLengthError :^: '[]

readAnimalFile :: FilePath -> HExceptT FileError IO String
readAnimalFile file = ExceptT $ runExceptT readAndCheckSize `catch` (runExceptT . handleIOError)
  where
    readAndCheckSize :: HExceptT FileError IO String
    readAndCheckSize = do
      contents <- lift $ readFile file
      let !first1001 = take 1001 contents
      if length first1001 > 1000 then
        hThrowE $ FileLengthError file
      else
        return first1001

    handleIOError :: HandlerT (Only IOError) FileError IO String
    handleIOError = handler1 $ \e ->
      if isDoesNotExistError e then hThrowE $ FileDoesNotExistError file
      else if isPermissionError e then hThrowE $ FilePermissionError file
      else hThrowE $ GeneralFileReadError file e

type AnimalsParseError = InvalidName :^: Both DogParseError CatParseError

type ProcessAnimalsError = Both FileError AnimalsParseError

processAnimalFiles :: FilePath -> FilePath -> HExceptT ProcessAnimalsError IO (Cat, Dog)
processAnimalFiles catFile dogFile = do
  catData <- extend $ readAnimalFile catFile
  dogData <- extend $ readAnimalFile dogFile
  cat <- hExceptT . extend $ parseCat catData
  dog <- hExceptT . extend $ parseDog dogData
  return (cat, dog)

type AnimalHandler es = HandlerT es '[] IO ()

displayFileProblem :: AnimalHandler FileError
displayFileProblem =
  handler1 (\(FileDoesNotExistError f) -> lift . putStrLn $ f ++ " could not be found") `orElse`
  handler1 (\(FilePermissionError f) -> lift . putStrLn $ "You do not have permission to read " ++ f) `orElse`
  handler1 (\(GeneralFileReadError f _) -> lift . putStrLn $ "A problem was encountered reading " ++ f) `orElse`
  handler1 (\(FileLengthError f) -> lift . putStrLn $ f ++ " is too large to process")

displayParseProblem :: AnimalHandler AnimalsParseError
displayParseProblem =
  handler1 (\(InvalidName n) -> lift . putStrLn $ n ++ " is not a valid name") `orElse`
  handler1 (\(InvalidCatBreed b) -> lift . putStrLn $ b ++ " is not a valid cat breed") `orElse`
  handler1 (\(InvalidDogBreed b) -> lift . putStrLn $ b ++ " is not a valid dog breed") `orElse`
  handler1 (\(InvalidCatFormat _) -> lift $ putStrLn "Malformed cat file") `orElse`
  handler1 (\(InvalidDogFormat _) -> lift $ putStrLn "Malformed dog file") `orElse`
  handler1 (\(InvalidDogType t) -> lift . putStrLn $ t ++ " is not a valid dog type")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [catFile, dogFile] ->
      let process = do
            (cat, dog) <- processAnimalFiles catFile dogFile
            lift . putStrLn $ "Successfully processed " ++ catName cat ++ " and " ++ dogName dog
      in
      value $ process `catchE` (displayFileProblem `orElse` displayParseProblem)
    _ -> putStrLn "Expected two file names." >> exitFailure
