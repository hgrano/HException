{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TypeOperators   #-}

module Animals (main) where

import           Control.Exception           (catch)
import           Control.HException          ((:^:), Both)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (ExceptT (ExceptT), catchE,
                                              runExceptT)
import           Control.Monad.Trans.HExcept (HExcept, HExcept1, HExceptT,
                                              Handler, extend, hExceptT,
                                              hThrowE, handler1, handlerT,
                                              orDefault, orElse, valueT)
import           Data.Char                   (isAlpha)
import           System.Environment          (getArgs)
import           System.Exit                 (exitFailure)
import           System.IO.Error             (IOError, isDoesNotExistError,
                                              isPermissionError)

-- This example is loosely based on this blog post http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html
-- It is recommend to read the original post to see more traditional Haskell approaches to the problem. The solution
-- in that post was to add extra sum types for exceptions (wrapping the original exception types). This example
-- shows how heterogeneous exceptions can remove the requirement on additional sum types. The example assumes a basic
-- understanding of Monad transformers (transformers package) and, in particular, the 'Control.Monad.Trans.Except'
-- module.

-- We are going to read some files containing information about cats and dogs. Let's start of defining our
-- mock data types to represent these animals:
data CatBreed = BritishShortHair | Burmese | Siamese deriving (Show, Eq, Ord)

data DogBreed = Cavalier | GoldenRetriever | Husky deriving (Show, Eq, Ord)

-- Type of a cat or dog name
type Name = String

-- In this example we'll add an extra field for dogs, showing what "type" of dog it is, to show that these two animals
-- may not always have the same type or number of fields.
data DogType = GuideDog | PetDog | PoliceDog deriving (Show, Eq, Ord)

-- The cat and dog data types which we will read from a file
data Cat = Cat {
  catName  :: !Name,
  catBreed :: !CatBreed
} deriving (Show, Eq, Ord)

data Dog = Dog {
  dogName  :: !Name,
  dogBreed :: !DogBreed,
  dogType  :: !DogType
} deriving (Show, Eq, Ord)

-- Now we'll define some (pure) parsing functions for the cat and dog fields, and corresponding parsing exceptions for
-- when the file content is not valid. Note: we are not going to create 'Control.Exception.Exception'
-- instances for these error types as we are not throwing or catching them, but instead using the returning them as pure
-- values.

-- The cat/dog name is not valid (e.g. has numbers in it). We'll store the invalid name inside the exception.
newtype InvalidName = InvalidName String deriving (Show, Eq, Ord)

-- The 'HExcept1' type returns either a single exception or a value
parseName :: String -> HExcept1 InvalidName Name
parseName name
  | null name || any (not . isAlpha) name =
    -- 'hThrowE' is analogous to 'Control.Monad.Trans.Except.throwE' but lifts a single exception value into a
    -- heterogeneous exception type
    hThrowE $ InvalidName name
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

-- We can create heterogeneous exception types using the ':^:' type-operator. This is roughly equivalent to using the
-- ':' operator for lists, but instead of creating lists of run-time values, it makes lists of compile-time types. The
-- '[] is just the empty list promoted to the type level. You'll need TypeOperators and DataKinds to use these.
type CatParseError = InvalidCatBreed :^: InvalidCatFormat :^: '[]

-- Here is our first example showing how heterogeneous exceptions can be easily composed. We marshall the 'parseName'
-- and 'parseCatBreed' together using the 'extend' function. For example, 'parseName' is defined by its type signature
-- to only return 'InvalidName' exceptions. By using 'extend' we are lifting it into a context in which 'InvalidName' or
-- a 'CatParseError' may be raised. Iff the "target" context we wish to lift to may return a superset of the
-- original exception(s) the code will compile. The benefit of this is we can be as restrictive as possible on the range
-- of exceptions that may arise from a function. This enables type signatures to be very informative.
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

-- Now we'll show we can handle exceptions in the IO monad. We'll set up some exception types which will help us to be
-- more informative to the user if something goes wrong when reading the animal files.
newtype FileDoesNotExistError = FileDoesNotExistError FilePath deriving (Show, Eq, Ord)
newtype FilePermissionError = FilePermissionError FilePath deriving (Show, Eq, Ord)
data UnknownFileReadError = UnknownFileReadError FilePath IOError deriving (Show, Eq)
newtype FileLengthError = FileLengthError FilePath deriving (Show, Eq, Ord)

type FileError = FileDoesNotExistError :^: FilePermissionError :^: UnknownFileReadError :^: FileLengthError :^: '[]

-- We need to use the 'HExceptT' type instead of 'HExcept' as we need to wrap everything in the IO Monad. 'HExceptT'
-- is just a type alias for a heterogeneous version of 'Control.Monad.Trans.Except.ExceptT' from the transformers
-- package
readAnimalFile :: FilePath -> HExceptT FileError IO String
readAnimalFile file = ExceptT $ runExceptT readAndCheckSize `catch` (runExceptT . handleIOError)
  where
    readAndCheckSize = do
      contents <- lift $ readFile file
      let !first1001 = take 1001 contents
      if length first1001 > 1000 then
        hThrowE $ FileLengthError file
      else
        return first1001

    -- We use this function to handle 'IOError's and convert them to something more informative
    handleIOError = handler1 $ \ioErr ->
      if isDoesNotExistError ioErr then hThrowE $ FileDoesNotExistError file
      else if isPermissionError ioErr then hThrowE $ FilePermissionError file
      else hThrowE $ UnknownFileReadError file ioErr

-- The 'Both' type simply means exceptions from both arguments may be returned
type AnimalsParseError = InvalidName :^: Both DogParseError CatParseError

type ProcessAnimalsError = Both FileError AnimalsParseError

-- We now compose our impure and pure functions together
processAnimalFiles :: FilePath -> FilePath -> HExceptT ProcessAnimalsError IO (Cat, Dog)
processAnimalFiles catFile dogFile = do
  catData <- extend $ readAnimalFile catFile
  dogData <- extend $ readAnimalFile dogFile
  cat <- hExceptT . extend $ parseCat catData -- we need 'hExceptT' to lift 'HExcept' values into the IO monad
  dog <- hExceptT . extend $ parseDog dogData
  return (cat, dog)

-- The 'Handler' encodes the type of functions used to handle exceptions. The 'AnimalHandler' takes some set of
-- exceptions es, returns no exceptions (the empty '[]) and gives a String value.
type AnimalHandler es = Handler es '[] String

displayFileProblem :: AnimalHandler FileError
displayFileProblem =
  -- 'orElse' lets us combine handlers: the first handler is tried; if the underlying exception type doesn't match that
  -- which is required by the first handler, then the next handler is tried.
  handler1 (\(FileDoesNotExistError f) -> return $ f ++ " could not be found") `orElse`
  handler1 (\(FilePermissionError f) -> return $ "You do not have permission to read " ++ f) `orElse`
  handler1 (\(UnknownFileReadError f _) -> return $ "A problem was encountered reading " ++ f) `orElse`
  handler1 (\(FileLengthError f) -> return $ f ++ " is too large to process")

displayParseProblem :: AnimalHandler AnimalsParseError
displayParseProblem =
  handler1 (\(InvalidName n) -> return $ n ++ " is not a valid name") `orElse`
  handler1 (\(InvalidCatBreed b) -> return $ b ++ " is not a valid cat breed") `orElse`
  handler1 (\(InvalidDogBreed b) -> return $ b ++ " is not a valid dog breed") `orElse`
  handler1 (\(InvalidCatFormat _) -> return "Malformed cat file") `orElse`
  handler1 (\(InvalidDogFormat _) -> return "Malformed dog file") `orElse`
  handler1 (\(InvalidDogType t) -> return $ t ++ " is not a valid dog type")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [catFile, dogFile] ->
      let process = do
            (cat, dog) <- processAnimalFiles catFile dogFile
            return $ "Successfully processed " ++ catName cat ++ " and " ++ dogName dog
      in do
      -- 'valueT' extracts the value out of a 'HExceptT', but is type safe - i.e. it won't compile unless we have
      -- handled all exceptions
      message <- valueT $ process `catchE` handlerT (displayFileProblem `orElse` displayParseProblem)
      -- If we don't want to provide handlers for every exception we can give a default value instead. On an error the
      -- default value is returned if none of the handlers are compatible with exception
      simpleMessage <- valueT $ process `catchE` handlerT
        (displayParseProblem `orDefault` return "Something went wrong reading a file")
      putStrLn message
      putStrLn simpleMessage
    _ -> putStrLn "Expected two file names." >> exitFailure
