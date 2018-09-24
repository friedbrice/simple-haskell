-- SYNTAX TEST "Packages/Haskell/Haskell.sublime-syntax"
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
--  ^^^^^^^^ meta.preprocessor.haskell keyword.other.preprocessor

module Main (
-- ^^^ keyword.other
    Foo(str, int),
--  ^^^ meta.declaration.exports.haskell storage.type
    -- a nice comment in the module exports list
    Fooable(..),
    DeriveMoreThanOne,
--  ^^^^^^^^^^^^^^^^^ meta.declaration.exports.haskell storage.type
    main,
    add,
    (-->),
--   ^^^ meta.declaration.exports.haskell keyword.operator
    (+:),
--   ^^^ meta.declaration.exports.haskell keyword.operator
) where
-- ^^^^ keyword.other
import System.Exit (ExitCode(..), exitWith)
--                  ^^^^^^^^ storage.type
import Data.String
-- ^^^ keyword.other

main :: IO ()
main = do
    print (tryLet "this")
    let x = 1 + 1
    let y = (+) 2 2
    z <- pure 3
    print (add x y z)
    putStrLn (str myFoo)
    exitWith ExitSuccess

add
    :: Int -> Int -> Int -> Int
add x y z = x + y + z

data Foo = Foo
    { str :: String
    , int :: Int
    } deriving Show

tryLet :: String -> String
tryLet this =
    let that = this ++ " that"
        those = that ++ " those"
        these = those ++ " these"
    in these

emptyFoo :: Foo
emptyFoo = Foo "" 0

class Fooable a where
    toFoo   :: a -> Foo
    fromFoo :: Foo -> a
    (<=>)   :: Foo -> a -> Bool

instance Fooable Foo where
    toFoo   = id
    fromFoo = id
    x <=> y = str x == str y && int x == int y

instance Fooable String where
    toFoo x = emptyFoo { str = x }
    fromFoo = str
    x <=> y = str x == y

instance Fooable Int where
    toFoo x = emptyFoo { int = x }
    fromFoo = int
    x <=> y = int x == y

class Show a => Foolike a

instance Foolike Foo

instance Foolike a => Foolike [a]

myFoo :: Foo
myFoo = Foo
    { str = multiLineString :: String
    , int = 37
    }

-- where (this is to close the 'Foolike' declaration)

multiLineString :: IsString a => a
multiLineString =
    "here\
    \is\
    \a\
    \multiline\
    \string"

data OneLine a = OneLine { unOneLine :: a }

class OneLiner a where oneLiner :: a -> OneLine a

----
-- Check For Regressions
----

-- deriving more than one type class
data DeriveMoreThanOne = MkDeriveMoreThanOne deriving (Eq, Read, Show)

-- arrows
toUnit :: DeriveMoreThanOne -> ()
toUnit _ = ()

-- question marks in operators
(!!?) :: DeriveMoreThanOne -> DeriveMoreThanOne -> DeriveMoreThanOne
_ !!? _ = MkDeriveMoreThanOne

-- operators that start with '--'
(-->) :: (a -> b) -> (b -> c) -> a -> c
f --> g = g . f

-- operators that end with '--'
(<--) :: (b -> c) -> (a -> b) -> a -> c
f <-- g = f . g

-- comments in class and instance declarations
class SomeClass a -- some comment
instance SomeClass a -- some comment

-- classes can have numbers and primes in their names
class Class0 a
instance Class0 Int

class Class' a
instance Class' Int

-- class constraints and contexts
class (Class0 a, Class' a) => Class0' a
instance Class0 a => Class0 (Maybe a)
instance Class' a => Class' (Maybe a)
instance (Class0 a, Class' a) => Class0' (Maybe a)

-- functions with primes in their names
someFunc' :: Char -> Int -> String
someFunc' char int = replicate int char

-- types with primes in their names
newtype Foo' = Foo' { unFoo' :: Foo }

-- operators, guards, and contexts
(+:) :: Enum a => Int -> a -> a
n +: x
    | n <= 0    = x
    | otherwise = (n - 1) +: succ x

-- infix functions, operator sections, list literals, char literals
someList :: [Char]
someList = (5 +:) `fmap1'` ['a', 'b', 'c']
  where
    fmap1' :: (Char -> Char) -> [Char] -> [Char]
    fmap1' = fmap
