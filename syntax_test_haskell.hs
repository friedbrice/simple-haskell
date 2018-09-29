-- SYNTAX TEST "Packages/User/Haskell.sublime-syntax"
{-# LANGUAGE FlexibleInstances, OverloadedStrings, UnicodeSyntax #-}
--  ^^^^^^^^ keyword.other.preprocessor.haskell

module Main (
    Foo(str, int),
    -- a nice comment in the module exports list
    Fooable(..),
    DeriveMoreThanOne,
    main,
    (-->),
    (+:),
    addEmUp,
) where

import Prelude hiding (Traversable(traverse, sequenceA))

import System.Exit (ExitCode(..), exitWith)
import Data.String

----
-- Original Tests
----

x0 = 23*36 -- single line comment
--         ^^ punctuation.definition.comment.haskell
--         ^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash.haskell
x1 = 23*36
-- <- - comment.line.double-dash.haskell

x2 = {- block comment -} 23*36
--   ^^ punctuation.definition.comment.begin.haskell
--   ^^^^^^^^^^^^^^^^^^^ comment.block.haskell
--                    ^^ punctuation.definition.comment.end.haskell
--                      ^ - comment.block.haskell

x3 = {- {-# #-} -} 23*36
--   ^^ punctuation.definition.comment.begin.haskell
--   ^^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--              ^^ punctuation.definition.comment.end.haskell
--                ^ - comment.block.haskell

x4 = {- {- #-} -} 23*36
--   ^^ punctuation.definition.comment.begin.haskell
--   ^^^^^^^^^^^^ comment.block.haskell
--             ^^ punctuation.definition.comment.end.haskell
--               ^ - comment.block.haskell

x5 = {- {- -} -} 23*36
--   ^^ punctuation.definition.comment.begin.haskell
--   ^^^^^^^^^^^ comment.block.haskell
--            ^^ punctuation.definition.comment.end.haskell
--              ^ - comment.block.haskell

x6 = {- {-# -} -} 23*36
--   ^^ punctuation.definition.comment.begin.haskell
--   ^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--             ^^ punctuation.definition.comment.end.haskell
--               ^ - comment.block.haskell

x7 = {- {-# {- test -} -} -} 23*36
--   ^^ punctuation.definition.comment.begin.haskell
--   ^^^^^^^^^^^^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--                        ^^ punctuation.definition.comment.end.haskell
--                          ^ - comment.block.haskell

class (Functor t, Foldable t) => Traversable t where
-- ^^ keyword.other.haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.class.haskell
    {-# MINIMAL traverse | sequenceA #-}
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.preprocessor.haskell
--                                      ^ - meta.preprocessor.haskell

    -- | Map each element of a structure to an action,
    -- evaluate these actions from left to right, and
    -- collect the results. For a version that ignores
    -- the results see 'Data.Foldable.traverse_'.
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash.haskell
    traverse :: Applicative f =>
--  ^^^^^^^^ entity.name.function.haskell
--           ^^ keyword.other.double-colon.haskell
--              ^^^^^^^^^^^ storage.type.haskell
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.type-declaration.haskell
--                            ^^ keyword.other.big-arrow.haskell
        (a -> f b)
--     ^^^^^^^^^^^^ meta.function.type-declaration.haskell
--         ^^ keyword.other.arrow.haskell
        -> t a
--     ^^^^^^^^ meta.function.type-declaration.haskell
--      ^^ keyword.other.arrow.haskell
        -> f (t b)
--     ^^^^^^^^^^^^ meta.function.type-declaration.haskell
--      ^^ keyword.other.arrow.haskell
    traverse f = sequenceA . fmap f
--  ^^^^^^^^^^^^^ meta.function.type-declaration.haskell
--             ^ keyword.operator.haskell
--                         ^ keyword.operator.haskell

    -- | Evaluate each action in the structure from
    -- left to right, and collect the results.
    -- For a version that ignores the results see
    -- 'Data.Foldable.sequenceA_'.
    sequenceA ∷ Applicative f ⇒ t (f a) → f (t a)
--  ^^^^^^^^^ entity.name.function.haskell
--            ^ keyword.other.double-colon.haskell
--              ^^^^^^^^^^^ storage.type.haskell
--                            ^ keyword.other.big-arrow.haskell
--                                       ^ keyword.other.arrow.haskell
    sequenceA = traverse id
--            ^ keyword.operator.haskell

----
-- Specific Tests
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

----
-- Random Stuff
----

main :: IO ()
main = do
    print (tryLet "this")
    let x = 1 + 1
    let y = (+) 2 2
    z <- pure 3
    print (addEmUp x y z)
    putStrLn (str myFoo)
    exitWith ExitSuccess

addEmUp
    :: Int -> Int -> Int -> Int
addEmUp x y z = x + y + z

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
