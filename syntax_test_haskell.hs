-- SYNTAX TEST "Packages/User/Haskell.sublime-syntax"
{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeOperators, UndecidableInstances, UnicodeSyntax #-}
--  ^^^^^^^^ keyword.other.preprocessor
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.preprocessor
--                                                                                                                         ^ -meta.preprocessor

module Main (
-- ^^^ keyword.other
-- ^^^^^^^^^ meta.declaration.module -meta.declaration.exports
--          ^^ meta.declaration.module meta.declaration.exports
    Foo(str, int),
--  ^^^ storage.type
--     ^^^^^^^^^^ -storage.type
    -- a nice comment in the module exports list
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line
    Fooable(..),
--  ^^^^^^^ storage.type
--         ^^^^ -storage.type
    DeriveMoreThanOne,
    main,
    (-->),
-- ^^^^^^^^ -comment.line
--  ^ -keyword.operator
--   ^^^ keyword.operator
--      ^ -keyword.operator
    (++:),
--  ^ -keyword.operator
--   ^^^ keyword.operator
--      ^ -keyword.operator
    addEmUp,
    ) where
--    ^^^^^ keyword.other
-- ^^ meta.declaration.exports
--   ^ -meta.declaration.exports
-- ^^^^^^^^ meta.declaration.module
--         ^ -meta.declaration.module

import Prelude hiding (Traversable(traverse, sequenceA))
-- ^^^ keyword.other
--             ^^^^^^ keyword.other
--     ^^^^^^^ support.other.module
--                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.imports
-- ^^^^^^^^^^^^^^^^^^^ -meta.declaration.import
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.import
--                                                      ^ -meta.import -meta.declaration.imports

import System.Exit (ExitCode(..), exitWith)
import qualified Data.String

data DeriveMoreThanOne = MkDeriveMoreThanOne deriving (Eq, Read, Show)
-- ^ keyword.other
--   ^^^^^^^^^^^^^^^^^ storage.type
--                     ^ keyword.operator
--                                           ^^^^^^^^ keyword.other
--                                                     ^^ entity.other.inherited-class
--                                                         ^^^^ entity.other.inherited-class
--                                                               ^^^^ entity.other.inherited-class


----
-- top-level definitions are names
----
toUnit :: DeriveMoreThanOne -> ()
-- ^^^ entity.name.function
--     ^^ keyword.operator
--        ^^^^^^^^^^^^^^^^^ storage.type
--                          ^^ keyword.operator
toUnit _ = ()
-- ^^^ -entity.name.function
--       ^ keyword.operator


----
-- operators with question marks
----
(!!?) :: a -> b -> DeriveMoreThanOne
-- ^ entity.name.function
--  ^ -entity.name.function
_ !!? _ = MkDeriveMoreThanOne
-- ^^ keyword.operator
--   ^ -keyword.operator


----
-- operators starting with `--`
----
(-->) :: (a -> b) -> (b -> c) -> a -> c
-- ^ entity.name.function
--  ^ -entity.name.function
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ -comment
f --> g = g . f
-- ^^ keyword.operator
--   ^ -keyword.operator
-- ^^^^^^^^^^^^^ -comment


----
-- operators that end with '--'
----
(<--) :: (b -> c) -> (a -> b) -> a -> c
-- ^ entity.name.function
--  ^ -entity.name.function
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ -comment
f <-- g = f . g
-- ^^ keyword.operator
--   ^ -keyword.operator
-- ^^^^^^^^^^^^^ -comment


----
-- comments in class and instance declarations
----
class SomeClass a -- some comment
-- ^^ keyword.other
--    ^^^^^^^^^ entity.other.inherited-class
--                ^^^^^^^^^^^^^^^ comment.line
instance SomeClass a -- some comment
-- ^^^^^ keyword.other
--       ^^^^^^^^^ entity.other.inherited-class
--                   ^^^^^^^^^^^^^^^ comment.line


----
-- classes can have numbers and primes in their names
----
class Class0 a
--    ^^^^^^ entity.other.inherited-class
--          ^ -entity.other.inherited-class
instance Class0 Int
--       ^^^^^^ entity.other.inherited-class
--             ^ -entity.other.inherited-class

class Class' a
--    ^^^^^^ entity.other.inherited-class
--          ^ -entity.other.inherited-class
instance Class' Int
--       ^^^^^^ entity.other.inherited-class
--             ^ -entity.other.inherited-class


----
-- class constraints and contexts
----
class (Class0 a, Class' a) => Class0' a
-- ^^^^ -entity.other.inherited-class
--     ^^^^^^ entity.other.inherited-class
--           ^^^^ -entity.other.inherited-class
--               ^^^^^^ entity.other.inherited-class
--                     ^^^^^^^ -entity.other.inherited-class
--                            ^^^^^^^ entity.other.inherited-class
--                                   ^^^ -entity.other.inherited-class
-- ^^^^^^^^^^^^^^^^^^^^^^^^ -keyword.operator
--                         ^^ keyword.operator
--                           ^^^^^^^^^^^ -keyword.operator
-- ^^ keyword.other
--   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ -keyword.other
instance Class0 a => Class0 (Maybe a)
-- ^^^^^ keyword.other
--      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ -keyword.other
-- ^^^^^^ -entity.other.inherited-class
--       ^^^^^^ entity.other.inherited-class
--             ^^^^^^ -entity.other.inherited-class
--                   ^^^^^^ entity.other.inherited-class
--                         ^ -entity.other.inherited-class
-- ^^^^^^^^^^^^^^^ -keyword.operator
--                ^^ keyword.operator
--                  ^^^^^^^^^^^^^^^^^^ -keyword.operator


----
-- functions with primes in their names
----
someFunc' :: Char -> Int -> String
-- ^^^^^^ entity.name.function
--       ^ -entity.name.function
someFunc' char int = replicate int char
-- ^^^^^^ -entity.name.function


----
-- types with primes in their names
----
newtype Foo' = Foo' { unFoo' :: Foo }
-- ^^^^ keyword.other
--     ^ -keyword.other
--      ^^^^ storage.type
--          ^ -storage.type


----
-- operators, guards
----
(++:) :: Enum a => Int -> a -> a
-- ^ entity.name.function
--  ^ -entity.name.function
--             ^ -keyword.operator
--              ^^ keyword.operator
--                ^ -keyword.operator
n ++: x
-- ^^ keyword.operator
--   ^ -keyword.operator
    | n <= 0    = x
-- ^ -keyword.operator
--  ^ keyword.operator
--   ^ -keyword.operator
--             ^ -keyword.operator
--              ^ keyword.operator
--               ^ -keyword.operator
    | otherwise = (n - 1) ++: succ x
-- ^ -keyword.operator
--  ^ keyword.operator
--   ^ -keyword.operatorx
--             ^ -keyword.operator
--              ^ keyword.operator
--               ^ -keyword.operator
--                       ^ -keyword.operator
--                        ^^^ keyword.operator
--                           ^ -keyword.operator


----
-- infix functions, operator sections, list literals, char literals, where blocks
----
someList :: [Char]
someList = (5 ++:) `fmap1'` ['a', 'b', 'c']
--            ^^^ keyword.operator
--               ^^ -keyword
--                 ^^^^^^^^ keyword.operator
--                         ^^ -keyword
--                           ^ punctuation.definition.string.begin
--                            ^ string
--                             ^ punctuation.definition.string.end
  where
-- ^^^^ keyword.other
--     ^ -keyword.other
    fmap1' :: (Char -> Char) -> [Char] -> [Char]
-- ^ -entity
--  ^^^^^^ entity.name.function
--        ^ -keyword -entity
--         ^^ keyword.operator
--           ^ -keyword
    fmap1' = fmap
-- ^^^^^^^^^^^^^^^ -entity
--         ^ keyword.operator


----
-- `do` block
----
main :: IO ()
main = do
-- ^^ -entity -keyword
--   ^ keyword.operator
--    ^ -keyword
--     ^^ keyword.control
    print (tryLet "this")
    let x = 1 + 1
-- ^ -keyword
--  ^^^ keyword.other
--     ^ -keyword
    let y = (+) 2 2
-- ^ -keyword
--  ^^^ keyword.other
--     ^ -keyword
    z <- pure 3
--   ^ -keyword
--    ^^ keyword.operator
--      ^ -keyword
    print (addEmUp x y z)
    putStrLn (str myFoo)
    exitWith ExitSuccess


----
-- `let` block, and strings
----
tryLet :: String -> String
tryLet this =
    let that = this ++ " that"
-- ^ -keyword
--  ^^^ keyword.other
--     ^ -keyword
--                 ^ -keyword
--                  ^^ keyword.operator
--                    ^ -keyword -string
--                     ^^^^^^^ string.quoted.double
--                            ^ -string
        those = that ++ " those"
--                  ^ -keyword
--                   ^^ keyword.operator
--                     ^ -keyword -string
--                      ^^^^^^^^ string.quoted.double
--                              ^ -string
        these = those ++ " these"
--                   ^ -keyword
--                    ^^ keyword.operator
--                      ^ -keyword -string
--                       ^^^^^^^^ string.quoted.double
--                               ^ -string
    in these
-- ^ -keyword
--  ^^ keyword.other
--    ^ -keyword


----
-- function declaration with signature on next line
----
addEmUp
-- ^^^^ entity.name.function
--     ^ -entity
    :: Int -> Int -> Int -> Int
-- ^ -keyword
--  ^^ keyword.operator
--    ^ -keyword
addEmUp x y z = x + y + z
-- ^^^^^^^^^^^^^^^^^^^^^^ -entity


----
-- record accessors are names
----
data Foo = Foo
    { str :: String
--   ^ -entity
--    ^^^ entity.name.function
--       ^ -entity -keyword
--        ^^ keyword.operator
--          ^ -keyword
    , int :: Int
--   ^ -entity
--    ^^^ entity.name.function
--       ^ -entity -keyword
--        ^^ keyword.operator
--          ^ -keyword
    } deriving Show

emptyFoo :: Foo
emptyFoo = Foo "" 0


----
-- class methods are names
----
class Fooable a where
    toFoo   :: a -> Foo
-- ^ -entity
--  ^^^^^ entity.name.function
--       ^^^ -entity -keyword
--          ^^ keyword.operator
--            ^ -keyword
    fromFoo :: Foo -> a
-- ^ -entity
--  ^^^^^^^ entity.name.function
--         ^ -entity -keyword
--          ^^ keyword.operator
--            ^ -keyword
    (<=>)   :: Foo -> a -> Bool
--  ^ -entity
--   ^^^ entity.name.function
--      ^^^^ -entity -function
--          ^^ keyword.operator
--            ^ -keyword


----
-- `&&` is an operator
----
instance Fooable Foo where
    toFoo   = id
    fromFoo = id
    x <=> y = str x == str y && int x == int y
--                          ^ -keyword
--                           ^^ keyword.operator
--                             ^ -keyword


----
-- record update syntax
---
instance Fooable String where
    toFoo x = emptyFoo { str = x }
--           ^^^^^^^^^^^^^^^^^^^^^^ -entity
--                          ^ -keyword
--                           ^ keyword.operator
--                            ^ -keyword
    fromFoo = str
    x <=> y = str x == y

instance Fooable Int where
    toFoo x = emptyFoo { int = x }
--           ^^^^^^^^^^^^^^^^^^^^^^ -entity
--                          ^ -keyword
--                           ^ keyword.operator
--                            ^ -keyword
    fromFoo = int
    x <=> y = int x == y


----
-- classes and instances with no methods
----
class Show a => Foolike a
-- ^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.class
--                       ^ -meta.declaration.class

instance Foolike Foo
-- ^^^^^^^^^^^^^^^^^ meta.declaration.class
--                  ^ -meta.declaration.class
instance Foolike a => Foolike [a]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.class
--                               ^ -meta.declaration.class


----
-- types operators
----
type a :*: b = (a, Bool -> b)
-- ^ keyword.other
--  ^^^ -keyword
--     ^^^ keyword.operator
--        ^^^ -keyword
--           ^ keyword.operator
--            ^^^^^^^^^^ -keyword
--                      ^^ keyword.operator
--                        ^ -keyword

instance (Show (x -> a), Foolike a) => Foolike (x -> a)
--               ^ -keyword
--                ^^ keyword.operator
--                  ^^^^^^^^^^^^^^^^ -keyword
--                                  ^^ keyword.operator
--                                    ^^^^^^^^^^^^ -keyword
--                                                ^^ keyword.operator
--                                                  ^ -keyword
instance (Show (Bool -> b), Foolike a, Foolike b) => Foolike (a :*: b)
--                  ^ -keyword
--                   ^^ keyword.operator
--                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^ -keyword
--                                                ^^ keyword.operator
--                                                  ^^^^^^^^^^^^ -keyword
--                                                              ^^^ keyword.operator
--                                                                 ^ -keyword


----
-- record syntax
----
myFoo :: Foo
myFoo = Foo
    { str = multiLineString :: String
-- ^^^^^^^^^ -entity
--       ^ -keyword
--        ^ keyword.operator
--         ^ -keyword
    , int = 37
-- ^^^^^^^^^^^^ -entity
--       ^ -keyword
--        ^ keyword.operator
--         ^ -keyword
    }

-- where (this is to close the 'Foolike' declaration)


----
-- multiline strings
----
multiLineString :: Data.String.IsString a => a
multiLineString =
    "here\
    \is\
    \a\
    \multiline\
    \string"
-- ^^^^^^^^^ string.quoted.double
--  ^ constant.character.escape.multi-line
--          ^ -string


----
-- records written on one line
----
newtype OneLine a = OneLine { unOneLine :: a }
-- ^^^^ keyword.other
--     ^ -keyword -storage
--      ^^^^^^^ storage.type
--             ^^^ -keyword -storage
--                ^ keyword.operator
--                 ^^^^^^^^^^^ -keyword -entity
--                            ^^^^^^^^^ entity.name.function
--                                     ^ -keyword -entity
--                                      ^^ keyword.operator
--                                        ^ -keyword


----
-- classes written on one line
----
class OneLiner a where oneLiner :: a -> OneLine a
-- ^^ keyword.other
--   ^ -keyword -entity
--    ^^^^^^^^ entity.other.inherited-class
--            ^^^ -entity -keyword
--               ^^^^^ keyword.other
--                    ^ -keyword -entity
--                     ^^^^^^^^ entity.name.function
--                             ^ -entity -keyword
--                              ^^ keyword.operator
--                                ^ -keyword


----
-- original tests
----
x0 = 23*36 -- single line comment
--         ^^ punctuation.definition.comment
--         ^^^^^^^^^^^^^^^^^^^^^^ comment.line
x1 = 23*36
-- <- - comment.line

x2 = {- block comment -} 23*36
--   ^^ punctuation.definition.comment.begin
--   ^^^^^^^^^^^^^^^^^^^ comment.block
--                    ^^ punctuation.definition.comment.end
--                      ^ -comment.block

x3 = {- {-# #-} -} 23*36
--   ^^ punctuation.definition.comment.begin
--   ^^^^^^^^^^^^^ comment.block -meta.preprocessor
--              ^^ punctuation.definition.comment.end
--                ^ -comment.block

x4 = {- {- #-} -} 23*36
--   ^^ punctuation.definition.comment.begin
--   ^^^^^^^^^^^^ comment.block
--             ^^ punctuation.definition.comment.end
--               ^ -comment.block

x5 = {- {- -} -} 23*36
--   ^^ punctuation.definition.comment.begin
--   ^^^^^^^^^^^ comment.block
--            ^^ punctuation.definition.comment.end
--              ^ -comment.block

x6 = {- {-# -} -} 23*36
--   ^^ punctuation.definition.comment.begin
--   ^^^^^^^^^^^^ comment.block -meta.preprocessor
--             ^^ punctuation.definition.comment.end
--               ^ -comment.block

x7 = {- {-# {- test -} -} -} 23*36
--   ^^ punctuation.definition.comment.begin
--   ^^^^^^^^^^^^^^^^^^^^^^^ comment.block -meta.preprocessor
--                        ^^ punctuation.definition.comment.end
--                          ^ -comment.block

class (Functor t, Foldable t) => Traversable t where
-- ^^ keyword.other
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.class
    {-# MINIMAL traverse | sequenceA #-}
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.preprocessor
--                                      ^ -meta.preprocessor

    -- | Map each element of a structure to an action,
    -- evaluate these actions from left to right, and
    -- collect the results. For a version that ignores
    -- the results see 'Data.Foldable.traverse_'.
--  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.haskell
    traverse :: Applicative f =>
--  ^^^^^^^^ entity.name.function
--           ^^ keyword.operator
--              ^^^^^^^^^^^ storage.type
--                            ^^ keyword.operator
        (a -> f b)
--         ^^ keyword.operator
        -> t a
--      ^^ keyword.operator
        -> f (t b)
--      ^^ keyword.operator
    traverse f = sequenceA . fmap f
--             ^ keyword.operator
--                         ^ keyword.operator

    -- | Evaluate each action in the structure from
    -- left to right, and collect the results.
    -- For a version that ignores the results see
    -- 'Data.Foldable.sequenceA_'.
    sequenceA ∷ Applicative f ⇒ t (f a) → f (t a)
--  ^^^^^^^^^ entity.name.function.haskell
--            ^ keyword.operator
--              ^^^^^^^^^^^ storage.type
--                            ^ keyword.operator
--                                      ^ keyword.operator
    sequenceA = traverse id
--            ^ keyword.operator.haskell
