{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

import Data.String

-- SublimeHaskell:
--   doesn't handle 'do' block correctly,
--   'let' binding breaks subsequent lines,
--   subsequent top-level declaration breaks
main :: IO ()
main = do
    print (tryLet "this")
    let x = 1 + 1
    let y = (+) 2 2
    z <- pure 3
    print (add x y z)
    putStrLn (str myFoo)

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

-- ST3:
--   doesn't consider record fields to be definitions,
--   doesn't correctly highlight derived class unless surrounded by parentheses
-- SublimeHaskell:
--   doesn't correctly highlight record fields beyond the first
data Foo = Foo
    { str :: String
    , int :: Int
    } deriving Show

-- SublimeHaskell:
--   doens't correctly handle 'let' block,
--   subsequent top-level declaration breaks
tryLet :: String -> String
tryLet this =
    let that = this ++ " that"
        those = that ++ " those"
        these = those ++ " these"
    in these

emptyFoo :: Foo
emptyFoo = Foo "" 0

-- ST3:
--   incorrectly requires definition to follow signature,
--   methods beyond the first method are not highlighted correctly,
--   subsequent top-level declaration breaks
--   doesn't consider operators to be definitions
class Fooable a where
    toFoo   :: a -> Foo
    fromFoo :: Foo -> a
    (<=>)   :: Foo -> a -> Bool

-- ST3:
--   doesn't consider '&&' an operator
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

-- ST3 default:
--   incorectly requires classes to have a 'where' clause,
--   several subsequent top-level declarations break
class Show a => Foolike a

instance Foolike Foo

instance Foolike a => Foolike [a]

-- SimpleHaskell:
--   misinterprets type annotations as definitions
--   fixing this breaks record fields as definitions (see below)
--   fixing this breaks classes with method on same line (see below)
--   generally consider this break to be worth it, though pull requests welcomed
myFoo :: Foo
myFoo = Foo
    { str = multiLineString :: String
    , int = 37
    }

-- where (this is to close the 'Foolike' declaration)

-- ST3: doesn't recognize multi-line strings
-- SublimeHaskell: doesn't handle multi-line strings correctly
multiLineString :: IsString a => a
multiLineString =
    "here\
    \is\
    \a\
    \multiline\
    \string"

-- ST3: doesn't consider record fields to be definitions
data OneLine a = OneLine { unOneLine :: a }

-- ST3: doesn't consider class methods to be definitions when on same line
class OneLiner a where oneLiner :: a -> OneLine a

----
-- Check For Regressions
----

-- deriving more than one type class
data DeriveMoreThanOne = DeriveMoreThanOne deriving (Eq, Read, Show)

-- arrows
toUnit :: DeriveMoreThanOne -> ()
toUnit DeriveMoreThanOne = ()

-- question marks in identifiers
(!!?) :: DeriveMoreThanOne -> DeriveMoreThanOne -> DeriveMoreThanOne
_ !!? _ = DeriveMoreThanOne
