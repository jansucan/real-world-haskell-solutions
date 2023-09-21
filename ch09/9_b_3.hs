-- Take the predicates and combinators from "Gluing predicates together" on page
-- 224 and make them work with our new Info type.

{-- From examples/examples/ch09/BetterPredicate.hs modified according to the assignment --}
import Data.Time (UTCTime(..))
import System.Directory (Permissions(..))
import System.FilePath (takeExtension)


data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

type InfoF a = Info -> a


equalP :: (Eq a) => InfoF a -> a -> (InfoF Bool)
equalP f k = (\info -> f info == k)

equalP' :: (Eq a) => (InfoF a) -> a -> (InfoF Bool)
equalP' f k info = (f info == k)

liftP :: (a -> b -> Bool) -> (InfoF a) -> b -> (InfoF Bool)
liftP q f k info = f info `q` k

greaterP, lesserP :: (Ord a) => (InfoF a) -> a -> (InfoF Bool)
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: (InfoF Bool) -> (InfoF Bool) -> (InfoF Bool)
simpleAndP f g info = f info && g info

liftP2 :: (a -> b -> Bool) -> (InfoF a) -> (InfoF b) -> (InfoF Bool)
liftP2 q f g info = f info `q` g info

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> (InfoF a)
constP k _ = k

liftP' q f k info = f info `q` constP k info

liftPath :: (FilePath -> a) -> (InfoF a)
liftPath f info = f (infoPath info)

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (infoSize `greaterP` (Just 131072))
{-- End of code from examples --}

-- ghci> :l 9_b_3.hs
-- [1 of 1] Compiling Main             ( 9_b_3.hs, interpreted )
-- Ok, one module loaded.

-- ghci> infoA = Info "asdf.cpp" Nothing (Just 131072) Nothing
-- ghci> myTest2 infoA
-- False

-- ghci> infoB = Info "asdf.hs" Nothing (Just 131073) Nothing
-- ghci> myTest2 infoB
-- False

-- ghci> infoC = Info "asdf.cpp" Nothing (Just 131073) Nothing
-- ghci> myTest2 infoC
-- True
