import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Text as T
import Parser (parse)
import Runtime (Runtime (multiset), emptyRuntime, run, zipper, unzipper)
import Core (Tree, RValue, rebranch)
import qualified Multiset as MS
import Trie
import Data.Maybe
import Invariants


parseOrDie :: String -> T.Text -> IO [Tree RValue]
parseOrDie fp prog = either (fail . show) pure $ parse (T.unpack prog) fp

newRuntime :: [Tree RValue] -> Runtime
newRuntime = emptyRuntime "unittest" False

runProg :: String -> T.Text -> IO Runtime
runProg fp prog = do
    rvals <- parseOrDie fp prog
    run $ newRuntime rvals

-- | shouldBecome also tacks on a rule to remove `(defined ...)` compiler output
shouldBecome :: HasCallStack => T.Text -> T.Text -> Expectation
shouldBecome tree transformed = do
    tree' <- runProg "test input" $ "(defined :x ~>)" `T.append` tree
    trans <- parseOrDie "test assertion" transformed
    (unzipper . Runtime.zipper $ tree') `shouldBe` trans

shouldBecomeWithBag :: HasCallStack => T.Text -> T.Text -> [(T.Text, Int)] -> Expectation
shouldBecomeWithBag tree transformed bag = do
    runtime <- runProg "test input" $ "(defined :x ~>)" `T.append` tree
    trans <- parseOrDie "test assertion" transformed
    bagTrees <- mapM (parseOrDie "bag" . fst) bag
    let bagCounts = snd <$> bag
    let bag' = MS.fromList $ zip (rebranch <$> bagTrees) bagCounts
    (unzipper . Runtime.zipper $ runtime) `shouldBe` trans
    Runtime.multiset runtime `shouldBe` bag'


main :: IO ()
main = hspec $ do
    describe "tree definition" $ do
        it "handles basic definitions" $ do
            "(hello ~> world) hello" `shouldBecome` "world"
        it "handles one time definitions" $ do
            "(hello ~ world) hello hello hello" `shouldBecome` "world hello hello"
        it "handles multiple terms on LHS" $ do
            "((1 1) ~ world) 1 1 (1 1) 1" `shouldBecome` "1 1 world 1"
            "(1 1 ~ world) 1 (1 1)" `shouldBecome` "1 world"
        it "handles multiple terms on the RHS" $ do
            "(0 ~> 1 2 3) 0" `shouldBecome` "1 2 3"
        it "handles handles empty condition" $ do
            pendingWith "bugfix TODO"
            "(~ 1 2 3)" `shouldBecome` "1 2 3"
        it "handles pattern variables" $ do
            "(hello :a ~> (goodbye :a)) (hello world)" `shouldBecome` "(goodbye world)"
    describe "bag definition" $ do
        it "supports bag builtin" $ do
            "(@ bag)" `shouldBecome` "()"
        it "handles bag push" $ do
            "(| pushed) (@ bag)" `shouldBecome` "((pushed 1))"
        it "handles bag multiple push" $ do 
            "(| pushed (pushy push) pushed) ()" `shouldBecomeWithBag` "()" $ [("pushed", 2), ("(pushy push)", 1)]
        it "handles bag pop" $ do
            "(| pushed) (pushed |) (@ bag)" `shouldBecome` "()"
            "(pushed |) (| pushed) (@ bag)" `shouldBecome` "()"
        it "handles a mess" $ do
            "(| apple pear orange peach) (apple pear |) (pear orange |) (peach |) ()" `shouldBecomeWithBag` "()" $ [("orange", 1)]

        it "handles permanent bag rules" $ do
            "(item |> peepus) (| item) (| item) (| item) (@ bag)" `shouldBecome` "((peepus 3))"
        it "handles variables in bag conditions" $ do
            pendingWith "feature TODO"
            "(| (my item)) ((my :x) | (your :x)) (@ bag)" `shouldBecome` "(((your item) 1))"
    describe "mixed definition" $ do
        it "applies with bag rule first" $ do
            "(marble | bagged & tree ~ slurped) (| marble) tree (@ bag)" `shouldBecome` "slurped ((bagged 1))"
        it "applies with tree rule first" $ do
            "(tree ~ slurped & marble | bagged) (| marble) tree (@ bag)" `shouldBecome` "slurped ((bagged 1))"
        it "apply once if both conditions apply once" $ do
            "(tree ~ slurped & marble | bagged) (| marble marble) tree tree" `shouldBecomeWithBag` "slurped tree" $ [("marble", 1), ("bagged", 1)]
        it "apply many times takes precedence" $ do
            "(tree ~ slurped & marble |> bagged) (| marble marble) tree tree (@ bag)" `shouldBecome` "slurped slurped ((bagged 2))"
            "(tree ~> slurped & marble | bagged) (| marble marble) tree tree (@ bag)" `shouldBecome` "slurped slurped ((bagged 2))"
        it "handles bindings from tree condition to pocket effect" $ do
            "((grab :x) ~> (grabbed :x) & | :x) (grab pipe) (@ bag)" `shouldBecome` "(grabbed pipe) ((pipe 1))"
    describe "precedence and eagerness" $ do
        it "respects declaration order" $ do
            "(hello ~ world) hello hello" `shouldBecome` "world hello"
            "hello (hello ~ world) hello" `shouldBecome` "hello world"
            "hello hello (hello ~ world)" `shouldBecome` "world hello"
        it "respects recency precedence order within many-time rules" $ do 
            "(a ~> c) (a ~> b) a" `shouldBecome` "b"
            "(a ~> b) (a ~> c) a" `shouldBecome` "c"
        it "respects recency precedence order within one-time rules" $ do 
            "(a ~ c) (a ~ b) a" `shouldBecome` "b"
            "(a ~ b) (a ~ c) a" `shouldBecome` "c"
        it "places higher precedence on one-time rules" $ do 
            "(a ~ once) (a ~> many) a" `shouldBecome` "once"
            "(a ~> many) (a ~ once) a" `shouldBecome` "once"
        it "places higher precedence on rules with lower depth" $ do 
            "(top :x ~ topped) (mid :x ~ midded) (top (mid (bottom)))" `shouldBecome` "topped"
            "(mid :x ~ midded) (top :x ~ topped) (top (mid (bottom)))" `shouldBecome` "topped"
        it "eagerness overrides depth precedence" $ do 
            "(mid :!x ~ midded) (top (mid (bottom))) (top :!x ~ topped) (top (eat))" `shouldBecome` "(top midded) topped"
            "(top :!x ~ topped) (mid :!x ~ midded) (top (mid (bottom))) (top eat)" `shouldBecome` "(top midded) topped"
            "(mid :!x ~ midded) (top :!x ~ topped) (top (mid (bottom))) (top eat)" `shouldBecome` "(top midded) topped"
        it "handles precedence on rules with the same depth " $ do
            "((grab :x) ~> (grabbed :x) & | :x) ((grab goose) ~> (grab duck)) (grab goose)" `shouldBecomeWithBag` "(grabbed duck)" $ [("duck", 1)]
            "((grab goose) ~> (grab duck)) ((grab :x) ~> (grabbed :x) & | :x) (grab goose)" `shouldBecomeWithBag` "(grabbed goose)" $ [("goose", 1)]
        it "handles noneager variables matching lazily" $ do
            "((grab :x) ~> (grabbed :x) & | :x) (goose ~> duck) (grab goose)" `shouldBecomeWithBag` "(grabbed duck)" $ [("goose", 1)]
            "(goose ~> duck) ((grab :x) ~> (grabbed :x) & | :x) (grab goose)" `shouldBecomeWithBag` "(grabbed duck)" $ [("goose", 1)]
        it "handles eager variables" $ do
            "((grab :!x) ~> (grabbed :x) & | :x) (goose ~> duck) (grab goose)" `shouldBecomeWithBag` "(grabbed duck)" $ [("duck", 1)]
            "(goose ~> duck) ((grab :!x) ~> (grabbed :x) & | :x) (grab goose)" `shouldBecomeWithBag` "(grabbed duck)" $ [("duck", 1)]
    describe "regex" $ do
        it "supports group matching" $ do
            "(/^(regex) (group) (test)!$/ ~> \"$3 $1 $2 $0\" ($1)) \"regex group test!\"" `shouldBecome` "\"test regex group regex group test!\" (\"regex\")"
        it "supports anchors" $ do
            "(/^H\\w..o!$/ ~> \"elo!\") \"... Hello!...\" \"Hello!\"" `shouldBecome` "\"... Hello!...\" \"elo!\""
        it "supports pre and post match groups" $ do
            "(/(floating) regex/ ~> \"$> $1 $<\") \"this should match my floating regex!\"" `shouldBecome` "\"! floating this should match my \""
    describe "trie property tests" $ do
        prop "chars can be added to tree" (addOnTrieSupportsElemCheck :: TrieWithElemPred SimpleChar [Int])
        prop "units can be added to tree" (addOnTrieSupportsElemCheck :: TrieWithElemPred () [Int])
        prop "toTrie . fromTrie on chars" (toTrieInverseIsFromTrie :: Trie [SimpleChar] [()] -> Bool)
        prop "toTrie . fromTrie on units" (toTrieInverseIsFromTrie :: Trie () [()] -> Bool)
        prop "fromTrie . toTrie on chars" (fromTrieInverseIsToTrie :: [([SimpleChar], ())] -> Bool)
        prop "fromTrie . toTrie on units" (fromTrieInverseIsToTrie :: [([()], ())] -> Bool)
