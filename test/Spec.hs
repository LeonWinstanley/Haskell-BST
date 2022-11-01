import Test.Tasty.QuickCheck as QC ( testProperty )
import Test.Tasty.HUnit ( assertBool, testCaseSteps )
import Lib
    ( treeRemove,
      fetchEntries,
      treeLookup,
      treeInsert,
      Item(Item, NotFound),
      Tree(Leaf, Node),
      treeRemoveIf )
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Data.List ( nub, sort )

{-- Utility Functions - START --}

exampleTree :: Tree Int String
exampleTree =
    treeInsert 22 "Alex" (treeInsert 20 "Adam" (treeInsert 50 "Mary" (treeInsert 30 "Leon" Leaf)))
    
largeExampleTree :: Tree Int String
largeExampleTree = Node 10 "Jane"
                      (Node 8 "Mike"
                        (Node 5 "Lewis"
                          (Node 2 "Harry" Leaf Leaf) Leaf)
                        (Node 9 "Neil" Leaf Leaf))
                      (Node 15 "Leon"
                        (Node 12 "Adam" Leaf Leaf)
                        (Node 20 "Alex" Leaf Leaf))

{-- Utility Functions - END --}


{-- Unit tests for lookup function - START --}
lookupTest :: TestTree
lookupTest = testCaseSteps "Lookup steps" $ \step -> do
  step "Preparing..."
  let tree = Node 10 "Alex"
              (Node 5 "Adam"
                (Node 2 "Leon" Leaf Leaf)
              Leaf)
              (Node 15 "Neil"
                (Node 13 "Mike" Leaf Leaf)
              Leaf)

  step "Running Part 1"
  assertBool "lookup NotFound == NotFound" (treeLookup 12 tree == NotFound)

  step "Running Part 2"
  assertBool "lookup item == item (Left - Left)" (treeLookup 2 tree == Item "Leon")

  step "Running Part 3"
  assertBool "lookup item == item (Right)" (treeLookup 15 tree == Item "Neil")

  step "Running Part 4"
  assertBool "lookup item == item (Root)" (treeLookup 10 tree == Item "Alex")

  step "Running Part 5"
  assertBool "lookup item == item (Right - Left)" (treeLookup 13 tree == Item "Mike")
{-- Unit tests for lookup function - END --}

{-- Property-based tests for insert function - START --}

insertProps :: TestTree
insertProps = testGroup "Prop Tests"
  [ QC.testProperty "multiple insert node /= leaf" prop_multipleInsertNotLeaf
  , QC.testProperty "multiple insert lookup node" prop_mutlipleInsertLookUpNode
  , QC.testProperty "multiple insert replace item" prop_multipleInsertReplace
  , QC.testProperty "multiple insert list entries" prop_multipleInsertListEntries
  , QC.testProperty "multiple remove lookup" prop_multipleRemoveLookup
  , QC.testProperty "multiple remove Insert lookup" prop_multipleInsertRemoveLookup
  ]

prop_multipleInsertNotLeaf :: Int -> String -> Bool
prop_multipleInsertNotLeaf insertKey insertString = treeInsert insertKey insertString Leaf /= Leaf

prop_mutlipleInsertLookUpNode :: Int -> String -> Bool
prop_mutlipleInsertLookUpNode insertKey insertString =
  treeLookup insertKey (treeInsert insertKey insertString exampleTree) == Item insertString

prop_multipleInsertReplace :: Int -> String -> String -> Bool
prop_multipleInsertReplace insertKey insertString insertReplaceString =
  treeLookup insertKey ( treeInsert insertKey insertReplaceString (treeInsert insertKey insertString Leaf))
  == Item insertReplaceString

{-- Property-based tests for insert function - END --}

{-- Unit tests for insert function - START --}

insertUnitTests :: TestTree
insertUnitTests = testGroup "Insert Unit Tests"
  [
    insertReplaceTest
  ]

insertReplaceTest :: TestTree
insertReplaceTest = testCaseSteps "insert replace steps" $ \step -> do
  step "Preparing..."
  let tree = treeInsert 10 "Jane" (treeInsert 10 "Alex" (Node 20 "Leon" Leaf Leaf))

  step "Running Part 1"
  assertBool "insert replace ==" (treeLookup 10 tree == Item "Jane")

  step "Running Part 2"
  assertBool "insert replace /=" (treeLookup 10 tree /= Item "Alex")

  step "Running Part 3"
  assertBool "tree structure exists" ((treeLookup 10 tree == Item "Jane") && (treeLookup 20 tree == Item "Leon"))


{-- Unit tests for insert function - END --}

{-- Unit tests for list entries function - START --}

listEntriesTest = testCaseSteps "list entries steps" $ \step -> do
  step "Preparing..."
  let tree = Node 10 "Alex"
              (Node 5 "Adam"
                (Node 2 "Leon" Leaf Leaf)
              Leaf)
              (Node 15 "Neil"
                (Node 13 "Mike" Leaf Leaf)
              Leaf)

  step "Running test 1"
  assertBool "Entries same as Leaf" (null (fetchEntries (Leaf :: Tree[Int][Char])))

  step "Running test 2"
  assertBool "Entries same as tree" (fetchEntries tree == [2,5,10,13,15])

prop_multipleInsertListEntries :: Int -> Int -> Int -> String -> String -> String -> Bool
prop_multipleInsertListEntries a b c a' b' c' = let tree = treeInsert a a' (treeInsert b b' (treeInsert c c' Leaf))
  in (fetchEntries tree == sort(nub[a,b,c]))

{-- Unit tests for list entries function - END --}

{-- Unit tests for remove function - START --}

prop_multipleRemoveLookup :: Int -> String -> Bool 
prop_multipleRemoveLookup key item = let tree = treeRemove key largeExampleTree
  in (treeLookup key tree /= Item item)

prop_multipleInsertRemoveLookup :: [Int] -> [String] -> Bool 
prop_multipleInsertRemoveLookup keys items = let tree = treeRemove (head keys)(multipleInsertRemoveLookupHelper keys items (Leaf :: Tree Int String))                                     
  in (treeLookup (head keys) tree /= Item (head items))

multipleInsertRemoveLookupHelper :: (Ord k) => [k] -> [i] -> Tree k i -> Tree k i
multipleInsertRemoveLookupHelper _ [] tree = tree
multipleInsertRemoveLookupHelper [] _ tree = tree
multipleInsertRemoveLookupHelper (keysHead:keysTail) (itemsHead:itemsTail) tree = multipleInsertRemoveLookupHelper keysTail itemsTail (treeInsert keysHead itemsHead tree)

removeTest :: TestTree
removeTest = testCaseSteps "remove steps" $ \step -> do
  step "Preparing..."
  let emptyTree = Leaf :: Tree Int [Char]
  let leftNodeTree = Node 10 "Jane" (Node 12 "Mike" Leaf Leaf) Leaf
  let rightNodeTree = Node 10 "Jane" Leaf (Node 12 "Mike" Leaf Leaf)
  let largeTree = Node 10 "Jane"
                    (Node 8 "Mike" Leaf
                      (Node 9 "Mary" Leaf Leaf))
                    (Node 13 "Lewis"
                      (Node 12 "Leon" Leaf Leaf) Leaf)
  let largeTreeLeftNodeRemove = treeRemove 13 largeTree
  let largeTreeRightNodeRemove = treeRemove 8 largeTree
  let doubleNodeTree = Node 10 "Jane" (Node 5 "Mike" Leaf Leaf) (Node 15 "Lewis" Leaf Leaf)
  let doubleNodeTreeRemoveNode = treeRemove 10 doubleNodeTree
  let completeTree = Node 10 "Jane"
                      (Node 8 "Mike"
                        (Node 5 "Lewis"
                          (Node 2 "Harry" Leaf Leaf) Leaf)
                        (Node 9 "Neil" Leaf Leaf))
                      (Node 15 "Leon"
                        (Node 12 "Adam" Leaf Leaf)
                        (Node 20 "Alex" Leaf Leaf))
  let completeTreeRemoveMike = treeRemove 8 completeTree

  step "Running test 1"
  assertBool "remove leaf = leaf" (treeRemove 13 emptyTree == Leaf)

  step "Running test 2"
  assertBool "remove node (leaf = leaf)" (treeRemove 13 (Node 13 "Jane" Leaf Leaf) == Leaf)

  step "Running test 3"
  assertBool "remove (Node key item Node Leaf)" (treeRemove 10 leftNodeTree
    == Node 12 "Mike" Leaf Leaf)

  step "Running test 4"
  assertBool "remove (Node key item Leaf Node)" (treeRemove 10 rightNodeTree
    == Node 12 "Mike" Leaf Leaf)

  step "Running test 5"
  assertBool "remove (Node key item Node Leaf) keep structure" (
    (treeLookup 10 largeTreeLeftNodeRemove == Item "Jane") &&
    (treeLookup 8 largeTreeLeftNodeRemove == Item "Mike") &&
    (treeLookup 9 largeTreeLeftNodeRemove == Item "Mary") &&
    (treeLookup 13 largeTreeLeftNodeRemove == NotFound) &&
    (treeLookup 12 largeTreeLeftNodeRemove == Item "Leon"))

  step "Running test 6"
  assertBool "remove (Node key item Node Leaf) keep structure" (
    (treeLookup 10 largeTreeRightNodeRemove == Item "Jane") &&
    (treeLookup 8 largeTreeRightNodeRemove == NotFound) &&
    (treeLookup 9 largeTreeRightNodeRemove == Item "Mary") &&
    (treeLookup 13 largeTreeRightNodeRemove == Item "Lewis") &&
    (treeLookup 12 largeTreeRightNodeRemove == Item "Leon"))

  step "Running test 7"
  assertBool "remove (Node key item Node Node) Root keep structure" (
    (treeLookup 10 doubleNodeTreeRemoveNode == NotFound) &&
    (treeLookup 5 doubleNodeTreeRemoveNode == Item "Mike") &&
    (treeLookup 15 doubleNodeTreeRemoveNode == Item "Lewis"))

  step "Running test 8"
  assertBool "remove (Node key item Node Node) middle tree keep structure"
    ((treeLookup 10 completeTreeRemoveMike == Item "Jane") &&
    (treeLookup 8 completeTreeRemoveMike == NotFound) &&
    (treeLookup 5 completeTreeRemoveMike == Item "Lewis") &&
    (treeLookup 2 completeTreeRemoveMike == Item "Harry") &&
    (treeLookup 9 completeTreeRemoveMike == Item "Neil") &&
    (treeLookup 15 completeTreeRemoveMike == Item "Leon") &&
    (treeLookup 12 completeTreeRemoveMike == Item "Adam") &&
    (treeLookup 20 completeTreeRemoveMike == Item "Alex"))

removeIfSteps :: TestTree
removeIfSteps = testCaseSteps "Remove If Steps" $ \step -> do
  step "Preparing..."
  let tree = Node 10 "Jane"
              (Node 8 "Mike"
                (Node 5 "Lewis"
                  (Node 2 "Harry" Leaf Leaf) Leaf)
                (Node 9 "Neil" Leaf Leaf))
              (Node 15 "Leon"
                (Node 12 "Adam" Leaf Leaf)
                (Node 20 "Alex" Leaf Leaf))
      treeRemoveEven = Node 15 "Leon" (Node 9 "Neil" (Node 5 "Lewis" Leaf Leaf) Leaf ) Leaf
      treeRemoveOdd = Node 10 "Jane" (Node 8 "Mike" (Node 2 "Harry" Leaf Leaf) Leaf) (Node 20 "Alex" (Node 12 "Adam" Leaf Leaf) Leaf)

  step "Running test 1"
  assertBool "Nothing removed" (treeRemoveIf (const False) tree == tree)

  step "Running test 2"
  assertBool "All removed" (treeRemoveIf (const True) tree == Leaf)

  step "Running test 3"
  assertBool "Only remove even keys" (treeRemoveIf even tree == treeRemoveEven)

  step "Running test 4"
  assertBool "Only remove odd keys" (treeRemoveIf odd tree == treeRemoveOdd)

{-- Unit tests for remove function - END --}

insertTests :: TestTree
insertTests = testGroup "Insert Tests" [insertProps, insertReplaceTest]
lookupTests :: TestTree
lookupTests = testGroup "Lookup Tests" [lookupTest]
allTests :: TestTree
allTests = testGroup "All Tests" [insertProps,
  insertReplaceTest, lookupTest, removeTest, listEntriesTest, removeIfSteps]

main :: IO ()
main = defaultMain allTests
