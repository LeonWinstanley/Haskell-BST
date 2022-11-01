module Lib
    ( Tree (..),
    Item(..),
    treeInsert,
    treeLookup,
    displayEntries,
    fetchEntries,
    treeRemove,
    treeRemoveIf,
    treeMultiRemove
    ) where

data Tree k i = Leaf | Node {
  key :: k,
  item :: i,
  leftNode :: Tree k i,
  rightNode :: Tree k i
  }
  deriving (Eq, Show)
data Item a = Item a | NotFound deriving (Eq, Show)

treeLookup :: (Ord k) => k -> Tree k i -> Item i
treeLookup _ Leaf = NotFound
treeLookup soughtKey (Node key item leftNode rightNode)
  | key > soughtKey   = treeLookup soughtKey leftNode
  | key < soughtKey   = treeLookup soughtKey rightNode
  | otherwise         = Item item

treeInsert :: (Ord k) => k -> i -> Tree k i -> Tree k i
treeInsert key item Leaf = Node key item Leaf Leaf
treeInsert insertKey insertItem (Node currentKey currentItem leftNode rightNode)
  | insertKey < currentKey  = Node currentKey currentItem (treeInsert insertKey insertItem leftNode) rightNode
  | insertKey > currentKey  = Node currentKey currentItem leftNode (treeInsert insertKey insertItem rightNode)
  | otherwise               = Node insertKey insertItem leftNode rightNode

treeRemove :: (Ord k) => k -> Tree k i -> Tree k i
treeRemove _ Leaf = Leaf
treeRemove deleteKey (Node key item Leaf rightNode)
  | deleteKey == key = rightNode
treeRemove deleteKey (Node key item leftNode Leaf)
  | deleteKey == key = leftNode
treeRemove deleteKey (Node key item leftNode rightNode)
  | deleteKey < key = Node key item (treeRemove deleteKey leftNode) rightNode
  | deleteKey > key = Node key item leftNode (treeRemove deleteKey rightNode)
  | otherwise = 
    let Item key' = treeMinimum rightNode
        Item item' = treeLookup key' rightNode
    in Node key' item' leftNode (treeRemoveMinimum rightNode)

treeMinimum :: (Ord k) => Tree k i -> Item k
treeMinimum Leaf = NotFound
treeMinimum (Node key _ leftNode rightNode) =
    let leftmin = treeMinimum leftNode
        rightmin = treeMinimum rightNode
    in Item $ case leftmin of
        NotFound -> case rightmin of
            NotFound -> key
            Item rightkey -> min key rightkey
        Item leftkey -> case rightmin of
            NotFound -> min key leftkey
            Item rightkey -> min key (min leftkey rightkey)

treeRemoveMinimum :: (Ord k) => Tree k i -> Tree k i
treeRemoveMinimum Leaf = Leaf
treeRemoveMinimum (Node _ _ Leaf rightNode) = rightNode
treeRemoveMinimum (Node key item leftNode rightNode) 
  = Node key item (treeRemoveMinimum leftNode) rightNode

displayEntries :: (Ord k, Show k, Show i) => Tree k i -> IO()
displayEntries Leaf = putStrLn "Inserted tree is a leaf"
displayEntries tree =
  print (fetchEntriesAndItems tree)

fetchEntries :: (Ord k) => Tree k i -> [k]
fetchEntries Leaf = []
fetchEntries (Node key item leftNode rightNode) =
  fetchEntries leftNode ++ [key] ++ fetchEntries rightNode

fetchEntriesAndItems :: (Ord k) => Tree k i -> [(k,i)]
fetchEntriesAndItems Leaf = []
fetchEntriesAndItems (Node key item leftNode rightNode) =
  fetchEntriesAndItems leftNode ++ [(key, item)] ++ fetchEntriesAndItems rightNode

treeRemoveIf :: (Ord k) => (k -> Bool) -> Tree k i -> Tree k i
treeRemoveIf boolfunction tree = 
  do
    let keys = fetchEntries tree
    let keysToRemove = filter boolfunction keys
    treeMultiRemove keysToRemove tree

treeMultiRemove :: (Ord k) => [k] -> Tree k a -> Tree k a
treeMultiRemove [] tree = tree
treeMultiRemove (headList:tailList) tree = do
  treeMultiRemove tailList (treeRemove headList tree) 


    
