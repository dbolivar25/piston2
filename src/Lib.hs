module Lib (
    compress,
    decompress,
) where

import Data.Bits (Bits (shiftL, shiftR), testBit, (.&.), (.|.))
import Data.Heap (deleteMin, fromList, insert, size)
import Data.Int (Int64)
import Text.Printf (printf)

-- Huffman Tree
data HuffmanNode = Leaf Int Char | Internal Int HuffmanNode HuffmanNode

instance Show HuffmanNode where
    show (Leaf f c) = printf "(Leaf %d %c)" f c
    show (Internal f l r) = printf "(Internal %d %s %s)" f (show l) (show r)

instance Eq HuffmanNode where
    (Leaf f1 _) == (Leaf f2 _) = f1 == f2
    (Internal f1 _ _) == (Internal f2 _ _) = f1 == f2
    _ == _ = False

instance Ord HuffmanNode where
    compare (Leaf f1 _) (Leaf f2 _) = compare f1 f2
    compare (Leaf f1 _) (Internal f2 _ _) = compare f1 f2
    compare (Internal f1 _ _) (Leaf f2 _) = compare f1 f2
    compare (Internal f1 _ _) (Internal f2 _ _) = compare f1 f2

-- Huffman tree
huffman :: [Char] -> HuffmanNode
huffman = build . fromList . map (uncurry Leaf) . count
  where
    build heap
        | size heap == 1 = l
        | otherwise = build $ insert (Internal (weight l + weight r) l r) $ deleteMin $ deleteMin heap
      where
        l = minimum heap
        r = minimum $ deleteMin heap
    weight (Leaf f _) = f
    weight (Internal f _ _) = f

asInt :: Either (Int, Char) Int -> Int64
asInt (Left (f, c)) = 0x80000000 .|. fromIntegral (fromEnum c) `shiftL` 32 .|. fromIntegral f
asInt (Right f) = fromIntegral f

fromInt :: Int64 -> Either (Int, Char) Int
fromInt x
    | isLeft x = Left (fromIntegral $ (x `shiftL` 32) `shiftR` 32, toEnum $ fromIntegral $ x `shiftR` 32)
    | otherwise = Right $ fromIntegral x
  where
    isLeft x = x .&. 0x80000000 /= 0

toList :: HuffmanNode -> [Int64]
toList (Leaf f c) = [asInt $ Left (f, c)]
toList (Internal f l r) = asInt (Right f) : toList l ++ toList r

-- use preorder traversal of list to decode the tree recursively
treeFromList :: [Int64] -> HuffmanNode
treeFromList = fst . helper . map fromInt
  where
    helper (Right f : tail) = (Internal f l r, tail'')
      where
        (l, tail') = helper tail
        (r, tail'') = helper tail'
    helper (Left (f, c) : tail) = (Leaf f c, tail)
    helper [] = error "Unexpected end of list"

-- character count map
count :: [Char] -> [(Int, Char)]
count lst = helper lst []
  where
    helper [] acc = acc
    helper (head : tail) acc = helper tail $ if head `elem` [c | (_, c) <- acc] then map (\(f, c) -> if c == head then (f + 1, c) else (f, c)) acc else (1, head) : acc

-- Huffman Encoding
encode :: [Char] -> ([Bool], Int, HuffmanNode)
encode lst = (concatMap (`encodeChar` tree) lst, len, tree)
  where
    tree = huffman lst
    encodeChar c (Leaf _ _) = []
    encodeChar c (Internal _ l r)
        | c `elem` chars l = False : encodeChar c l
        | c `elem` chars r = True : encodeChar c r
        | otherwise = error "Character not found"
    chars (Leaf _ c) = [c]
    chars (Internal _ l r) = chars l ++ chars r
    len = length lst

-- Huffman Decoding
decode :: ([Bool], Int, HuffmanNode) -> [Char]
decode (lst, len, tree) = helper lst len tree []
  where
    helper _ 1 (Leaf _ c) acc = reverse $ c : acc
    helper lst len (Leaf _ c) acc = helper lst (len - 1) tree (c : acc)
    helper (True : tail) len (Internal _ _ r) acc = helper tail len r acc
    helper (False : tail) len (Internal _ l _) acc = helper tail len l acc
    helper _ _ _ _ = error "Invalid encoding"

toChars :: [Bool] -> [Char]
toChars [] = []
toChars lst = helper lst []
  where
    helper [] acc = acc
    helper lst acc = helper (drop 8 lst) $ acc ++ [toChar $ take 8 lst]
    toChar lst = toEnum $ foldl (\acc (i, b) -> acc + if b then 2 ^ i else 0) 0 $ zip [0 ..] lst

fromChars :: [Char] -> [Bool]
fromChars = concatMap fromChar
  where
    fromChar c = map (testBit (fromEnum c)) [0 .. 7]

compress :: [Char] -> ([Char], Int, [Int64])
compress lst = (toChars encoded, len, toList tree)
  where
    (encoded, len, tree) = encode lst

decompress :: ([Char], Int, [Int64]) -> [Char]
decompress (lst, len, tree) = decode (fromChars lst, len, treeFromList tree)
