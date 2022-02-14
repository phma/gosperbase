module Data.FlowsnakeBase where
import Data.Array
import Data.Word

-- 2 3
--6 0 1
-- 4 5

-- Addition table for flowsnake base. In base 7:
-- 0, 1, 2, 3, 4, 5, 6
-- 1,12, 3,34, 5,16, 0
-- 2, 3,24,25, 6, 0,61
-- 3,34,25,36, 0, 1, 2
-- 4, 5, 6, 0,41,52,43
-- 5,16, 0, 1,52,53, 4
-- 6, 0,61, 2,43, 4,65
aTable=array ((0,0),(6,6))
  [ ((0,0), 0), ((0,1), 1), ((0,2), 2), ((0,3), 3), ((0,4), 4), ((0,5), 5), ((0,6), 6),
    ((1,0), 1), ((1,1), 9), ((1,2), 3), ((1,3),25), ((1,4), 5), ((1,5),13), ((1,6), 0),
    ((2,0), 2), ((2,1), 3), ((2,2),18), ((2,3),19), ((2,4), 6), ((2,5), 0), ((2,6),43),
    ((3,0), 3), ((3,1),25), ((3,2),19), ((3,3),27), ((3,4), 0), ((3,5), 1), ((3,6), 2),
    ((4,0), 4), ((4,1), 5), ((4,2), 6), ((4,3), 0), ((4,4),29), ((4,5),37), ((4,6),31),
    ((5,0), 5), ((5,1),13), ((5,2), 0), ((5,3), 1), ((5,4),37), ((5,5),38), ((5,6), 4),
    ((6,0), 6), ((6,1), 0), ((6,2),43), ((6,3), 2), ((6,4),31), ((6,5), 4), ((6,6),47)
  ]

mTable=array ((0,0),(6,6)) -- Multiplication table for flowsnake base
  [ ((0,0),0), ((0,1),0), ((0,2),0), ((0,3),0), ((0,4),0), ((0,5),0), ((0,6),0),
    ((1,0),0), ((1,1),1), ((1,2),2), ((1,3),3), ((1,4),4), ((1,5),5), ((1,6),6),
    ((2,0),0), ((2,1),2), ((2,2),4), ((2,3),6), ((2,4),1), ((2,5),3), ((2,6),5),
    ((3,0),0), ((3,1),3), ((3,2),6), ((3,3),2), ((3,4),5), ((3,5),1), ((3,6),4),
    ((4,0),0), ((4,1),4), ((4,2),1), ((4,3),5), ((4,4),2), ((4,5),6), ((4,6),3),
    ((5,0),0), ((5,1),5), ((5,2),3), ((5,3),1), ((5,4),6), ((5,5),4), ((5,6),2),
    ((6,0),0), ((6,1),6), ((6,2),5), ((6,3),4), ((6,4),3), ((6,5),2), ((6,6),1)
  ]

th7dig :: (Integral a) => Int -> a -> a
th7dig pos num = (num `div` 7^pos) `mod` 7

split7 :: Word32 -> [Word32]
split7 0 = []
split7 n = (n `mod` 7) : split7 (n `div` 7)

split343 :: Word32 -> [Word32]
split343 0 = []
split343 n = (n `mod` 343) : split343 (n `div` 343)

join7_3 :: [Word32] -> [Word32]
join7_3 [] = []
join7_3 (n0:[]) = [n0]
join7_3 (n0:n1:[]) = [7 * n1 + n0]
join7_3 (n0:n1:n2:ns) = 49*n2+7*n1+n0 : join7_3 ns

join343 :: [Word32] -> Word32
join343 [] = 0
join343 (n:ns) = n + 343 * join343 ns