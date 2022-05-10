qsort []=[]
qsort (x:xs)= qsort ys ++ [x] ++ qsort zs
    where
        ys=[a | a<-xs, a<=x]
        zs=[a | a<-xs, a>x]

-- head [1..10]

-- tail [1..10]

-- [1..10] !! 2

-- take 3 [0..10]
-- drop 3 [0..10]
-- sum [1..10]
-- sum[] -- returns 0
-- length [1..10] -- returns 0
-- length[] -- returns 0
-- product [1..10]
-- product [] -- returns 1
-- [1..10] ++ [11..20]
-- reverse [1..10]

double x=x+x

quadruple x=double (double x)

factorial n=product[1..n]
average ns= sum ns `div` length ns

n=a `div` length xs
    where
        a=10
        xs=[1..10]

lastt xs=head (reverse xs)
lasttt xs= xs !! (length xs - 1)

initt xs=reverse (tail (reverse xs))
inittt xs= take (length xs -1) xs

add :: (Int,Int) -> Int
add (x,y)=x+y -- this is a function which takes 2 parameters/arguments
-- curried functions
add' :: Int ->(Int ->Int)
add' x y=x+y
add5=add' 5

addd (x,y,z)=x+y+z

mult :: Int -> (Int -> (Int ->Int))
mult x y z=x*y*z
mult5= mult 5
-- Conditionals
abss :: Int-> Int
abss n=if n >=0 then n else -n

-- Guarded equations
absss :: Int->Int
absss n | n>=0 =n
        | otherwise = -n

sgnum :: Int-> Int
sgnum n  | n<0 = -1
         | n==0    = 0
         | otherwise = 1

nott :: Bool -> Bool
nott False=True
nott True=False
-- pattern matching
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _=False

--  safetail function for empty list as well
-- if else
safetail :: [a] -> [a]
safetail xs= if null xs then [] else tail xs

-- guarded
safetaill :: [a] -> [a]
safetaill xs | null xs=[]
               | otherwise= tail xs

-- pattern matching
safetailll :: [a] -> [a]
safetailll []=[]
safetailll (_:xs)=xs

(||) :: Bool -> Bool -> Bool

False || False =False
False || True= True
True  || True=True
True  || False=False

-- False || False =False
-- _ || _=True

-- False || b=b
-- True || _ =True

-- write && using conditional
(&&&) :: Bool -> Bool -> Bool
(&&&) x y = if x then if y then True else False else False

(&&&&) :: Bool -> Bool -> Bool
(&&&&) x y = if x Prelude.&& y then True else False

(&&&&&) :: Bool -> Bool -> Bool
(&&&&&) x y = if x then y else False