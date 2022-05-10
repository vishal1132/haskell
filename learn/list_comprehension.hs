y=[x^2 | x <- [1..10]]
z=[x^2 | x <- [1..10], x^2 > 50]
a=[(x^2,y) | x <- [1..10],y <- [1..10]]
b=[x^2+y | x <- [1..10],y <- [1..10]]
bb=[x^2+y | x <- [1..10],y <- [x..10]]
bbl=length bb

concat :: [[a]] -> [a]
concat xss =[x | xs <- xss, x <-xs]
xe=[x | x<-[1..10], even x]

factors :: Int -> [Int]
factors n=[x | x <-[1..n], n `mod` x==0]
prime n= factors n ==[1,n]
primes n=[x | x <- [2..n], prime x]

pairs xs=zip xs (tail xs)
sorted :: Ord a => [a] -> Bool
sorted xs=and[x<=y | (x,y) <-pairs xs] 
pos x xs=[i | (x',i)<- zip xs [0..], x==x']

count :: Char -> String -> Int
count x xs=length [x' | x'<-xs, x==x' ]

-- find pythagorean triplets
pyths :: Int -> [(Int,Int,Int)]
pyths n=[ (x,y,n) | x <-[1..n], y<-[1..n], x^2+y^2==n^2]
-- perfect number is a number if sum of it's factors excluding the number itself is equal to the number

perf :: Int -> Bool
perf n=sum (factors n) -n == n
perff n= sum(init(factors n))==n
perfs n=[x | x<-[1..n], perf x]

-- scalar product of two lists
scpr xs ys=sum [ x*y | (x,y)<-zip xs ys ]
sp xs ys=sum[xs !! i * ys !!i | i<-[0..length(xs)-1] ]