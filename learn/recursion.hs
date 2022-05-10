fac 0=1
fac n=n * fac(n-1)

prod :: Num a=> [a] -> a
prod []=1
prod (n:ns)=n*prod ns
a=prod [1,2,3]

len :: [a]->Int
len []=0
len (_:xs)=1+len xs

rev :: [a] -> [a]
rev (x:xs)=reverse xs +++ [x]

zipp :: [a] -> [b] ->[(a,b)]
zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (y:ys)=(x,y): zip xs ys

dropp :: Int-> [a] -> [a]
dropp 0 xs=xs
dropp _ []=[]
dropp n (_:xs)=dropp(n-1) xs

(++) :: [a] -> [a] -> [a]
[] ++ ys =ys
(x:xs) ++ ys= x: (xs Main.++ ys)

(+++) :: [a] -> [a] -> [a]
[] +++ ys =ys
(x:xs) +++ ys= x: (xs +++ ys)

qsort :: Ord a => [a] ->[a]
qsort []=[]
qsort (x:xs)= qsort smaller Main.++ [x] Main.++ qsort larger
              where
                  smaller = [a | a<- xs, a<=x]
                  larger = [b | b<- xs, b>x]