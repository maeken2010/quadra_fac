import Control.Applicative

main = do
  putStrLn "係数を空白区切りで入れてください"
  ss <- (map read . words) <$> getLine
  print $ quadra_fac ss

quadra_fac :: [Float] -> [Float]
quadra_fac [] = [] 
quadra_fac [a,b,c]
  | d>0  = [(-b+(sqrt d))/(2*a),(-b-(sqrt d))/(2*a)] --解の公式
  | d==0 = [(-b/2*a)] --重解
  | d<0  = [] --虚数解
  where d = (b*b)-(4*a*c) --いわゆる判別式 
quadra_fac (x:y) = [] --要素数3つ以外は空リストを返す
