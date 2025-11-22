-- pex6.hs 
-- unKnot Haskell

-- name: Daewon Kwon

{- DOCUMENTATION:
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

listLength :: [(Char, Char)] -> Int
listLength a = case a of
    [] -> 0
    (a:as) -> 1 + listLength as

typeIknot :: [(Char, Char)] -> [(Char, Char)]
typeIknot tripCode
   | null tripCode = []
   | listLength tripCode < 2 = tripCode
   | fst(head tripCode) == fst(head(tail tripCode)) = typeIknot (tail(tail tripCode))
   | otherwise = head tripCode:typeIknot (tail tripCode)

typeIIknot :: [(Char, Char)] -> [(Char, Char)]
typeIIknot tripCode
   | null tripCode = []
   | listLength tripCode < 2 = tripCode
   | snd(head tripCode) == snd(head(tail tripCode)) = typeIknot (tail(tail tripCode))
   | otherwise = head tripCode:typeIknot (tail tripCode)

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

