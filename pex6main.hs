-- pex6.hs 
-- unKnot Haskell

-- name: Daewon Kwon

{- DOCUMENTATION: Utilized website (https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html) as reference for how : worked with lists.
Utiilized Lesson 26-28 Pre-Readings in order to reference how Haskell interacted with lists and tuples.-}

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
   | snd(head tripCode) == snd(head(tail tripCode)) = typeIIknot (tail(tail tripCode))
   | otherwise = head tripCode:typeIIknot (tail tripCode)

typeIIknotCirc :: [(Char, Char)] -> [(Char, Char)]
typeIIknotCirc tripCode
   | null tripCode = []
   | listLength tripCode < 2 = tripCode
   | otherwise = typeIIknotCirc (tail tripCode)

typeIIknotRemoveLast :: [(Char, Char)] -> [(Char, Char)]
typeIIknotRemoveLast tripCode
   | null tripCode = []
   | listLength tripCode < 2 = []
   | otherwise = head tripCode:typeIIknotRemoveLast (tail tripCode)

typeIIknotFinal :: [(Char, Char)] -> [(Char, Char)]
typeIIknotFinal tripCode
   | null tripCode = []
   | listLength tripCode < 2 = tripCode
   | snd(head tripCode) == snd(head (typeIIknotCirc(tripCode))) = typeIIknotRemoveLast (tail tripCode)
   | otherwise = tripCode

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null (typeIknot tripCode) && null (typeIIknotFinal(typeIIknot tripCode)) = "unknot"
   | typeIknot tripCode /= tripCode = unKnot (typeIknot tripCode)
   | typeIIknotFinal(typeIIknot tripCode) /= tripCode = unKnot (typeIIknotFinal(typeIIknot tripCode))
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

