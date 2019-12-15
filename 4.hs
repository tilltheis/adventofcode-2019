answer1 = length . filter (\n -> (twoAdjacentDigits n) && (neverDecreases n)) $ range
answer2 = length . filter (\n -> (exactlyTwoAdjacentDigits n) && (neverDecreases n)) $ range

twoAdjacentDigits = any (uncurry (==)) . window2 . digits
neverDecreases    = all (uncurry (<=)) . window2 . digits

exactlyTwoAdjacentDigits = go . digits
  where go xs@(x : _) = case span (x ==) xs of
                             ([_, _], _) -> True
                             (_, ys)     -> go ys
        go _          = False

window2 xs = zip xs $ tail xs

digits = show

range = [197487..673251]