module ListUtils where

import Data.Maybe (fromJust)

findValueP :: (a -> Bool) -> [a] -> a
findValueP f lst = head $ filter f lst

findValue :: Eq key => key -> [(key, value)] -> value
findValue key lst = fromJust $ key `lookup` lst

removeKey :: Eq key => key -> [(key, value)] -> [(key, value)]
removeKey key lst = filter ((/= key) . fst) lst

updateKey :: Eq key => key -> value -> [(key, value)] -> [(key, value)]
updateKey key value lst = (key, value) : removeKey key lst
