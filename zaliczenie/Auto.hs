module Auto (Auto, accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where

import Data.List

data Auto a q = A {
  states :: [q],
  initStates :: [q],
  isAccepting :: q -> Bool,
  transition :: q -> a -> [q]
}

travel :: Auto a q -> a -> [q] -> [q]
travel automat a = foldr (\q states -> transition automat q a ++ states) []

travelLong :: Eq q => Auto a q -> [q] -> [a] -> [q]
travelLong automat qs = foldl (\states a -> nub $ travel automat a states) qs

anyStateAccepts :: Auto a q -> [q] -> Bool
anyStateAccepts automat qs = any (isAccepting automat) qs

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts automat as = anyStateAccepts automat (travelLong automat (initStates automat) as)

emptyA :: Auto a ()
emptyA = A [] [] (const False) (\_ _ -> [])

epsA :: Auto a ()
epsA = A [()] [()] (const True) (\_ _ -> [])

symA :: Eq a => a -> Auto a Bool
symA a = A [False, True] [False] (\x -> x) (\state letter -> if(letter == a && (not state))
                                             then [True] else [])
         
leftA :: Auto a q -> Auto a (Either q r)
leftA automat = A (map Left $ states automat) (map Left $ initStates automat)
                (either (isAccepting automat) (const False))
                (either (\q a -> map Left (transition automat q a)) (\_ _ -> []))

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA automat1 automat2 = A ((map Left (states automat1))++(map Right (states automat2)))
                         ((map Left (initStates automat1))++(map Right (initStates automat2)))
                         (either (isAccepting automat1) (isAccepting automat2))
                         (either
                           (\q a -> map Left (transition automat1 q a))
                           (\q a -> map Right (transition automat2 q a)))

first (a, _, _) = a
second (_, b, _) = b
third (_, _, c) = c

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA automat1 automat2 = A ((map Left (states automat1))++(map Right (states automat2)))
                          (let leftStates = initStates automat1 in
                             if anyStateAccepts automat1 leftStates
                             then ((map Left (initStates automat1))++(map Right (initStates automat2)))
                             else map Left (initStates automat1))
                          (either (const False) (isAccepting automat2))
                          (either
                           (\q a -> let leftStates = transition automat1 q a in
                               if anyStateAccepts automat1 leftStates
                               then (map Left leftStates)++(map Right (initStates automat2))
                               else map Left leftStates)
                           (\q a -> map Right (transition automat2 q a)))

fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists states initStates terminals transitions =
  A states initStates (\q -> elem q terminals)
  (\q a -> foldr (\tuple states-> if (q == first(tuple) && a == second(tuple))
                   then third(tuple)++states else states) [] transitions)

toLists :: (Enum a, Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists automat = (qs, initStates automat, filter (isAccepting automat) qs,
                   foldr
                    (\sym states -> foldr(\state acc -> let destiny = travel automat sym [state] in
                               if not $ null destiny then (state, sym, destiny):acc else acc) states qs)
                    [] as)
  where qs = states(automat)
        as = [minBound .. maxBound]

instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
  show automat = show (toLists automat)
