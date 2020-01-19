-- | Representation of coherent rings. Traditionally a ring is coherent if every
-- finitely generated ideal is finitely presented. This means that it is
-- possible to solve homogenous linear equations in them.
module Algebra.Structures.Coherent
  ( Coherent(solve)
  , propCoherent, isSolution
  , solveMxN, propSolveMxN
  , solveWithIntersection
  , solveGeneralEquation, propSolveGeneralEquation
  , solveGeneral, propSolveGeneral
  ) where

import Test.QuickCheck

import Algebra.Structures.IntegralDomain
import Algebra.Structures.StronglyDiscrete
import Algebra.Matrix
import Algebra.Ideal


-------------------------------------------------------------------------------
-- | Definition of coherent rings.
--
-- We say that R is coherent iff for any M, we can find L such that ML=0 and
--
--   MX=0   \<-\>  \exists Y. X=LY
--
-- that is, iff we can generate the solutions of any linear homogeous system
-- of equations.
--
-- The main point here is that ML=0, it is not clear how to represent the
-- equivalence in Haskell. This would probably be possible in type theory.
--
class IntegralDomain a => Coherent a where
  solve :: Vector a -> Matrix a

-- | Test that the second matrix is a solution to the first.
isSolution :: (CommutativeRing a, Eq a) => Matrix a -> Matrix a -> Bool
isSolution m sol = all (==zero) (concat (unMVec (m `mulM` sol)))

propCoherent :: (Coherent a, Eq a) => Vector a -> Bool
propCoherent m = isSolution (vectorToMatrix m) (solve m)

-------------------------------------------------------------------------------
-- | Solves a system of equations.
solveMxN :: (Coherent a, Eq a) => Matrix a -> Matrix a
solveMxN (M (l:ls)) = solveMxN' (solve l) ls
  where
  -- Inductively solve all subsystems. If the computed solution is in fact a
  -- solution to the next set of equations then don't do anything.
  -- This solves the problems with having many identical rows in the system,
  -- like [[1,1],[1,1]].
  solveMxN' :: (Coherent a, Eq a) => Matrix a -> [Vector a] -> Matrix a
  solveMxN' m []      = m
  solveMxN' m1 (x:xs) = if isSolution (vectorToMatrix x) m1
                           then solveMxN' m1 xs
                           else solveMxN' (m1 `mulM` m2) xs
    where m2 = solve (matrixToVector (mulM (vectorToMatrix x) m1))


-- | Test that the solution of an MxN system is in fact a solution of the system
propSolveMxN :: (Coherent a, Eq a) => Matrix a -> Bool
propSolveMxN m = isSolution m (solveMxN m)


-------------------------------------------------------------------------------
-- | Intersection computable -> Coherence.
--
-- Proof that if there is an algorithm to compute a f.g. set of generators for
-- the intersection of two f.g. ideals then the ring is coherent.
--
-- Takes the vector to solve, \[x1,...,xn\], and a function (int) that computes
-- the intersection of two ideals.
--
-- If \[ x_1, ..., x_n \] \`int\` \[ y_1, ..., y_m \] = \[ z_1, ..., z_l \]
--
-- then int should give witnesses us and vs such that:
--
--     z_k = n_k1 * x_1 + ... + u_kn * x_n = u_k1 * y_1 + ... + n_km * y_m
--
solveWithIntersection :: (IntegralDomain a, Eq a)
                      => Vector a
                      -> (Ideal a -> Ideal a -> (Ideal a,[[a]],[[a]]))
                      -> Matrix a
solveWithIntersection (Vec xs) int = transpose $ matrix $ solveInt xs
  where
  solveInt []     = error "solveInt: Can't solve an empty system"
  solveInt [x]    = [[zero]] -- Base case, could be [x,y] also...
                             -- That wouldn't give the trivial solution...
  solveInt [x,y]  | x == zero || y == zero = [[zero,zero]]
                  | otherwise =
    let (Id ts,us,vs) = (Id [x]) `int` (Id [neg y])
    in [ u ++ v | (u,v) <- zip us vs ]
  solveInt (x:xs)
    | x == zero             = (one : replicate (length xs) zero) : (map (zero:) $ solveInt xs)
    | isSameIdeal int as bs = s ++ m'
    | otherwise             = error "solveInt: This does not compute the intersection"
      where
      as            = Id [x]
      bs            = Id (map neg xs)

      -- Compute the intersection of <x1> and <-x2,...,-xn>
      (Id ts,us,vs) = as `int` bs
      s             = [ u ++ v | (u,v) <- zip us vs ]

      -- Solve <0,x2,...,xn> recursively
      m             = solveInt xs
      m'            = map (zero:) m


-------------------------------------------------------------------------------
-- | Strongly discrete coherent rings.
--
-- If the ring is strongly discrete and coherent then we can solve arbitrary
-- equations of the type AX=b.
--
solveGeneralEquation :: (Coherent a, StronglyDiscrete a) => Vector a -> a -> Maybe (Matrix a)
solveGeneralEquation v@(Vec xs) b =
  let sol = solve v
  in case b `member` (Id xs) of
    Just as -> Just $ transpose (M (replicate (length (head (unMVec sol))) (Vec as)))
                      `addM` sol
    Nothing -> Nothing


propSolveGeneralEquation :: (Coherent a, StronglyDiscrete a, Eq a)
                         => Vector a
                         -> a
                         -> Bool
propSolveGeneralEquation v b = case solveGeneralEquation v b of
  Just sol -> all (==b) $ concat $ unMVec $ vectorToMatrix v `mulM` sol
  Nothing  -> True


isSolutionB v sol b = all (==b) $ concat $ unMVec $ vectorToMatrix v `mulM` sol


-- | Solves general linear systems of the kind AX = B.
--
-- A is given as a matrix and B is given as a row vector (it should be column
-- vector).
--
solveGeneral :: (Coherent a, StronglyDiscrete a, Eq a)
             => Matrix a   -- M
             -> Vector a   -- B
             -> Maybe (Matrix a, Matrix a)  -- (L,X0)
solveGeneral (M (l:ls)) (Vec (a:as)) =
  case solveGeneral' (solveGeneralEquation l a) ls as [(l,a)] of
    Just x0 -> Just (solveMxN (M (l:ls)), x0)
    Nothing -> Nothing
  where
  -- Compute a new solution inductively and check that the new solution
  -- satisfies all the previous equations.
  solveGeneral' Nothing _ _ _              = Nothing
  solveGeneral' (Just m) [] [] old         = Just m
  solveGeneral' (Just m) (l:ls) (a:as) old =
    if isSolutionB l m a
       then solveGeneral' (Just m) ls as old
       else case solveGeneralEquation (matrixToVector (vectorToMatrix l `mulM` m)) a of
         Just m' -> let m'' = m `mulM` m'
                    in if all (\(x,y) -> isSolutionB x m'' y) old
                          then solveGeneral' (Just m'') ls as ((l,a):old)
                          else Nothing
         Nothing -> Nothing
  solveGeneral' _ _ _ _ = error "solveGeneral: Bad input"

-- It would be great to only generate solvable systems...
-- propSolveGeneral :: (Coherent a, StronglyDiscrete a, Eq a) => Matrix a -> Vector a -> Property
propSolveGeneral m b = length (unM m) == length (unVec b) ==> case solveGeneral m b of
  Just (l,x) -> all (==b) (unM (transpose (m `mulM` x))) &&
                isSolution m l
  Nothing -> True
