{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sched
  ( formulation,
    Time (..),
    prettySolved,
    prettyAssignment,
    expandMap,
  )
where

import Algebra.Classes
import Control.Lens hiding (op)
import Control.Monad.Writer
import Data.LinearProgram
import qualified Data.Map as M
import Protolude hiding (Num (..))
import qualified Protolude (Num (..))
import Prelude (String)

class ToILPVar a where
  lpShow :: a -> String

newtype Time = Time {unTime :: Int} deriving (Show)

expandMap :: Map a [a] -> [(a, a)]
expandMap m = mconcat $ M.toList m <&> \(a, as) -> ((a,) <$> as)

formulation ::
  forall job resource.
  (Eq job, ToILPVar job, ToILPVar resource) =>
  Set job ->
  Set resource ->
  Map job [job] -> -- precedence relations
  ((job, resource) -> Bool) -> -- job validity
  ((job, resource) -> Time) -> -- runtimes
  Time -> -- makespan upper bound
  LP String Int
formulation
  (toList -> _J :: [job])
  (toList -> _R :: [resource])
  (expandMap -> _P :: [(job, job)])
  v_
  ((unTime .) -> p_ :: (job, resource) -> Int)
  (unTime -> u :: Int) =
    execLPM . sequenceA_ $
      -- parametrization
      [setVarKind (x_ jr) BinVar | jr <- cartesian _J _R, v_ jr]
        <> [varGeq s_j 0 >> setVarKind s_j IntVar | (s_ -> s_j) <- _J]
        <> [setVarKind cMax ContVar, setObjective $ toFun cMax, setDirection Min]
        -- constraints 1 makespan
        <> [ linCombination [(-1, cMax), (1, s_ j), (p_ jr, x_ jr)] `leqTo` 0
             | jr@(j, _) <- cartesian _J _R,
               v_ jr
           ]
        -- constraints 2 job assigned to exactly one resource
        <> [linCombination [(1, x_ (j, r)) | r <- _R] `equalTo` 1 | j <- _J]
        -- constraints 3 resource must support job
        <> [toFun (x_ jr) `equalTo` 0 | jr <- cartesian _J _R, not (v_ jr)]
        -- constraints 4 precedences
        <> [ linCombination [(1, s_ k), (-1, s_ j), (- (p_ jr), x_ jr)]
               `geqTo` 0
             | (j, k) <- _P,
               r <- _R,
               v_ (j, r),
               let jr = (j, r)
           ]
        -- constraints 5 resources not over-committed
        <> [ let eq =
                   toFun (s_ k)
                     - toFun (s_ j)
                       + linCombination [(u, tau_ (j, k))]
                     - linCombination
                       [(p_ (j, r), x_ (j, r)) | r <- _R, v_ (j, r)]
              in do
                   eq `geqTo` 0
                   eq `leqTo` (u -1)
                   setVarKind (tau_ (j, k)) BinVar
             | (j, k) <- cartesian _J _J,
               j /= k,
               (j, k) `notElem` fullPrecs
           ]
        <> [ let elms = [x_ (j, r), x_ (k, r), tau_ (j, k), tau_ (k, j)]
              in linCombination ((1,) <$> elms) `leqTo` 3
             | (j, k) <- cartesian _J _J,
               j /= k,
               (j, k) `notElem` fullPrecs,
               r <- _R,
               v_ (j, r),
               v_ (k, r)
           ]
    where
      fullPrecs = fillPrecedences _P

fillPrecedences :: (Eq job) => [(job, job)] -> [(job, job)]
fillPrecedences _P = go _P
  where
    go _Q = newEdges _Q & \case
      [] -> _Q
      nes -> go (_Q <> nes)
    newEdges _Q =
      [(i, k) | (i, j) <- _P, (jj, k) <- _Q, jj == j, (i, k) `notElem` _Q]

------ variables

x_ :: (ToILPVar job, ToILPVar resource) => (job, resource) -> String
x_ = sub2 "x"

s_ :: ToILPVar job => job -> String
s_ = sub "s"

tau_ :: (ToILPVar a, ToILPVar b) => (a, b) -> String
tau_ = sub2 "Theta"

cMax :: String
cMax = "C_max"

------ helpers

cartesian :: Applicative f => f a1 -> f a2 -> f (a1, a2)
cartesian a b = (,) <$> a <*> b

toFun :: (Ord v, Additive r, Protolude.Num r) => v -> LinFunc v r
toFun x = linCombination [(1, x)]

------ variable naming

instance Show a => ToILPVar a where
  lpShow x = go <$> show x
    where
      go ' ' = '_'
      go y = y

sub2 :: (ToILPVar a, ToILPVar b) => String -> (a, b) -> String
sub2 x (y, z) = x <> "_" <> lpShow y <> "_" <> lpShow z

sub :: (ToILPVar a) => String -> a -> String
sub x z = x <> "_" <> lpShow z

------ Pretty printing

prettyAssignment :: (Show job, Show resource) => (job, resource, Double) -> Text
prettyAssignment (j, r, t) =
  "t=" <> show t <> " scheduling task " <> show j <> " on " <> show r

prettySolved ::
  (Show job, Show resource) =>
  [job] ->
  [resource] ->
  Map String Double ->
  [(job, resource, Double)]
prettySolved _J _R solved = mconcat listAssignments
  where
    listAssignments = cartesian _J _R
      <&> \(j, r) -> M.lookup (x_ (j, r)) solved & \case
        Nothing -> []
        Just 1 ->
          let t = M.lookup (s_ j) solved & \case
                Nothing -> panic "s_j should exist for all j!"
                Just x -> x
           in [(j, r, t)]
        Just 0 -> []
        Just {} -> panic "x_j should be binary for all j!"
