{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example
  ( example,
  )
where

import Algebra.Classes
import Control.Lens hiding (op)
import Control.Monad.Writer
import Data.Colour
import Data.Colour.Names as C
import Data.Default
import Data.Graph.Inductive.Graph hiding ((&))
-- import Data.Colour
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Attributes.Complete hiding (Rect (..), XLabel)
import Data.LinearProgram
import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Protolude hiding (Num (..))
import qualified Protolude (Num (..))
import Sched
import Prelude (String)

data Task = A | B Int deriving (Eq, Ord, Show)

data Resource = RA | RB deriving (Eq, Ord, Show)

newtype TaskID = TaskID {unTaskID :: Int}
  deriving (Eq, Ord, Show)
  deriving newtype (Protolude.Num)

newtype ResourceID = ResourceID {unRID :: Int}
  deriving (Eq, Ord, Show)
  deriving newtype (Protolude.Num)

testProgramBuilder ::
  Set TaskID ->
  Set ResourceID ->
  Map TaskID [TaskID] ->
  LP String Int
testProgramBuilder _J _R _P =
  formulation _J _R _P validity runtime (Time 20)

-- (S.fromList $ M.keys exampleTasks)
-- (S.fromList $ M.keys exampleResources)
-- examplePrecedences

validity :: (TaskID, ResourceID) -> Bool
validity (tid, rid) =
  (M.lookup tid exampleTasks, M.lookup rid exampleResources) & \case
    (Just t, Just r) -> exampleValidity (t, r)
    _ -> panic "should not happen"

runtime :: (TaskID, ResourceID) -> Time
runtime (tid, rid) =
  (M.lookup tid exampleTasks, M.lookup rid exampleResources) & \case
    (Just t, Just r) -> exampleRuntime (t, r)
    _ -> panic "should not happen"

exampleValidity :: (Task, Resource) -> Bool
exampleValidity (A, RA) = True
exampleValidity ((B _), RB) = True
exampleValidity _ = False

exampleRuntime :: (Task, Resource) -> Time
exampleRuntime (A, RA) = Time 1
exampleRuntime ((B x), RB) = Time x
exampleRuntime _ = Time (panic "runtime query on invalid placement")

exampleTasks :: Map TaskID Task
exampleTasks =
  M.fromList $
    [(0, B 3), (1, B 3), (2, B 2)]
      <> (fmap (,A) . TaskID [3 .. 10])

exampleResources :: Map ResourceID Resource
exampleResources = M.fromList [(0, RA), (1, RA), (2, RB)]

examplePrecedences :: Map TaskID [TaskID]
examplePrecedences =
  M.fromList $
    bimap TaskID (fmap TaskID)
      <$> [ (2, [4, 3, 8, 5]),
            (1, [4, 5, 2, 3]),
            (3, [7, 6, 8, 10])
          ]

------ main

problemToPng :: Map TaskID a -> Map TaskID [TaskID] -> FilePath -> IO ()
problemToPng exTasks exPrecedences fn =
  void $
    runGraphviz
      ( graphToDot
          quickParams
          (toGraph (toList (M.keys exTasks)) (exPrecedences))
      )
      Png
      fn

example :: IO ()
example = do
  problemToPng exampleTasks examplePrecedences "exampleProblem.png"
  let testProblem' =
        testProgramBuilder
          (S.fromList $ M.keys exampleTasks)
          (S.fromList $ M.keys exampleResources)
          examplePrecedences
  writeLP "example.ilp" testProblem'
  putText "solving LP"
  lowerbound <- glpSolveVars
    (simplexDefaults {msgLev = MsgOff})
    (testProblem' {varTypes = varTypes testProblem' <&> const ContVar})
    >>= \case
      (Success, Just (x, _)) -> return x
      _ -> panic "LP relaxation, solving failed"
  glpSolveVars (mipDefaults {tmLim = 500}) testProblem' >>= \case
    (returnCode, Just (_, solved)) -> do
      print returnCode
      putText $
        "Solved program with "
          <> show (length solved)
          <> " variables."
      let assignments :: [(TaskID, ResourceID, Double)]
          assignments =
            sortBy
              (\(_, _, t) (_, _, t') -> compare t t')
              ( prettySolved (M.keys exampleTasks) (M.keys exampleResources) solved
              )
          formatted :: Text
          formatted =
            mconcat . mconcat $
              (: ["\n"])
                <$> (prettyAssignment <$> assignments)
      void $ renderableToFile def "schedule.png"
        $ fillBackground def
        $ layoutToRenderable
        $ gantt assignments examplePrecedences lowerbound
      print assignments
      writeFile "output.txt" $ toS formatted
    _ -> panic "failure"

------ problem instance to graphviz (one-off code)

instance {-# OVERLAPPABLE #-} Show a => Labellable a where
  toLabelValue x = StrLabel (show x)

toGraph :: [TaskID] -> Map TaskID [TaskID] -> Gr Text Text
toGraph nodes' edges' =
  mkGraph
    ( nodes' <&> \t@(TaskID i) ->
        ( i,
          show i <> ": " <> (show . fromJust $ M.lookup t exampleTasks)
        )
    )
    (expandMap edges' <&> \(TaskID i, TaskID j) -> (i, j, ""))

------ solution to gantt chart (one-off code)

intToDouble :: Int -> Double
intToDouble = fromInteger . Protolude.toInteger

gantt ::
  [(TaskID, ResourceID, Double)] ->
  Map TaskID [TaskID] ->
  Double ->
  Layout Double Double
gantt assignments precedences lowerbound = layout
  where
    dp :: Double
    dp = 0.35
    taskMap :: Map TaskID (ResourceID, Double)
    taskMap = M.fromList (assignments <&> \(t, r, d) -> (t, (r, d)))
    resourceValue :: ResourceID -> Double
    resourceValue (ResourceID i) = fromInteger $ Protolude.toInteger i
    precedenceVectors :: [((Double, Double), (Double, Double))]
    precedenceVectors = mconcat $
      M.toList precedences <&> \(tid, tids) ->
        pVector tid <$> tids
    pVector :: TaskID -> TaskID -> ((Double, Double), (Double, Double))
    pVector i i' = ((ti + rt - dp, ri), (ti' - ti - rt + dp * 2, ri' - ri))
      where
        rt = (intToDouble $ unTime $ runtime (i, rid))
        (rid@(resourceValue -> ri), ti) = fromJust $ M.lookup i taskMap
        (resourceValue -> ri', ti') = fromJust $ M.lookup i' taskMap
    bound =
      plot_lines_values .~ [[(lowerbound, -1 :: Double), (lowerbound, 4)]]
        $ plot_lines_style .~ dashedLine 1.5 [2, 2] (withOpacity C.red 1)
        $ def
    vals :: [((Double, Double), (Double, Double))]
    vals = assignments <&> \(t, r, s) ->
      let x = fromIntegral $ fromJust (elemIndex r (M.keys exampleResources))
          p = fromIntegral (unTime (runtime (t, r)))
       in ((s + dp, x), (p -2 * dp, 0))
    annot =
      plot_annotation_values
        .~ [ (d + 0.2, - 0.05 + fromIntegral (fromJust (elemIndex r (M.keys exampleResources))), show t)
             | ((TaskID t), r, d) <- assignments
           ]
        $ def
    prefVectors =
      plot_vectors_values .~ precedenceVectors
        $ plot_vectors_scale .~ 0
        $ plot_vectors_style . vector_line_style
          .~ dashedLine 1.5 [2, 2] (withOpacity black 1)
        $ plot_vectors_style . vector_head_style . point_color .~ withOpacity black 1
        $ def
    taskVectors =
      plot_vectors_values .~ vals
        $ plot_vectors_scale .~ 0
        $ plot_vectors_style . vector_line_style . line_width .~ 40
        $ plot_vectors_style . vector_line_style . line_color
          .~ withOpacity black 0.1
        $ plot_vectors_style . vector_head_style . point_radius .~ 0
        $ def
    layout =
      layout_title .~ "solution"
        $ layout_x_axis . laxis_override
          .~ ( const $
                 makeAxis
                   ( fmap
                       ( (\x -> if x >= 0 then show x else "")
                           . (round :: Double -> Integer)
                       )
                   )
                   ( [0 .. 10],
                     [],
                     [0 .. 10]
                   )
             )
        $ layout_y_axis . laxis_override
          .~ ( const $
                 makeAxis
                   ( fmap
                       \x -> M.lookup
                         x
                         ( M.mapKeys
                             (intToDouble . unRID)
                             exampleResources
                         )
                         & \case
                           Nothing -> ""
                           Just l -> show ((round x) :: Integer) <> ":" <> show l
                   )
                   ( [-0.5] <> (M.keys exampleResources <&> \(ResourceID i) -> fromInteger $ Protolude.toInteger i) <> [2.5] :: [Double],
                     vals <&> \((x, _), _) -> x,
                     [0, 1, 2] :: [Double]
                   )
             )
        $ layout_left_axis_visibility . axis_show_ticks .~ False
        $ layout_plots
          .~ [ plotVectorField taskVectors,
               toPlot annot,
               toPlot bound,
               plotVectorField prefVectors
             ]
        $ def
