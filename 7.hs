import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map
import Data.List (nub)

-- Y = λf.(λx.f(x x))(λx.f(x x))
-- factorial using Y-combinator

data GraphNode = 
    Lambda String GraphNode          -- λx.body
  | Apply GraphNode GraphNode        -- f arg
  | Var String                        -- variable
  | If GraphNode GraphNode GraphNode -- if-then-else
  | Num Int                           -- number literal
  | Op String GraphNode GraphNode    -- operations: =, *, -
  deriving (Show, Eq)

-- Y-combinator (Curry's fixed-point combinator)
yCombinator :: GraphNode
yCombinator = 
  Lambda "f" (
    Apply 
      (Lambda "x" (Apply (Var "f") (Apply (Var "x") (Var "x"))))
      (Lambda "x" (Apply (Var "f") (Apply (Var "x") (Var "x"))))
  )

-- Factorial function: λf.λn.if (n=0) then 1 else n*f(n-1)
factorialFunction :: GraphNode
factorialFunction = 
  Lambda "f" (
    Lambda "n" (
      If 
        (Op "=" (Var "n") (Num 0))
        (Num 1)
        (Op "*" (Var "n") 
          (Apply (Var "f") (Op "-" (Var "n") (Num 1))))
    )
  )

-- Step 0: Y combinator definition
step0 :: GraphNode
step0 = yCombinator

-- Step 1: factorial function definition
step1 :: GraphNode
step1 = factorialFunction

-- Step 2: Y factorial = applying Y to factorial function
step2 :: GraphNode
step2 = Apply yCombinator factorialFunction

-- Step 3: After first β-reduction
step3 :: GraphNode
step3 = Apply 
  (Lambda "x" (Apply factorialFunction (Apply (Var "x") (Var "x"))))
  (Lambda "x" (Apply factorialFunction (Apply (Var "x") (Var "x"))))

-- Step 4: Applying to number 3: (Y factorial) 3
step4 :: GraphNode
step4 = Apply (Apply yCombinator factorialFunction) (Num 3)

-- Step 5: Simplified: factorial applied to 3
step5 :: GraphNode
step5 = Apply
  (Lambda "n" (
    If 
      (Op "=" (Var "n") (Num 0))
      (Num 1)
      (Op "*" (Var "n") (Apply (Var "fact") (Op "-" (Var "n") (Num 1))))
  ))
  (Num 3)

-- Step 6: After substitution: if (3=0) then 1 else 3*fact(3-1)
step6 :: GraphNode
step6 = If 
  (Op "=" (Num 3) (Num 0))
  (Num 1)
  (Op "*" (Num 3) (Apply (Var "fact") (Op "-" (Num 3) (Num 1))))

-- Step 7: Condition evaluates to false, so: 3 * fact(2)
step7 :: GraphNode
step7 = Op "*" (Num 3) (Apply (Var "fact") (Num 2))

-- Application state
data AppState = AppState
  { currentStep :: Int
  , maxSteps :: Int
  }

-- List of reduction steps
steps :: [GraphNode]
steps = [step0, step1, step2, step3, step4, step5, step6, step7]

stepDescriptions :: [String]
stepDescriptions = 
  [ "Y-combinator definition: enables recursion without explicit self-reference"
  , "Factorial function: recursive definition using function parameter f"
  , "Applying Y to factorial: Y fact creates recursive factorial"
  , "Beta-reduction: unfolding the Y-combinator structure"
  , "Computing factorial of 3: (Y fact) 3"
  , "Function application: substituting n with 3"
  , "Evaluating condition: 3=0 is false, go to else branch"
  , "Recursive call: 3 * fact(2) - recursion continues"
  ]

-- Annotations for graph nodes
type Annotation = String

getAnnotations :: Int -> GraphNode -> Map.Map String Annotation
getAnnotations stepNum node = case stepNum of
  0 -> Map.fromList 
    [ ("λf", "Takes function f")
    , ("@_root", "Application")
    , ("λx_1", "First copy")
    , ("λx_2", "Second copy")
    ]
  1 -> Map.fromList
    [ ("λf", "Recursive parameter")
    , ("λn", "Number argument")
    , ("if", "Base case check")
    , ("=", "Is n zero?")
    , ("*", "Multiply")
    , ("-", "Decrement")
    ]
  2 -> Map.fromList
    [ ("@_root", "Y applied to fact")
    , ("λf_y", "Y combinator")
    , ("λf_fact", "Factorial function")
    ]
  4 -> Map.fromList
    [ ("@_root", "Apply to 3")
    , ("3", "Input value")
    ]
  5 -> Map.fromList
    [ ("λn", "Function body")
    , ("3", "Argument")
    , ("if", "Check base case")
    ]
  6 -> Map.fromList
    [ ("if", "Condition check")
    , ("3_1", "n value")
    , ("0", "Base case")
    , ("1", "Return 1")
    , ("3_2", "Multiply by 3")
    ]
  7 -> Map.fromList
    [ ("*", "Multiplication")
    , ("3", "Current value")
    , ("fact", "Recursive call")
    , ("2", "Next iteration")
    ]
  _ -> Map.empty

-- Position on screen
type Pos = (Float, Float)

-- Node info: (label, position, children positions, unique id for annotations)
type NodeInfo = (String, Pos, [Pos], String)

-- Calculate positions for graph nodes with better spacing
layoutGraph :: GraphNode -> Float -> Float -> Int -> [NodeInfo]
layoutGraph node x y stepNum = 
  let (nodes, _) = layout node x y 0 0
  in nodes
  where
    hGap = 180
    vGap = 110
    
    layout (Num n) px py level idx = 
      let nodeId = show n ++ if level > 0 then "_" ++ show idx else ""
      in ([(show n, (px, py), [], nodeId)], idx + 1)
    
    layout (Var name) px py level idx = 
      let nodeId = name ++ if level > 0 then "_" ++ show idx else ""
      in ([(name, (px, py), [], nodeId)], idx + 1)
    
    layout (Lambda param body) px py level idx = 
      let (bodyNodes, idx') = layout body px (py - vGap) (level + 1) (idx + 1)
          lambdaLabel = "λ" ++ param
          bodyPos = getPos $ head bodyNodes
          nodeId = lambdaLabel ++ if level > 0 then "_" ++ show idx else ""
      in ((lambdaLabel, (px, py), [bodyPos], nodeId) : bodyNodes, idx')
    
    layout (Apply f arg) px py level idx = 
      let leftShift = if level > 2 then hGap * 0.7 else hGap
          rightShift = if level > 2 then hGap * 0.7 else hGap
          (fNodes, idx') = layout f (px - leftShift) (py - vGap) (level + 1) (idx + 1)
          (argNodes, idx'') = layout arg (px + rightShift) (py - vGap) (level + 1) idx'
          appLabel = "@"
          fPos = getPos $ head fNodes
          argPos = getPos $ head argNodes
          nodeId = appLabel ++ "_" ++ show idx
      in ((appLabel, (px, py), [fPos, argPos], nodeId) : (fNodes ++ argNodes), idx'')
    
    layout (If cond thn els) px py level idx = 
      let spacing = hGap * 1.5
          (condNodes, idx') = layout cond (px - spacing) (py - vGap) (level + 1) (idx + 1)
          (thnNodes, idx'') = layout thn px (py - vGap) (level + 1) idx'
          (elsNodes, idx''') = layout els (px + spacing) (py - vGap) (level + 1) idx''
          condPos = getPos $ head condNodes
          thnPos = getPos $ head thnNodes
          elsPos = getPos $ head elsNodes
          nodeId = "if_" ++ show idx
      in (("if", (px, py), [condPos, thnPos, elsPos], nodeId) : (condNodes ++ thnNodes ++ elsNodes), idx''')
    
    layout (Op op left right) px py level idx = 
      let spacing = hGap * 0.7
          (leftNodes, idx') = layout left (px - spacing) (py - vGap) (level + 1) (idx + 1)
          (rightNodes, idx'') = layout right (px + spacing) (py - vGap) (level + 1) idx'
          leftPos = getPos $ head leftNodes
          rightPos = getPos $ head rightNodes
          nodeId = op ++ "_" ++ show idx
      in ((op, (px, py), [leftPos, rightPos], nodeId) : (leftNodes ++ rightNodes), idx'')
    
    getPos (_, pos, _, _) = pos

-- Draw the graph with scaling to fit screen
drawGraph :: GraphNode -> Int -> Picture
drawGraph node stepNum = 
  let nodes = layoutGraph node 0 350 stepNum
      annotations = getAnnotations stepNum node
      -- Calculate bounds
      positions = [pos | (_, pos, _, _) <- nodes]
      xs = [x | (x, _) <- positions]
      ys = [y | (_, y) <- positions]
      minX = minimum xs - 50
      maxX = maximum xs + 150
      minY = minimum ys - 50
      maxY = maximum ys + 50
      width = maxX - minX
      height = maxY - minY
      -- Scale to fit screen (leave room for UI)
      scaleX = 1600 / width
      scaleY = 900 / height
      scaleFactor = min (min scaleX scaleY) 1.0
      -- Center offset
      centerX = -(minX + maxX) / 2
      centerY = -(minY + maxY) / 2
  in Scale scaleFactor scaleFactor $ 
     Translate centerX centerY $
     Pictures $ concatMap (drawNode annotations) nodes

drawNode :: Map.Map String Annotation -> NodeInfo -> [Picture]
drawNode annotations (label, pos@(x, y), children, nodeId) =
  let nodePic = Pictures
        [ translate x y $ Color nodeColor $ circleSolid 30
        , translate x y $ Scale 0.15 0.15 $ Color white $ Text label
        ]
      edgePics = map (drawEdge pos) children
      nodeColor = case label of
        '@':_ -> makeColor 0.2 0.6 0.9 1.0    -- blue for application
        'λ':_ -> makeColor 0.9 0.4 0.2 1.0    -- orange for lambda
        "if"  -> makeColor 0.8 0.2 0.8 1.0    -- purple for if
        "="   -> makeColor 0.9 0.9 0.2 1.0    -- yellow for operators
        "*"   -> makeColor 0.9 0.9 0.2 1.0
        "-"   -> makeColor 0.9 0.9 0.2 1.0
        c:_   -> if c `elem` ['0'..'9'] 
                 then makeColor 0.2 0.9 0.9 1.0  -- cyan for numbers
                 else makeColor 0.3 0.8 0.3 1.0  -- green for variables
        _     -> makeColor 0.5 0.5 0.5 1.0
      
      -- Add annotation if exists
      annotationPic = case Map.lookup nodeId annotations of
        Just ann -> [translate (x + 50) (y + 5) $ 
                     Pictures [ Color (makeColor 0.1 0.1 0.1 0.8) $ 
                               Polygon [(-5, -8), (textWidth, -8), (textWidth, 8), (-5, 8)]
                              , Color (makeColor 1 1 0.8 1) $ 
                                translate 0 (-3) $ Scale 0.09 0.09 $ Text ann
                              ]
                    ]
              where textWidth = fromIntegral (length ann) * 5
        Nothing -> []
      
  in nodePic : edgePics ++ annotationPic

drawEdge :: Pos -> Pos -> Picture
drawEdge (x1, y1) (x2, y2) = 
  Color (greyN 0.5) $ Line [(x1, y1), (x2, y2)]

-- Draw legend
drawLegend :: Picture
drawLegend = translate (-750) 450 $ Pictures
  [ Color (makeColor 0.2 0.2 0.2 0.9) $ Polygon [(-10, -180), (200, -180), (200, 10), (-10, 10)]
  , Color white $ translate 5 (-10) $ Scale 0.13 0.13 $ Text "Legend:"
  , Color (makeColor 0.9 0.4 0.2 1.0) $ translate 10 (-40) $ circleSolid 10
  , Color white $ translate 30 (-43) $ Scale 0.11 0.11 $ Text "Lambda"
  , Color (makeColor 0.2 0.6 0.9 1.0) $ translate 10 (-65) $ circleSolid 10
  , Color white $ translate 30 (-68) $ Scale 0.11 0.11 $ Text "Application"
  , Color (makeColor 0.3 0.8 0.3 1.0) $ translate 10 (-90) $ circleSolid 10
  , Color white $ translate 30 (-93) $ Scale 0.11 0.11 $ Text "Variable"
  , Color (makeColor 0.2 0.9 0.9 1.0) $ translate 10 (-115) $ circleSolid 10
  , Color white $ translate 30 (-118) $ Scale 0.11 0.11 $ Text "Number"
  , Color (makeColor 0.9 0.9 0.2 1.0) $ translate 10 (-140) $ circleSolid 10
  , Color white $ translate 30 (-143) $ Scale 0.11 0.11 $ Text "Operator"
  , Color (makeColor 0.8 0.2 0.8 1.0) $ translate 10 (-165) $ circleSolid 10
  , Color white $ translate 30 (-168) $ Scale 0.11 0.11 $ Text "Condition"
  ]

-- Main drawing function
drawState :: AppState -> Picture
drawState state = 
  Pictures
    [ translate 0 (-480) $ Scale 0.22 0.22 $ Color white $ 
        Text ("Step " ++ show (currentStep state + 1) ++ "/" ++ show (maxSteps state))
    , translate 0 (-520) $ Scale 0.12 0.12 $ Color (makeColor 1 1 0.8 1) $ 
        Text (stepDescriptions !! currentStep state)
    , translate 0 (-550) $ Scale 0.11 0.11 $ Color (greyN 0.7) $ 
        Text "Use arrow keys <- -> to navigate | F - fullscreen"
    , drawGraph (steps !! currentStep state) (currentStep state)
    , drawLegend
    ]

-- Event handling
handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state =
  state { currentStep = min (currentStep state + 1) (maxSteps state - 1) }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state =
  state { currentStep = max (currentStep state - 1) 0 }
handleEvent _ state = state

-- Update state
updateState :: Float -> AppState -> AppState
updateState _ state = state

-- Initial state
initialState :: AppState
initialState = AppState
  { currentStep = 0
  , maxSteps = length steps
  }

-- Main function
main :: IO ()
main = play
  FullScreen
  black
  30
  initialState
  drawState
  handleEvent
  updateState