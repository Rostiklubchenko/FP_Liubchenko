import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map

-- Graph representation for GCD function
-- gcd m 0 = m
-- gcd m n = gcd n (m `mod` n)

data GraphNode = 
    Lambda String GraphNode          -- λx.body
  | Apply GraphNode GraphNode        -- f arg
  | Var String                        -- variable
  | If GraphNode GraphNode GraphNode -- if-then-else (pattern matching)
  | Num Int                           -- number literal
  | Op String GraphNode GraphNode    -- operations: =, mod
  | GcdCall GraphNode GraphNode      -- gcd function call
  deriving (Show, Eq)

-- GCD function definition with Y-combinator
-- gcd = λm.λn.if (n=0) then m else gcd n (m mod n)
gcdFunction :: GraphNode
gcdFunction = 
  Lambda "m" (
    Lambda "n" (
      If 
        (Op "=" (Var "n") (Num 0))
        (Var "m")
        (GcdCall (Var "n") (Op "mod" (Var "m") (Var "n")))
    )
  )

-- Step 0: GCD function definition
step0 :: GraphNode
step0 = gcdFunction

-- Step 1: Apply gcd to 9: gcd 9
step1 :: GraphNode
step1 = Apply gcdFunction (Num 9)

-- Step 2: Apply to 10: gcd 9 10
step2 :: GraphNode
step2 = Apply (Apply gcdFunction (Num 9)) (Num 10)

-- Step 3: After first λm reduction: (λn.if (n=0) then 9 else gcd n (9 mod n)) 10
step3 :: GraphNode
step3 = Apply 
  (Lambda "n" (
    If 
      (Op "=" (Var "n") (Num 0))
      (Num 9)
      (GcdCall (Var "n") (Op "mod" (Num 9) (Var "n")))
  ))
  (Num 10)

-- Step 4: After λn reduction: if (10=0) then 9 else gcd 10 (9 mod 10)
step4 :: GraphNode
step4 = If 
  (Op "=" (Num 10) (Num 0))
  (Num 9)
  (GcdCall (Num 10) (Op "mod" (Num 9) (Num 10)))

-- Step 5: Condition is false, evaluate else: gcd 10 (9 mod 10)
-- 9 mod 10 = 9
step5 :: GraphNode
step5 = GcdCall (Num 10) (Num 9)

-- Step 6: Apply gcd again: if (9=0) then 10 else gcd 9 (10 mod 9)
step6 :: GraphNode
step6 = If 
  (Op "=" (Num 9) (Num 0))
  (Num 10)
  (GcdCall (Num 9) (Op "mod" (Num 10) (Num 9)))

-- Step 7: Condition is false: gcd 9 (10 mod 9)
-- 10 mod 9 = 1
step7 :: GraphNode
step7 = GcdCall (Num 9) (Num 1)

-- Step 8: Apply gcd: if (1=0) then 9 else gcd 1 (9 mod 1)
step8 :: GraphNode
step8 = If 
  (Op "=" (Num 1) (Num 0))
  (Num 9)
  (GcdCall (Num 1) (Op "mod" (Num 9) (Num 1)))

-- Step 9: Condition is false: gcd 1 (9 mod 1)
-- 9 mod 1 = 0
step9 :: GraphNode
step9 = GcdCall (Num 1) (Num 0)

-- Step 10: Apply gcd: if (0=0) then 1 else ...
step10 :: GraphNode
step10 = If 
  (Op "=" (Num 0) (Num 0))
  (Num 1)
  (GcdCall (Num 0) (Op "mod" (Num 1) (Num 0)))

-- Step 11: Condition is true, return 1 (NORMAL FORM)
step11 :: GraphNode
step11 = Num 1

-- Application state
data AppState = AppState
  { currentStep :: Int
  , maxSteps :: Int
  }

-- List of reduction steps
steps :: [GraphNode]
steps = [step0, step1, step2, step3, step4, step5, step6, step7, step8, step9, step10, step11]

stepDescriptions :: [String]
stepDescriptions = 
  [ "GCD function: λm.λn.if (n=0) then m else gcd n (m mod n)"
  , "Apply gcd to first argument: gcd 9"
  , "Apply to second argument: gcd 9 10"
  , "β-reduction (substitute m=9): (λn.if (n=0) then 9 else ...) 10"
  , "β-reduction (substitute n=10): if (10=0) then 9 else gcd 10 (9 mod 10)"
  , "Condition false, evaluate else: gcd 10 9 (since 9 mod 10 = 9)"
  , "Pattern match again: if (9=0) then 10 else gcd 9 (10 mod 9)"
  , "Evaluate: gcd 9 1 (since 10 mod 9 = 1)"
  , "Pattern match: if (1=0) then 9 else gcd 1 (9 mod 1)"
  , "Evaluate: gcd 1 0 (since 9 mod 1 = 0)"
  , "Pattern match: if (0=0) then 1 else ..."
  , "NORMAL FORM REACHED: Result = 1 (gcd 9 10 = 1)"
  ]

-- Annotations for graph nodes
type Annotation = String

getAnnotations :: Int -> GraphNode -> Map.Map String Annotation
getAnnotations stepNum node = case stepNum of
  0 -> Map.fromList 
    [ ("λm", "First parameter")
    , ("λn", "Second parameter")
    , ("if", "Base case check")
    , ("=", "Is n zero?")
    , ("gcd", "Recursive call")
    ]
  2 -> Map.fromList
    [ ("9", "First argument")
    , ("10", "Second argument")
    , ("gcd", "Function to apply")
    ]
  4 -> Map.fromList
    [ ("if", "Check n=0")
    , ("10", "n value")
    , ("0", "Base case")
    , ("9", "m value")
    , ("mod", "Modulo operation")
    ]
  5 -> Map.fromList
    [ ("gcd", "Recursive call")
    , ("10_1", "New m")
    , ("9_1", "New n")
    ]
  7 -> Map.fromList
    [ ("gcd", "Still recursing")
    , ("9_1", "m = 9")
    , ("1", "n = 1")
    ]
  9 -> Map.fromList
    [ ("gcd", "Almost done")
    , ("1_1", "m = 1")
    , ("0_1", "n = 0")
    ]
  11 -> Map.fromList
    [ ("1", "Final result!")
    ]
  _ -> Map.empty

-- Position on screen
type Pos = (Float, Float)

-- Node info: (label, position, children positions, unique id)
type NodeInfo = (String, Pos, [Pos], String)

-- Calculate positions for graph nodes
layoutGraph :: GraphNode -> Float -> Float -> Int -> [NodeInfo]
layoutGraph node x y stepNum = 
  let (nodes, _) = layout node x y 0 0
  in nodes
  where
    hGap = 180
    vGap = 110
    
    layout (Num n) px py level idx = 
      let nodeId = show n ++ "_" ++ show idx
      in ([(show n, (px, py), [], nodeId)], idx + 1)
    
    layout (Var name) px py level idx = 
      let nodeId = name ++ "_" ++ show idx
      in ([(name, (px, py), [], nodeId)], idx + 1)
    
    layout (Lambda param body) px py level idx = 
      let (bodyNodes, idx') = layout body px (py - vGap) (level + 1) (idx + 1)
          lambdaLabel = "λ" ++ param
          bodyPos = getPos $ head bodyNodes
          nodeId = lambdaLabel
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
          nodeId = "if"
      in (("if", (px, py), [condPos, thnPos, elsPos], nodeId) : (condNodes ++ thnNodes ++ elsNodes), idx''')
    
    layout (Op op left right) px py level idx = 
      let spacing = hGap * 0.7
          (leftNodes, idx') = layout left (px - spacing) (py - vGap) (level + 1) (idx + 1)
          (rightNodes, idx'') = layout right (px + spacing) (py - vGap) (level + 1) idx'
          leftPos = getPos $ head leftNodes
          rightPos = getPos $ head rightNodes
          nodeId = op ++ "_" ++ show idx
      in ((op, (px, py), [leftPos, rightPos], nodeId) : (leftNodes ++ rightNodes), idx'')
    
    layout (GcdCall left right) px py level idx = 
      let spacing = hGap * 0.8
          (leftNodes, idx') = layout left (px - spacing) (py - vGap) (level + 1) (idx + 1)
          (rightNodes, idx'') = layout right (px + spacing) (py - vGap) (level + 1) idx'
          leftPos = getPos $ head leftNodes
          rightPos = getPos $ head rightNodes
          nodeId = "gcd"
      in (("gcd", (px, py), [leftPos, rightPos], nodeId) : (leftNodes ++ rightNodes), idx'')
    
    getPos (_, pos, _, _) = pos

-- Draw the graph with auto-scaling
drawGraph :: GraphNode -> Int -> Picture
drawGraph node stepNum = 
  let nodes = layoutGraph node 0 350 stepNum
      annotations = getAnnotations stepNum node
      -- Calculate bounds
      positions = [pos | (_, pos, _, _) <- nodes]
      xs = [x | (x, _) <- positions]
      ys = [y | (_, y) <- positions]
      minX = if null xs then 0 else minimum xs - 50
      maxX = if null xs then 0 else maximum xs + 150
      minY = if null ys then 0 else minimum ys - 50
      maxY = if null ys then 0 else maximum ys + 50
      width = maxX - minX
      height = maxY - minY
      -- Scale to fit screen
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
        "mod" -> makeColor 0.9 0.9 0.2 1.0
        "gcd" -> makeColor 0.9 0.2 0.5 1.0    -- pink for gcd
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
  [ Color (makeColor 0.2 0.2 0.2 0.9) $ Polygon [(-10, -205), (200, -205), (200, 10), (-10, 10)]
  , Color white $ translate 5 (-10) $ Scale 0.13 0.13 $ Text "Legend:"
  , Color (makeColor 0.9 0.4 0.2 1.0) $ translate 10 (-40) $ circleSolid 10
  , Color white $ translate 30 (-43) $ Scale 0.11 0.11 $ Text "Lambda"
  , Color (makeColor 0.2 0.6 0.9 1.0) $ translate 10 (-65) $ circleSolid 10
  , Color white $ translate 30 (-68) $ Scale 0.11 0.11 $ Text "Application"
  , Color (makeColor 0.9 0.2 0.5 1.0) $ translate 10 (-90) $ circleSolid 10
  , Color white $ translate 30 (-93) $ Scale 0.11 0.11 $ Text "GCD call"
  , Color (makeColor 0.3 0.8 0.3 1.0) $ translate 10 (-115) $ circleSolid 10
  , Color white $ translate 30 (-118) $ Scale 0.11 0.11 $ Text "Variable"
  , Color (makeColor 0.2 0.9 0.9 1.0) $ translate 10 (-140) $ circleSolid 10
  , Color white $ translate 30 (-143) $ Scale 0.11 0.11 $ Text "Number"
  , Color (makeColor 0.9 0.9 0.2 1.0) $ translate 10 (-165) $ circleSolid 10
  , Color white $ translate 30 (-168) $ Scale 0.11 0.11 $ Text "Operator"
  , Color (makeColor 0.8 0.2 0.8 1.0) $ translate 10 (-190) $ circleSolid 10
  , Color white $ translate 30 (-193) $ Scale 0.11 0.11 $ Text "Condition"
  ]

-- Draw reduction info
drawReductionInfo :: Int -> Picture
drawReductionInfo stepNum = translate 500 450 $ Pictures
  [ Color (makeColor 0.2 0.2 0.2 0.9) $ Polygon [(-10, -100), (250, -100), (250, 10), (-10, 10)]
  , Color white $ translate 5 (-10) $ Scale 0.13 0.13 $ Text "Reduction Info:"
  , Color (makeColor 0.8 0.9 1.0 1.0) $ translate 5 (-35) $ Scale 0.1 0.1 $ Text info
  ]
  where
    info = case stepNum of
      0 -> "Definition phase"
      1 -> "Partial application"
      2 -> "Full application"
      3 -> "Beta-reduction: m=9"
      4 -> "Beta-reduction: n=10"
      5 -> "Recursion: gcd 10 9"
      6 -> "Check base case"
      7 -> "Recursion: gcd 9 1"
      8 -> "Check base case"
      9 -> "Recursion: gcd 1 0"
      10 -> "Base case matched"
      11 -> "NORMAL FORM!"
      _ -> ""

-- Main drawing function
drawState :: AppState -> Picture
drawState state = 
  Pictures
    [ translate 0 (-480) $ Scale 0.22 0.22 $ Color white $ 
        Text ("Step " ++ show (currentStep state + 1) ++ "/" ++ show (maxSteps state))
    , translate 0 (-520) $ Scale 0.12 0.12 $ Color (makeColor 1 1 0.8 1) $ 
        Text (stepDescriptions !! currentStep state)
    , translate 0 (-550) $ Scale 0.11 0.11 $ Color (greyN 0.7) $ 
        Text "Use arrow keys <- -> to navigate | ESC to exit"
    , drawGraph (steps !! currentStep state) (currentStep state)
    , drawLegend
    , drawReductionInfo (currentStep state)
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