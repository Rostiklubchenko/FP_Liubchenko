import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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
-- (λx.fact(x x))(λx.fact(x x)) where fact = λf.λn.if...
step3 :: GraphNode
step3 = Apply 
  (Lambda "x" (Apply factorialFunction (Apply (Var "x") (Var "x"))))
  (Lambda "x" (Apply factorialFunction (Apply (Var "x") (Var "x"))))

-- Step 4: Applying to number 3: (Y factorial) 3
step4 :: GraphNode
step4 = Apply (Apply yCombinator factorialFunction) (Num 3)

-- Step 5: Simplified: factorial applied to 3
-- λn.if (n=0) then 1 else n*fact(n-1) applied to 3
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
  [ "Y = λf.(λx.f(x x))(λx.f(x x)) - Y-combinator definition"
  , "fact = λf.λn.if (n=0) then 1 else n*f(n-1) - factorial function"
  , "Y fact - applying Y to factorial"
  , "(λx.fact(x x))(λx.fact(x x)) - β-reduction"
  , "(Y fact) 3 - computing factorial of 3"
  , "if (n=0) then 1 else n*fact(n-1) applied to 3"
  , "if (3=0) then 1 else 3*fact(2) - substitution"
  , "3 * fact(2) - recursion continues..."
  ]

-- Position on screen
type Pos = (Float, Float)

-- Calculate positions for graph nodes
layoutGraph :: GraphNode -> Float -> Float -> [(String, Pos, [Pos])]
layoutGraph node x y = layout node x y 0
  where
    hGap = 100
    vGap = 70
    
    layout (Num n) px py level = 
      [(show n, (px, py), [])]
    
    layout (Var name) px py level = 
      [(name, (px, py), [])]
    
    layout (Lambda param body) px py level = 
      let bodyNodes = layout body px (py - vGap) (level + 1)
          lambdaLabel = "λ" ++ param
          bodyPos = snd3 $ head bodyNodes
      in (lambdaLabel, (px, py), [bodyPos]) : bodyNodes
    
    layout (Apply f arg) px py level = 
      let fNodes = layout f (px - hGap) (py - vGap) (level + 1)
          argNodes = layout arg (px + hGap) (py - vGap) (level + 1)
          appLabel = "@"
          fPos = snd3 $ head fNodes
          argPos = snd3 $ head argNodes
          children = [fPos, argPos]
      in (appLabel, (px, py), children) : (fNodes ++ argNodes)
    
    layout (If cond thn els) px py level = 
      let condNodes = layout cond (px - hGap) (py - vGap) (level + 1)
          thnNodes = layout thn px (py - vGap) (level + 1)
          elsNodes = layout els (px + hGap) (py - vGap) (level + 1)
          condPos = snd3 $ head condNodes
          thnPos = snd3 $ head thnNodes
          elsPos = snd3 $ head elsNodes
          children = [condPos, thnPos, elsPos]
      in ("if", (px, py), children) : (condNodes ++ thnNodes ++ elsNodes)
    
    layout (Op op left right) px py level = 
      let leftNodes = layout left (px - hGap/2) (py - vGap) (level + 1)
          rightNodes = layout right (px + hGap/2) (py - vGap) (level + 1)
          leftPos = snd3 $ head leftNodes
          rightPos = snd3 $ head rightNodes
          children = [leftPos, rightPos]
      in (op, (px, py), children) : (leftNodes ++ rightNodes)
    
    snd3 (_, b, _) = b

-- Draw the graph
drawGraph :: GraphNode -> Picture
drawGraph node = 
  let nodes = layoutGraph node 0 250
      nodeMap = [(label, pos) | (label, pos, _) <- nodes]
  in Pictures $ concatMap (drawNode nodeMap) nodes

drawNode :: [(String, Pos)] -> (String, Pos, [Pos]) -> [Picture]
drawNode nodeMap (label, pos@(x, y), children) =
  let nodePic = Pictures
        [ translate x y $ Color nodeColor $ circleSolid 25
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
  in nodePic : edgePics

drawEdge :: Pos -> Pos -> Picture
drawEdge (x1, y1) (x2, y2) = 
  Color (greyN 0.5) $ Line [(x1, y1), (x2, y2)]

-- Main drawing function
drawState :: AppState -> Picture
drawState state = 
  Pictures
    [ translate 0 (-320) $ Scale 0.2 0.2 $ Color white $ 
        Text ("Step " ++ show (currentStep state + 1) ++ "/" ++ show (maxSteps state))
    , translate 0 (-365) $ Scale 0.13 0.13 $ Color white $ 
        Text (stepDescriptions !! currentStep state)
    , translate 0 (-405) $ Scale 0.12 0.12 $ Color (greyN 0.7) $ 
        Text "Use arrow keys <- -> to navigate"
    , drawGraph (steps !! currentStep state)
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
  (InWindow "Y-Combinator: Factorial Example" (1200, 850) (100, 100))
  black
  30
  initialState
  drawState
  handleEvent
  updateState