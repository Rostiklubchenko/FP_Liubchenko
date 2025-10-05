import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Y = λf.(λx.f(x x))(λx.f(x x))

data GraphNode = 
    Lambda String GraphNode          -- λx.body
  | Apply GraphNode GraphNode        -- f arg
  | Var String                        -- variable
  deriving (Show, Eq)

-- Y-combinator (Curry's fixed-point combinator)
yCombinator :: GraphNode
yCombinator = 
  Lambda "f" (
    Apply 
      (Lambda "x" (Apply (Var "f") (Apply (Var "x") (Var "x"))))
      (Lambda "x" (Apply (Var "f") (Apply (Var "x") (Var "x"))))
  )

-- Reduction step 1: Y g = (λf.(λx.f(x x))(λx.f(x x))) g
step1 :: GraphNode
step1 = Apply yCombinator (Var "g")

-- After β-reduction: (λx.g(x x))(λx.g(x x))
step2 :: GraphNode
step2 = Apply 
  (Lambda "x" (Apply (Var "g") (Apply (Var "x") (Var "x"))))
  (Lambda "x" (Apply (Var "g") (Apply (Var "x") (Var "x"))))

-- After β-reduction: g((λx.g(x x))(λx.g(x x)))
step3 :: GraphNode
step3 = Apply (Var "g") step2

-- Application state
data AppState = AppState
  { currentStep :: Int
  , maxSteps :: Int
  }

-- List of reduction steps
steps :: [GraphNode]
steps = [yCombinator, step1, step2, step3]

stepDescriptions :: [String]
stepDescriptions = 
  [ "Y = λf.(λx.f(x x))(λx.f(x x))"
  , "Y g - application of Y to g"
  , "(λx.g(x x))(λx.g(x x)) - β-reduction"
  , "g((λx.g(x x))(λx.g(x x))) - unfolding"
  ]

-- Position on screen
type Pos = (Float, Float)

-- Calculate positions for graph nodes
layoutGraph :: GraphNode -> Float -> Float -> [(String, Pos, [Pos])]
layoutGraph node x y = layout node x y 0
  where
    hGap = 120
    vGap = 80
    
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
    
    snd3 (_, b, _) = b

-- Draw the graph
drawGraph :: GraphNode -> Picture
drawGraph node = 
  let nodes = layoutGraph node 0 200
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
        '@':_ -> makeColor 0.2 0.6 0.9 1.0  -- blue for application
        'λ':_ -> makeColor 0.9 0.4 0.2 1.0  -- orange for lambda
        _     -> makeColor 0.3 0.8 0.3 1.0  -- green for variables
  in nodePic : edgePics

drawEdge :: Pos -> Pos -> Picture
drawEdge (x1, y1) (x2, y2) = 
  Color (greyN 0.5) $ Line [(x1, y1), (x2, y2)]

-- Main drawing function
drawState :: AppState -> Picture
drawState state = 
  Pictures
    [ translate 0 (-300) $ Scale 0.2 0.2 $ Color white $ 
        Text ("Step " ++ show (currentStep state + 1) ++ "/" ++ show (maxSteps state))
    , translate 0 (-350) $ Scale 0.15 0.15 $ Color white $ 
        Text (stepDescriptions !! currentStep state)
    , translate 0 (-400) $ Scale 0.12 0.12 $ Color (greyN 0.7) $ 
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
  (InWindow "Y-Combinator (Curry) - Graph Representation" (1200, 800) (100, 100))
  black
  30
  initialState
  drawState
  handleEvent
  updateState