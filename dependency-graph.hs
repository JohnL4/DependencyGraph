-- Emit to stdout a series of dot(1) edges specifying dependencies.
-- "A -> B" means "A depends on B".
--
-- Build with 'ghc dependency-graph.hs'
-- 
-- Input is a text file containing lines as follows:
--      (some indentation) (some extraneous text) (file-A) in (some directory)
--          (some extra indentation) (some extraneous text) (file-B) in (some directory)
--      (some indentation matching the first line above) (some extraneous text) (file-C) in (some directory)
--
-- This means that file-A depends on file-B, but neither file-A nor file-B depend on file-C.
--
-- Sample:
--    Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Processing SXA.Compass.Config.ViewModel.dll	in C:\Program Files (x86)\Allscripts Sunrise\Clinical Manager Client\7.2.5575.0\
--    Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Adding C:\Program Files (x86)\Allscripts Sunrise\Clinical Manager Client\7.2.5575.0\SXA.Compass.Config.ViewModel.dll	(IsPresent=true)	to assemblyList at beginning of GetAssemblyListEx()
--      Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Processing SXA.Compass.Config.Utils.dll	in C:\Program Files (x86)\Allscripts Sunrise\Clinical Manager Client\7.2.5575.0\
--
-- (Need to skip the line containing "Adding", and only process the ones containing "Processing".)

import Debug.Trace
import System.Environment
import Prelude -- hiding (readFile) -- Because we want the System.IO.Strict version
import System.IO
import Text.Regex.TDFA
import qualified Data.Map.Strict as Map
import Data.List
import Data.Map.Strict as Map ((!))

-- See http://stackoverflow.com/q/32149354/370611
-- toRegex = makeRegexOpts defaultCompOpt{multiline=False} defaultExecOpt

-- Escape parens?
-- initialFillerRegex :: String
-- initialFillerRegex = "Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList\\(\\) Information: 0 : Processing"

-- Regex matching (marking) a line to be processed
-- valuableLineRegex :: String
-- valuableLineRegex = "\\bProcessing\\b"

-- |Regex matching line to be parsed
parseLineRegex :: String
parseLineRegex = "^(.* Information: 0 : Processing )([^ ]*)[ \t]+in (.*)" -- 3 subexpressions

main :: IO()
main = do
  args <- getArgs
  logContents <-
    if length args == 0
    then do
      putStrLn "Reading stdin..."
      getContents
    else do
      putStrLn ("Reading " ++ show (args !! 0) ++ "...")
      handle <- openFile (args !! 0) ReadMode
      hGetContents handle

  putStrLn $ unlines $ process (map parseIndent $ lines logContents) $ State {stack=[], edges=Map.empty, nodes=Map.empty}

-- |A line of indented text.
data IndentedText = IndentedText { indent :: Int, -- ^ The indent (expected to be a number of spaces)
                                   text :: String -- ^ The text (expected not to start with a space).
                                 }
                    deriving (Show)

-- |An edge
data Edge = Edge { from :: String,
                   to :: String
                 }
            deriving (Show, Ord, Eq)

-- |Internal pgm state
data State = State { stack :: [IndentedText], -- ^ A stack of indented log lines, each at a higher indent level than the
                                              -- ^ previous.  Head is top of stack.
                     edges :: Map.Map Edge Int, -- ^ A set of edges with frequency counts.  Node names will be abbreviated.
                     nodes :: Map.Map String String -- ^ A map from long filepath to abbreviation
                   }
             deriving (Show)
             

----------------------------------------------------------------
-- |Parses out the leading indentation of the given String into a count of spaces and the rest of the line
parseIndent :: String -> IndentedText
parseIndent s =
  let matchv = (fourth $ (s =~ "^( *)(.*)" :: (String,String,String,[String])))
  in
    IndentedText { indent = length $ matchv !! 0,
                   text = matchv !! 1
                 }

----------------------------------------------------------------
-- |Process the input, keeping track of state while doing so.
process :: [IndentedText] -> State -> [String]

process [] state = (edgeDump $ Map.assocs $ edges state)
  ++ [""]                       -- blank line
  ++ (nodeDump $ sortBy nodeSort $ Map.assocs $ nodes state)

process (line:rest) state
  | processable =
    trace "Processing line"
    process rest $ newState state line
  | otherwise =
    trace "Skipping line"
    process rest state -- Unprocessable line ==> continue, no state change
  where processable = text line =~ parseLineRegex :: Bool


----------------------------------------------------------------
-- |Update state while processing. Input (IndentedText) is a line from the log being processed.
newState :: State -> IndentedText -> State

newState state curLine
  | (length $ stack state) == 0 =
      -- initial state
      State { stack = [curLine],
              edges = Map.empty,
              nodes = Map.singleton (fullname $ text curLine) "f0"
            }
  | indent curLine > (indent $ head $ stack state) =
      -- indented
      State { stack = (curLine:(stack state)),
              edges = (Map.insertWith (+) (edgeFromTo (head $ stack state) curLine nnmap) 1 (edges state)),
              nodes = nnmap
            }
  | indent curLine == (indent $ head $ stack state) =
      -- same level
      State { stack = (curLine:prevStack),
              edges = if (length prevStack) == 0 then
                        edges state
                      else
                        (Map.insertWith (+) (edgeFromTo (head prevStack) curLine nnmap) 1 (edges state)),
              nodes = nnmap
            }
  | indent curLine < (indent $ head $ stack state) =
      -- outdented
      State { stack = (curLine:prevStack),
              edges = if length prevStack == 0 then
                        edges state -- No new edge, since the stack is empty
                      else
                        (Map.insertWith (+) (edgeFromTo (head prevStack) curLine nnmap) 1 (edges state)),
              nodes = nnmap
            }
  where prevStack =
          -- Unwind the stack to where its top corresponds to a line with smaller indent than the current line.
          (dropWhile greaterIndent (stack state)) -- Could use in "==" case?
        greaterIndent stackLine = (indent stackLine) >= (indent curLine) -- Could drop entire stack, returning []
        nnmap = newNodesMap (nodes state) curLine

----------------------------------------------------------------
-- |Returns a new nodes map which is the old nodes map, possibly with a new node inserted from the IndentedText argument.
newNodesMap :: Map.Map String String -- ^ Old nodes map
  -> IndentedText                    -- ^ Possible source of new node
  -> Map.Map String String           -- ^ New nodes map

newNodesMap aMap indText
  | Map.member key aMap =
    trace ("Abbrev already exists for key " ++ show key)
    aMap
  | otherwise =
    trace ("Inserting new node abbrev f" ++ (show $ Map.size aMap) ++ " for " ++ show key)
    Map.insert key ("f" ++ (show $ Map.size aMap)) aMap
  where key = fullname $ text indText
  
----------------------------------------------------------------
-- |Returns an "abbreviated edge" in which long filenames generated from the passed IndentedText are replaced with
-- |abbreviations from the given map.
edgeFromTo :: IndentedText -> IndentedText -> Map.Map String String -> Edge

-- edgeFromTo f t m | trace ("edgeFrom " ++ show f ++ " " ++ show t ++ " " ++ show m) False = undefined
edgeFromTo aFrom aTo aMap = Edge { from = (aMap ! (fullname $ text aFrom)),
                                   to = (aMap ! (fullname $ text aTo))
                                 }

----------------------------------------------------------------
-- fullname :: (String,String,String,[String]) -> String
-- fullname (_,_,_,[_,fileName,directoryName]) = directoryName ++ fileName

-- |Return full filename from given string matching parseLineRegex
fullname :: String -> String
fullname aText =
  let matchv = fourth $ (aText =~ parseLineRegex :: (String,String,String,[String]))
  in
    if length matchv == 3
    then (matchv !! 2) ++ (matchv !! 1)
    else error ("Match failed on \"" ++ show aText ++ "\"")

----------------------------------------------------------------
-- |Returns a list of edges as strings, possibly with comments indicating occurrence counts > 1
edgeDump :: [(Edge,Int)]        -- ^ List of (edge,count) tuples
  -> [String]                   -- ^ List of edges, possibly w/comments

-- edgeDump a | trace ("edgedump " ++ show a) False = undefined
edgeDump [] = []

edgeDump ((edge,count):rest)
  | count <= 1  = edgeAsString edge:(edgeDump rest)
  | otherwise   = (edgeAsString edge ++ " [color=red] /* " ++ (show count) ++ " occurrences */"):(edgeDump rest)

----------------------------------------------------------------
-- |Renders an Edge as a string (but not using Show, since this is a one-way rendering for the purposes of whatever
-- |external process is going to consume this string.
edgeAsString :: Edge -> String
edgeAsString e = from e ++ " -> " ++ to e

----------------------------------------------------------------
-- |Return a printable list of strings representing the given Map entries (long filename -> abbreviation), in
-- |abbreviation order.
nodeDump :: [(String,String)] -> [String]
nodeDump nodeAbbrevs = map nodeXform nodeAbbrevs

----------------------------------------------------------------
-- |Printable node representation
nodeXform :: (String,String) -> String
nodeXform (long,short) = short ++ " is " ++ long

----------------------------------------------------------------
nodeSort :: (String,String) -> (String,String) -> Ordering
nodeSort a b
  | snd a < snd b = LT
  | snd a == snd b = EQ
  | otherwise = GT
  
----------------------------------------------------------------
first :: (a,b,c,d) -> a
first (x,_,_,_) = x

fourth :: (a,b,c,d) -> d
fourth (_,_,_,x) = x

