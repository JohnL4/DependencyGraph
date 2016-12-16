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

import System.Environment
import Prelude -- hiding (readFile) -- Because we want the System.IO.Strict version
import System.IO
import Text.Regex.TDFA
import qualified Data.Map.Strict as Map

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

  putStrLn $ unlines $ process (map parseIndent $ lines logContents) $ State {stack=[], edges=Map.empty}

-- |A line of indented text.
data IndentedText = IndentedText { indent :: Int, -- ^ The indent (expected to be a number of spaces)
                                   text :: String -- ^ The text (expected not to start with a space).
                                 }
                    deriving (Show)

-- |Internal pgm state
data State = State { stack :: [IndentedText],     -- ^ A stack of indented log lines, each at a higher indent level than
                                                  -- ^ the previous.  Head is top of stack.
                     edges :: Map.Map String Int  -- ^ A set of edges in form "a -> b" with frequency counts.
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

process [] state = edgeDump $ Map.assocs $ edges state

process (line:rest) state = process rest $ newState state line  

----------------------------------------------------------------
-- |Update state while processing. Input (IndentedText) is a line from the log being processed.
newState :: State -> IndentedText -> State

newState state curLine
  | (length $ stack state) == 0 =
      -- initial state
      State { stack = [curLine],
              edges = edges state -- no change
            }
  | indent curLine > (indent $ head $ stack state) =
      -- indented
      State { stack = (curLine:(stack state)),
              edges = (Map.insertWith (+) (edgeFromTo (head $ stack state) curLine) 1 (edges state))
            }
  | indent curLine == (indent $ head $ stack state) =
      -- same level
      State { stack = (curLine:prevStack),
              edges = if (length prevStack) == 0 then
--                         trace "same indent; stack empty"
                        edges state
                      else
--                         trace ("same indent; length stack = " ++ (show $ length prevStack))
                        (Map.insertWith (+) (edgeFromTo (head prevStack) curLine) 1 (edges state))
            }
  | indent curLine < (indent $ head $ stack state) =
      -- outdented
      State { stack = (curLine:prevStack),
              edges = if length prevStack == 0 then
--                         trace "outdent; prevStack empty"
                        edges state -- No new edge, since the stack is empty
                      else
--                         trace ("outdent; length prevStack = " ++ (show $ length prevStack))
                        (Map.insertWith (+) (edgeFromTo (head prevStack) curLine) 1 (edges state))
            }
  where prevStack =
          -- Unwind the stack to where its top corresponds to a line with smaller indent than the current line.
--           trace "computing prevstack"
          (dropWhile greaterIndent (stack state)) -- Could use in "==" case?
        greaterIndent stackLine = (indent stackLine) >= (indent curLine) -- Could drop entire stack, returning []

----------------------------------------------------------------
edgeFromTo :: IndentedText -> IndentedText -> String

edgeFromTo from to = (text from) ++ " -> " ++ (text to)

----------------------------------------------------------------
fullname :: (String,String,String,[String]) -> String

fullname (_,_,_,[_,fileName,directoryName]) = directoryName ++ fileName

----------------------------------------------------------------
-- |Returns a list of edges, possibly with comments indicating occurrence counts > 1
edgeDump :: [(String,Int)]     -- ^ List of (edge,count) tuples
  -> [String]                  -- ^ List of edges, possibly w/comments

-- edgeDump a | trace ("edgedump " ++ show a) False = undefined
edgeDump [] = []

edgeDump ((edge,count):rest)
  | count <= 1  = edge:(edgeDump rest)
  | otherwise   = (edge ++ " /* " ++ (show count) ++ " occurrences */"):(edgeDump rest)

----------------------------------------------------------------
first :: (a,b,c,d) -> a
first (x,_,_,_) = x

fourth :: (a,b,c,d) -> d
fourth (_,_,_,x) = x

