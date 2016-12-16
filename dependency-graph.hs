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
-- 
-- Algorithm:
--      Read first line, parse, remember indentation
--      Repeat for other lines, but if indentation increases, store pair A -> B in hashset.
--      At end, dump out hashset.

-- import Debug.Trace
-- import System.Environment
-- import System.Console.GetOpt
-- import Data.Maybe (fromMaybe)
-- import Data.List.Split
import Prelude -- hiding (readFile) -- Because we want the System.IO.Strict version
-- import System.IO (hPutStr, hPutStrLn, stderr)
-- import System.IO.Strict
-- import Control.Monad
-- import System.Directory
-- import System.FilePath
import Text.Regex.TDFA
-- import Text.Regex.TDFA.String
-- import Text.Printf

-- import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as Map

---------------------------------------------------------------- Test Data
-- l1 = "    Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Processing SXA.Compass.Config.ViewModel.dll\tin C:\\Program Files (x86)\\Allscripts Sunrise\\Clinical Manager Client\\7.2.5575.0\\"
-- l2 = "    Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Adding C:\\Program Files (x86)\\Allscripts Sunrise\\Clinical Manager Client\\7.2.5575.0\\SXA.Compass.Config.ViewModel.dll\t(IsPresent=true)\tto assemblyList at beginning of GetAssemblyListEx()"
-- l3 = "      Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Processing SXA.Compass.Config.Utils.dll\tin C:\\Program Files (x86)\\Allscripts Sunrise\\Clinical Manager Client\\7.2.5575.0\\"
---------------------------------------------------------------- Test Data Ends
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
parseLineRegex = "^(.* Information: 0 : Processing )([^ ]*)[ \t]+in (.*)" -- 3subexpressions

main :: IO()
main = do
  logContents <- getContents
  putStrLn $ unlines $ process (map parseIndent $ lines logContents) $ State {stack=[], edges=Map.empty}

-- So, we need a "state" class that contains: a stack of lines, and a set of edges.  When indentation increases, add to
-- stack, when it decreases, pop from stack.

-- |A line of indented text.
data IndentedText = IndentedText { indent :: Int, -- ^ The indent (expected to be a number of spaces)
                                   text :: String -- ^ The text (expected not to start with a space).
                                 }

-- |Internal pgm state
data State = State { stack :: [IndentedText],     -- ^ A stack of indented log lines, each at a higher indent level than
                                                  -- the previous.  Head is top of stack.
                     edges :: Map.Map String Int  -- ^ A set of edges in form "a -> b" with frequency counts.
                   }
             

----------------------------------------------------------------
-- |Parses out the leading indentation of the given String into a string of spaces and the rest of the line
parseIndent :: String -> IndentedText
parseIndent s =
  let matchv = (fourth $ (s =~ "^( *)(.*)" :: (String,String,String,[String])))
  in
    IndentedText { indent = length $ matchv !! 0, text = matchv !! 1 }

----------------------------------------------------------------
process :: [IndentedText] -> State -> [String]

process [] state = edgeDump $ Map.assocs $ edges state

process (line:rest) state = process rest $ newState state line  

----------------------------------------------------------------
newState :: State -> IndentedText -> State

newState state curLine
  | indent curLine > (indent $ head $ stack state)
    -- indented
    = State { stack = (curLine:(stack state)),
              edges = (Map.insertWith (+) (edgeFromTo (head $ stack state) curLine) 1 (edges state))
            }
  | indent curLine == (indent $ head $ stack state)
    -- same level
    -- Pop prev. stack head, replace with current line, add edges from prev prev head to current line
    --   a <-- prev prev
    --     b <-- prev
    --     c <-- current
    = State { stack = (curLine:(tail $ stack state)), -- pop one
              edges = (Map.insertWith (+) (edgeFromTo (head $ tail $ stack state) curLine) 1 (edges state))
            }
  | indent curLine < (indent $ head $ stack state)
    -- outdented
    -- pop to line w/less indent than current, then proceed from there
    = State { stack = (curLine:prevStack),
              edges = (Map.insertWith (+) (edgeFromTo (head prevStack) curLine) 1 (edges state))
            }
  where prevStack = (dropWhile greaterIndent (stack state)) -- Could use in "==" case?
        greaterIndent stackLine = (indent curLine) >= (indent stackLine)

----------------------------------------------------------------
edgeFromTo :: IndentedText -> IndentedText -> String

edgeFromTo from to = (text from) ++ " -> " ++ (text to)
-- ----------------------------------------------------------------
-- -- |Returns a list of strings describing edges in the form "a -> b /* comment */"
-- process :: 
--   [(String,String)]             -- ^ Input tuples: (indent, restOfString)
--   -> Map.Map String Int -- ^ Map of edges in form "a -> b" with a count of the number of times that edge occurs
--   -> [String]           -- ^ Output list of edge descriptions in form "a -> b optionalExtraText"
--   
-- process [] edgeSet =
--   (edgeDump $ Map.assocs edgeSet, 0)
--   
-- process (lastLine:[]) edgeSet =
--   (edgeDump $ Map.assocs edgeSet, 1)
-- 
-- process (fstLogLine:sndLogLine:[]) edgeSet =
--   let fstFields = (snd fstLogLine) =~ parseLineRegex :: (String,String,String,[String])
--       sndFields = (snd sndLogLine) =~ parseLineRegex :: (String,String,String,[String])
--   in
--     if length (fourth fstFields) == 0
--     then error ("Unmatched: " ++ (first fstFields)) -- First line must always match
--     else if length (fourth sndFields) == 0 -- "Adding", not "Processing"
--     then process (fstLogLine:[]) edgeSet -- Skip useless line
--     else if indentLength fstLogLine >= indentLength sndLogLine
--     then process (sndLogLine:[]) edgeSet -- Can't be an edge from first to second line; drop first line and keep going.
--     else process (sndLogLine:[])
--          (Map.insertWith (+) ((fullName fstFields) ++ (fullName sndFields)) 1)
--   
-- process (fstLogLine:sndLogLine:thdLogLine:logLines) edgeSet =
--   let fstFields = (snd fstLogLine) =~ parseLineRegex :: (String,String,String,[String])
--       sndFields = (snd sndLogLine) =~ parseLineRegex :: (String,String,String,[String])
--       thdFields = (snd thdLogLine) =~ parseLineRegex :: (String,String,String,[String])
--   in
--     if length (fourth fstFields) == 0
--     then error ("Unmatched: " ++ (first fstFields)) -- First line must always match
-- 
--     else if length (fourth sndFields) == 0 -- "Adding", not "Processing"
--     then process (fstLogLine:thdLogLine:logLines) edgeSet -- Skip useless line
-- 
--     else if indentLength fstLogLine >= indentLength sndLogLine
--     then []                     -- Stop processing at outdent
-- 
--     else
--       -- Looking one of:
--       --       1
--       --          2 -- process 1 -> 2, then process 2.. as subtree
--       --             3 -- Need to process as subtree rooted at 2, then drop subtree (zero or more lines at same level as 3)
--       -- or
--       --       1
--       --          2 -- processs, then drop this line (process 2.. as empty subtree?)
--       --          3
--       -- or
--       --       1
--       --          2 -- process, then drop this line (drop entire subtree rooted at 1) (same as above, drop empty subtree? (2))
--       --       3
--       -- or
--       --       1
--       --          2 -- same as above? Drop empty subtree rooted at 2
--       --    3
--       process (sndLogLine:thdLogLine:logLines) (Map.insertWith (+) ((fullName fstFields) ++ (fullName sndFields)) 1) -- now what? I need to pass the UPDATED edgeSet on to the next call, after the subtree rooted at 2 is dropped.
--       
--       
-- 
--     then process (sndLogLine:logLines) edgeSet -- Can't be an edge from first to second line; drop first line and keep going.
--     else process (sndLogLine:(takeWhile (increasingIndent $ length $ fst fstLogLine) logLines))
--          (Map.insertWith (+) ((fullName fstFields) ++ (fullName sndFields)) 1)
--     else ((fst $ process (sndLogLine:logLines) edgeSet)
--            ++ (fst $ process (fstLogLine:(drop
--                                         (snd $ process (sndLogLine:logLines) edgeSet) -- # of lines processed
--                                         logLines)) edgeSet),
--           (snd $ process (sndLogLine:logLines) edgeSet)
--           + (snd $ process (fstLogLine:(drop
--                                       (snd $ process (sndLogLine:logLines) edgeSet) -- # of lines processed
--                                       logLines)) edgeSet)
--          )

----------------------------------------------------------------
fullname :: (String,String,String,[String]) -> String
fullname (_,_,_,[_,fileName,directoryName]) = directoryName ++ fileName

----------------------------------------------------------------
-- |Edges from the first line to all following lines
edgesFrom :: String             -- ^ First line
  -> [String]                   -- ^ Following lines
  -> Map.Map String Int         -- ^ Set of edges built so far
  -> [String]
edgesFrom a b c = []

----------------------------------------------------------------
-- |Return length of indent or error
indentLength :: (String,String,String,[String]) -- ^ Regex match context
  -> Int                                        -- ^ Length of indent
indentLength (prefix,_,_,[]) = error $ "Not matched: " ++ prefix
indentLength (_,_,_,subexprs) =
  length $ subexprs !! 0

----------------------------------------------------------------
-- |Returns a list of edges, possibly with comments indicating occurrence counts > 1
edgeDump :: [(String,Int)]     -- ^ List of (edge,count) tuples
  -> [String]                  -- ^ List of edges, possibly w/comments
edgeDump [] = []
edgeDump ((edge,count):rest)
  | count <= 1  = edge:(edgeDump rest)
  | otherwise   = (edge ++ " /* " ++ (show count) ++ " occurrences */"):(edgeDump rest)

----------------------------------------------------------------
first :: (a,b,c,d) -> a
first (x,_,_,_) = x

fourth :: (a,b,c,d) -> d
fourth (_,_,_,x) = x

