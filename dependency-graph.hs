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
l1 = "    Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Processing SXA.Compass.Config.ViewModel.dll\tin C:\\Program Files (x86)\\Allscripts Sunrise\\Clinical Manager Client\\7.2.5575.0\\"
l2 = "    Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Adding C:\\Program Files (x86)\\Allscripts Sunrise\\Clinical Manager Client\\7.2.5575.0\\SXA.Compass.Config.ViewModel.dll\t(IsPresent=true)\tto assemblyList at beginning of GetAssemblyListEx()"
l3 = "      Helios.MigrationTool.Common.AssemblyUtils.GetAssemblyList() Information: 0 : Processing SXA.Compass.Config.Utils.dll\tin C:\\Program Files (x86)\\Allscripts Sunrise\\Clinical Manager Client\\7.2.5575.0\\"
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
parseLineRegex = "^( *)(.* Information: 0 : Processing )([^ ]*)[ \t]+in (.*)" -- 4 subexpressions

main :: IO()
main = do
  logContents <- getContents
  putStrLn $ unlines $ fst $ edges (lines logContents) Map.empty

----------------------------------------------------------------
-- |Returns a list of strings describing edges in the form "a -> b /* comment */"
edges :: 
  [String]              -- ^ Input lines of text
  -> Map.Map String Int -- ^ Map of edges in form "a -> b" with a count of the number of times that edge occurs
  -> ([String],         -- ^ Output list of edge descriptions
      Int)              -- ^ Number of lines processed
  
edges [] edgeSet =
  (edgeDump $ Map.assocs edgeSet, 0)
  
edges (lastLine:[]) edgeSet =
  (edgeDump $ Map.assocs edgeSet, 1)

edges (fstLogLine:sndLogLine:logLines) edgeSet =
  let fstFields = fstLogLine =~ parseLineRegex :: (String,String,String,[String])
      sndFields = sndLogLine =~ parseLineRegex :: (String,String,String,[String])
  in
    if length (fourth fstFields) == 0
    then error ("Unmatched: " ++ (first fstFields))
    else if length (fourth sndFields) == 0 -- "Adding", not "Processing"
    then edges (fstLogLine:logLines) edgeSet -- Skip useless line
    else if indentLength fstFields >= indentLength sndFields
    then edges (sndLogLine:logLines) edgeSet -- Can't be an edge from first to second line; drop first line and keep going.
    else ((fst $ edges (sndLogLine:logLines) edgeSet)
           ++ (fst $ edges (fstLogLine:(drop
                                        (snd $ edges (sndLogLine:logLines) edgeSet) -- # of lines processed
                                        logLines)) edgeSet),
          (snd $ edges (sndLogLine:logLines) edgeSet)
          + (snd $ edges (fstLogLine:(drop
                                      (snd $ edges (sndLogLine:logLines) edgeSet) -- # of lines processed
                                      logLines)) edgeSet)
         )

----------------------------------------------------------------
fullname :: (String,String,String,[String]) -> String
fullname (_,_,_,[_,_,fileName,directoryName]) = directoryName ++ fileName

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

