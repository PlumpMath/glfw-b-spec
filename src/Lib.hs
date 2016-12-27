{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-} -- For standalone Eq instance of CExternalDeclaration.
module Lib
    ( someFunc
    ) where
--stdlib
import Control.Arrow ((&&&))
import System.Environment(getProgName, getArgs)
import Text.Printf (printf)
import qualified Data.Ord as Ord
--thirdparty
import Language.C.System.GCC (newGCC)
import Text.Show.Pretty (ppShow, pPrint)
import qualified Data.Heap as Heap
import qualified Language.C as C
import qualified Language.C.Comments as CC
import qualified Language.Haskell.Exts as HS

-- make a map from GLFW.hs mapping c'glfw* functions which are called to the *exposed* haskell function(s)
-- for each function in glfw3.h
--  - examine the doxygen comment for threading, reentrace, and other information
--  - lookup in the map to find the exposing haskell function, and its signature
--  - generate a function which calls the exposing haskell function, but wraps it in a type which contains the threading, reenentrance, other information

someFunc :: IO ()
someFunc = do
    prog <- getProgName
    args <- getArgs
    (c, hs) <- case args of
        [] -> error $ printf "USAGE: %s ...include/GLFW/glfw3.h" prog
        [cHeader, hsModule] -> do
            c <- parseC cHeader
            hs <- parseHs hsModule
            return (c, hs)
    return ()

-- hs source parsing

parseHs :: FilePath -> IO HS.Module
parseHs path = do
    result <- HS.parseFile path
    return $ case result of
        HS.ParseOk m -> m
        f@(HS.ParseFailed _ _) -> error $ show f

-- c source parsing w/comments

parseC :: FilePath -> IO [CSource]
parseC path = do
    C.CTranslUnit declarations _ <- either (error . show) id <$> parseAST path
    comments <- CC.comments path
    return $ mergeC (sameFile declarations) comments
    where
        parseAST = C.parseCFile (newGCC "gcc") Nothing []
        sameFile = filter $ ((pure path) ==) . C.fileOfNode

mergeC :: [C.CExternalDeclaration C.NodeInfo] -> [CC.Comment] -> [CSource]
mergeC ds cs = Heap.toAscList (Heap.union dh ch :: Heap.MinHeap CSource)
    where
        dh = Heap.fromAscList $ Declaration <$> ds
        ch = Heap.fromAscList $ Comment <$> cs

data CSource
    = Declaration (C.CExternalDeclaration C.NodeInfo)
    | Comment CC.Comment
    deriving
    ( Eq
    , Show
    )

isComment :: CSource -> Bool
isComment (Comment _) = True
isComment _ = False

position :: CSource -> C.Position
position (Declaration d) = C.posOfNode $ C.nodeInfo d
position (Comment c) = CC.commentPosition c

fileLineColumn :: C.Position -> (String, (Int, Int))
fileLineColumn = (C.posFile &&& C.posRow &&& C.posColumn)

instance Eq (C.CExternalDeclaration C.NodeInfo) where
    _ /= _ = error "Cannot compare code for equality."

instance Ord CSource where
    compare = Ord.comparing $ fileLineColumn . position
