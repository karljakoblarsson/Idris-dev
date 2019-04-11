{-|
Module      : IRTS.CodegenAgda
Description : The JavaScript code generator.

License     : BSD3
Maintainer  : The Idris Community.
-}
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module IRTS.CodegenAgda (codegenAgda
                             ) where

import Data.Char
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import IRTS.CodegenCommon
import IRTS.JavaScript.Codegen
import System.Directory
import System.FilePath

-- codegenJs :: CGConf -> CodeGenerator

{- data CodegenInfo = CodegenInfo { -}
{-     outputFile    :: String -}
{-   , outputType    :: OutputType -}
{-   , targetTriple  :: String -}
{-   , targetCPU     :: String -}
{-   , includes      :: [FilePath] -}
{-   , importDirs    :: [FilePath] -}
{-   , compileObjs   :: [String] -}
{-   , compileLibs   :: [String] -}
{-   , compilerFlags :: [String] -}
{-   , debugLevel    :: DbgLevel -}
{-   , simpleDecls   :: [(Name, SDecl)] -}
{-   , defunDecls    :: [(Name, DDecl)] -}
{-   , liftDecls     :: [(Name, LDecl)] -}
{-   , interfaces    :: Bool -}
{-   , exportDecls   :: [ExportIFace] -}
{-   , ttDecls       :: [(Name, TTDecl)] -}
{-   } -}

{- type CodeGenerator = CodegenInfo -> IO () -}
codegenAgda :: CodeGenerator
codegenAgda ci =
    TIO.writeFile (outputFile ci) $ T.concat $ intersperse ("\n" :: Text)
        [ "-- Start translated Agda code" :: Text ] ++
        map (T.pack . show) (liftDecls ci)
        ++ [ "-- End translated Agda code" :: Text
        ]


{- codegenJs :: CGConf -> CodeGenerator -}
{- codegenJs conf ci = -}
{-   do -}
{-     debug <- isYes <$> lookupEnv "IDRISJS_DEBUG" -}
{-     let defs' = Map.fromList $ liftDecls ci -}
{-     let defs = globlToCon defs' -}
{-     let iface = interfaces ci -}
{-     let used = if iface then -}
{-                   Map.elems $ removeDeadCode defs (getExpNames $ exportDecls ci) -}
{-                   else Map.elems $ removeDeadCode defs [sMN 0 "runMain"] -}
{-     when debug $ do -}
{-         writeFile (outputFile ci ++ ".LDeclsDebug") $ (unlines $ intersperse "" $ map show used) ++ "\n\n\n" -}
{-         putStrLn $ "Finished calculating used" -}

{-     let (out, stats) = doCodegen defs used -}

{-     path <- getIdrisJSRTSDir -}
{-     jsbn <- if usedBigInt stats -}
{-               then TIO.readFile $ path </> jsbnPath conf -}
{-               else return "" -}

{-     runtimeCommon <- TIO.readFile $ path </> "Runtime-common.js" -}
{-     extraRT <- TIO.readFile $ path </> (extraRunTime conf) -}

{-     includes <- getIncludes $ includes ci -}
{-     let libs = T.pack $ includeLibs $ compileLibs ci -}
{-     TIO.writeFile (outputFile ci) $ T.concat [ header conf -}
{-                                              , "\"use strict\";\n\n" -}
{-                                              , "(function(){\n\n" -}
{-                                              -- rts -}
{-                                              , runtimeCommon, "\n" -}
{-                                              , extraRT, "\n" -}
{-                                              , jsbn, "\n" -}
{-                                              -- external libraries -}
{-                                              , includes, "\n" -}
{-                                              , libs, "\n" -}
{-                                              -- user code -}
{-                                              , doPartials (partialApplications stats), "\n" -}
{-                                              , doHiddenClasses (hiddenClasses stats), "\n" -}
{-                                              , out, "\n" -}
{-                                              , if iface then T.concat ["module.exports = {\n", T.intercalate ",\n" $ concatMap (makeExportDecls defs) (exportDecls ci), "\n};\n"] -}
{-                                                   else jsName (sMN 0 "runMain") `T.append` "();\n" -}
{-                                              , "}.call(this))" -}
{-                                              , footer conf -}
{-                                              ] -}
