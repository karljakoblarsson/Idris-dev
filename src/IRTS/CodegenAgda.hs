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
import Data.Text (Text)
import qualified Data.Text as T
import IRTS.CodegenCommon
import IRTS.JavaScript.Codegen
import System.Directory
import System.FilePath

htmlHeader :: Text
htmlHeader =
  T.concat [ "<html>\n"
           , " <head>\n"
           , "  <meta charset='utf-8'>\n"
           , " </head>\n"
           , " <body>\n"
           , "  <script type='text/javascript'>\n"
           ]

htmlFooter :: Text
htmlFooter =
  T.concat [ "\n  </script>\n"
           , " </body>\n"
           , "</html>"
           ]

codegenJavaScript :: CodeGenerator
codegenJavaScript ci =
  let (h, f) = if (map toLower $ takeExtension $ outputFile ci) == ".html" then
                  (htmlHeader, htmlFooter)
                  else ("","")
  in codegenJs (CGConf { header = h
                       , footer = f
                       , jsbnPath = "jsbn/jsbn-browser.js"
                       , extraRunTime = "Runtime-javascript.js"
                       }
               )
               ci

-- codegenJs :: CGConf -> CodeGenerator

codegenAgda :: CodeGenerator
codegenAgda ci =
  let (h, f) = if (map toLower $ takeExtension $ outputFile ci) == ".html" then
                  (htmlHeader, htmlFooter)
                  else ("","")
  in codegenJs (CGConf { header = "Begin translated Agda code"
                       , footer = "End translated Agda code"
                       }
               )
               ci

codegenNode :: CodeGenerator
codegenNode ci =
  if interfaces ci then
    codegenJs (CGConf { header = ""
                      , footer = ""
                      , jsbnPath = "jsbn/jsbn-browser.js"
                      , extraRunTime = "Runtime-node.js"
                      }
              )
              ci
    else
      do
        codegenJs (CGConf { header = "#!/usr/bin/env node\n"
                          , footer = ""
                          , jsbnPath = "jsbn/jsbn-browser.js"
                          , extraRunTime = "Runtime-node.js"
                          }
                  )
                  ci
        setPermissions (outputFile ci) (emptyPermissions { readable   = True
                                                         , executable = True
                                                         , writable   = True
                                                         })
