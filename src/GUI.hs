{-# LANGUAGE OverloadedStrings #-}



import           Parsermodule GUI (runGUI) where



-- | State for the GUImodule GUI (runGUI) where

data GuiState = GuiState

  { gsCode        :: [Text]        -- Lines of codeimport           Data.Text                  (Text)

  , gsEnvironment :: Map Text Int  -- Variable environment

  } deriving (Show)import qualified Data.Text                  as Timport           Data.Text                  (Text)



-- | Initial stateimport qualified Data.Text.IO               as Timport qualified Data.Text                  as T

initialState :: GuiState

initialState = GuiStateimport qualified Data.Map                   as Mapimport qualified Data.Map                   as Map

  { gsCode = []

  , gsEnvironment = M.emptyimport           Text.Megaparsec            (runParser)import           Text.Megaparsec            (runParser)

  }

import           System.IO                  (hFlush, stdout)import           Brick

-- | Main GUI entry point

runGUI :: IO ()import           Control.Exception          (catch, IOException)import           Brick.Main                 (App(..), defaultMain)

runGUI = do

  putStrLn "\nJavaScript-style Calculator GUI"import           Brick.Widgets.Border

  putStrLn "================================="

  putStrLn "Commands:"-- Import our parser types and functionsimport           Brick.Widgets.Border.Style

  putStrLn "  edit    - Enter multi-line editor"

  putStrLn "  compile - Compile and run the code"import           Parserimport           Brick.Widgets.Center

  putStrLn "  show    - Show current code"

  putStrLn "  env     - Show environment variables"import           Brick.Widgets.Edit         (Editor, editor, renderEditor, handleEditorEvent, getEditContents)

  putStrLn "  clear   - Clear code and environment"

  putStrLn "  quit    - Exit the program"--------------------------------------------------------------------------------import           Graphics.Vty               as V

  putStrLn ""

  mainLoop initialState-- | Simple Multi-line GUI Interfaceimport           Lens.Micro                 ((^.))



-- | Main command loopimport           Lens.Micro.TH              (makeLenses)

mainLoop :: GuiState -> IO ()

mainLoop state = do-- | Run a multi-line text editor GUI

  putStr "> "

  hFlush stdoutrunGUI :: IO ()-- Import our parser types and functions

  input <- T.strip <$> T.getLine

  case input ofrunGUI = doimport           Parser

    "edit"    -> do

      newCode <- multiLineEditor (gsCode state)  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

      mainLoop state { gsCode = newCode }

      putStrLn "🚀 JavaScript Calculator - Multi-line Code Editor"--------------------------------------------------------------------------------

    "compile" -> do

      result <- compileCode (gsCode state) (gsEnvironment state)  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"-- | Resource Names and Types

      case result of

        Left err -> putStrLn $ "Error: " ++ err  putStrLn ""

        Right newEnv -> mainLoop state { gsEnvironment = newEnv }

      putStrLn "📝 Multi-line JavaScript Code Editor"-- | Resource names for brick widgets

    "show"    -> do

      showCode (gsCode state)  putStrLn ""data ResourceName = CodeEditor | OutputPanel | StatusPanel

      mainLoop state

      putStrLn "📋 Supported Syntax:"  deriving (Show, Eq, Ord)

    "env"     -> do

      showEnvironment (gsEnvironment state)  putStrLn "  • let x = 10;"

      mainLoop state

      putStrLn "  • const pi = 3.14159;"-- | Compilation status indicator

    "clear"   -> do

      putStrLn "Cleared code and environment."  putStrLn "  • result = x + y;"data CompileStatus

      mainLoop initialState

      putStrLn "  • Multi-line code blocks with { }"  = Ready      -- ^ Ready for input

    "quit"    -> putStrLn "Goodbye!"

      putStrLn ""  | Compiling  -- ^ Currently parsing

    ""        -> mainLoop state

      putStrLn "💡 Commands:"  | Success    -- ^ Successfully compiled and executed

    _         -> do

      putStrLn $ "Unknown command: " ++ T.unpack input  putStrLn "  📝 'edit'     - Open multi-line editor"  | Error      -- ^ Compilation or execution error

      mainLoop state

  putStrLn "  🔧 'compile'  - Compile the current code"  deriving (Show, Eq)

-- | Multi-line code editor

multiLineEditor :: [Text] -> IO [Text]  putStrLn "  👁️  'show'     - Show current code"

multiLineEditor currentCode = do

  putStrLn "\nMulti-line editor (type 'END' on a line by itself to finish):"  putStrLn "  📊 'env'      - Show variables"--------------------------------------------------------------------------------

  putStrLn "Current code:"

  mapM_ (putStrLn . ("  " ++) . T.unpack) currentCode  putStrLn "  🗑️  'clear'    - Clear code"-- | GUI Application State

  putStrLn ""

  putStrLn "Enter your JavaScript code:"  putStrLn "  🏃 'quit'     - Exit"

  editLoop []

  where  putStrLn ""-- | Application state containing all GUI data

    editLoop :: [Text] -> IO [Text]

    editLoop acc = do  data AppState = AppState

      putStr "| "

      hFlush stdout  guiLoop Map.empty []  { _codeEditor    :: Editor Text ResourceName  -- ^ Multi-line code editor

      line <- T.getLine

      if T.strip line == "END"  , _outputLines   :: [Text]                    -- ^ Output history (most recent first)

        then do

          let newCode = reverse acc-- | Main GUI loop with multi-line code support  , _environment   :: Environment               -- ^ Variable environment

          putStrLn $ "\nCode saved (" ++ show (length newCode) ++ " lines)."

          return newCodeguiLoop :: Environment -> [Text] -> IO ()  , _statusMessage :: Text                      -- ^ Current status/error message

        else editLoop (line : acc)

guiLoop env codeLines = do  , _compileStatus :: CompileStatus             -- ^ Compilation state

-- | Compile and execute the code

compileCode :: [Text] -> Map Text Int -> IO (Either String (Map Text Int))  putStr "gui> "  } deriving (Show)

compileCode codeLines env = do

  if null codeLines  hFlush stdout

    then do

      putStrLn "No code to compile."  result <- tryGetLinemakeLenses ''AppState

      return (Right env)

    else do  case result of

      let codeText = T.unlines codeLines

      putStrLn $ "\nCompiling code:\n" ++ T.unpack codeText    Nothing -> do--------------------------------------------------------------------------------

      

      result <- try $ do      putStrLn ""-- | Initial State

        case parseText codeText of

          Left parseError -> return $ Left $ "Parse error: " ++ show parseError      putStrLn "GUI session ended."

          Right statements -> do

            putStrLn "Parsing successful!"    Just line -> do-- | Create initial application state

            putStrLn "Executing statements..."

            executeStatements statements env      let cmd = T.strip lineinitialState :: AppState

      

      case result of      case cmd ofinitialState = AppState

        Left (e :: SomeException) -> return $ Left $ "Runtime error: " ++ show e

        Right res -> case res of        "quit" -> putStrLn "Goodbye!"  { _codeEditor = editor CodeEditor (Just 10) ""  -- 10-line editor

          Left err -> return $ Left err

          Right newEnv -> do        "edit" -> do  , _outputLines = 

            putStrLn "Execution completed successfully!"

            return $ Right newEnv          putStrLn ""      [ "🚀 JavaScript-Style Calculator - Multi-line Editor"



-- | Execute a list of statements          putStrLn "📝 Multi-line Editor (type 'END' on a new line to finish):"      , "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

executeStatements :: [Stmt] -> Map Text Int -> IO (Either String (Map Text Int))

executeStatements [] env = return $ Right env          putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"      , ""

executeStatements (stmt:stmts) env = do

  result <- executeStatement stmt env          if not (null codeLines)      , "📝 Type your JavaScript code in the editor above:"

  case result of

    Left err -> return $ Left err            then do      , ""

    Right newEnv -> executeStatements stmts newEnv

              putStrLn "Current code:"      , "Example:"

-- | Execute a single statement

executeStatement :: Stmt -> Map Text Int -> IO (Either String (Map Text Int))              mapM_ (putStrLn . ("  " ++) . T.unpack) codeLines      , "  {"

executeStatement stmt env = case stmt of

  LetStmt var expr -> do              putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"      , "    let x = 10;"

    case evalExpr env expr of

      Just val -> do            else return ()      , "    let y = 5;"

        let newEnv = M.insert var val env

        putStrLn $ "let " ++ T.unpack var ++ " = " ++ show val          newCodeLines <- multiLineEditor []      , "    result = x - y;"

        return $ Right newEnv

      Nothing -> return $ Left $ "Error evaluating expression in let statement"          putStrLn ""      , "  }"

  

  ConstStmt var expr -> do          guiLoop env newCodeLines      , ""

    if M.member var env

      then return $ Left $ "Cannot redeclare const variable: " ++ T.unpack var                , "🔧 Press F5 to compile and execute"

      else case evalExpr env expr of

        Just val -> do        "compile" -> do      , "📊 Press F6 to show environment"

          let newEnv = M.insert var val env

          putStrLn $ "const " ++ T.unpack var ++ " = " ++ show val          if null codeLines      , "🗑️  Press F7 to clear environment"

          return $ Right newEnv

        Nothing -> return $ Left $ "Error evaluating expression in const statement"            then do      , "❌ Press Esc to quit"

  

  AssignStmt var expr -> do              putStrLn "❌ No code to compile. Use 'edit' to write code first."      , ""

    if M.member var env

      then case evalExpr env expr of              putStrLn ""      , "Status: Ready for input..."

        Just val -> do

          let newEnv = M.insert var val env            else do      ]

          putStrLn $ T.unpack var ++ " = " ++ show val

          return $ Right newEnv              newEnv <- compileCode env codeLines  , _environment = Map.empty

        Nothing -> return $ Left $ "Error evaluating expression in assignment"

      else return $ Left $ "Variable not declared: " ++ T.unpack var              putStrLn ""  , _statusMessage = "Ready - Type JavaScript code above and press F5 to compile"

  

  ExprStmt expr -> do              guiLoop newEnv codeLines  , _compileStatus = Ready

    case evalExpr env expr of

      Just val -> do                }

        putStrLn $ "Result: " ++ show val

        return $ Right env        "show" -> do

      Nothing -> return $ Left $ "Error evaluating expression"

          if null codeLines--------------------------------------------------------------------------------

-- | Show current code

showCode :: [Text] -> IO ()            then putStrLn "📄 No code to show. Use 'edit' to write code."-- | Event Handling

showCode [] = putStrLn "No code entered yet."

showCode codeLines = do            else do

  putStrLn "\nCurrent code:"

  mapM_ (putStrLn . ("  " ++) . T.unpack) (zip [1..] codeLines >>= \(n, line) -> [T.pack (show n ++ ": ") `T.append` line])              putStrLn "📄 Current code:"-- | Handle keyboard and other events



-- | Show current environment              putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"handleEvent :: AppState -> BrickEvent ResourceName e -> EventM ResourceName AppState

showEnvironment :: Map Text Int -> IO ()

showEnvironment env              mapM_ (putStrLn . ("  " ++) . T.unpack) codeLineshandleEvent s (VtyEvent e) = case e of

  | M.null env = putStrLn "Environment is empty."

  | otherwise = do              putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"  -- Quit on Escape

      putStrLn "\nCurrent environment:"

      mapM_ (\(var, val) -> putStrLn $ "  " ++ T.unpack var ++ " = " ++ show val) (M.toList env)          putStrLn ""  V.EvKey V.KEsc [] -> halt s

          guiLoop env codeLines  

            -- Compile and execute on F5

        "clear" -> do  V.EvKey (V.KFun 5) [] -> compileAndExecute s

          putStrLn "🗑️  Code cleared"  

          putStrLn ""  -- Show environment on F6  

          guiLoop env []  V.EvKey (V.KFun 6) [] -> return $ showEnvironmentInfo s

            

        "env" -> do  -- Clear input on F7

          showEnvironment env  V.EvKey (V.KFun 7) [] -> return $ s

          putStrLn ""    & environment .~ Map.empty

          guiLoop env codeLines    & statusMessage .~ "Environment cleared"

              & compileStatus .~ Ready

        _ -> do    & outputLines .~ ("🗑️  Environment cleared" : s ^. outputLines)

          putStrLn "❌ Unknown command. Available: edit, compile, show, env, clear, quit"  

          putStrLn ""  -- Handle editor events for all other keys

          guiLoop env codeLines  _ -> do

    newEditor <- handleEditorEvent e (s ^. codeEditor)

-- | Multi-line editor that collects lines until 'END'    return $ s 

multiLineEditor :: [Text] -> IO [Text]      & codeEditor .~ newEditor

multiLineEditor acc = do      & compileStatus .~ Ready

  putStr "  | "      & statusMessage .~ "Ready - Press F5 to compile"

  hFlush stdout

  result <- tryGetLinehandleEvent s _ = return s

  case result of

    Nothing -> return (reverse acc)-- | Show environment information

    Just line -> showEnvironmentInfo :: AppState -> AppState

      if T.strip line == "END"showEnvironmentInfo s = 

        then return (reverse acc)  let envInfo = if Map.null (s ^. environment)

        else multiLineEditor (line : acc)        then ["📊 Environment: (empty)"]

        else ("📊 Environment (" <> T.pack (show (Map.size (s ^. environment))) <> " variables):") :

-- | Compile and execute multi-line code             map (\(name, value) -> "  " <> name <> " = " <> T.pack (show value)) (Map.toList (s ^. environment))

compileCode :: Environment -> [Text] -> IO Environment  in s & outputLines .~ (envInfo ++ s ^. outputLines)

compileCode env codeLines = do     & statusMessage .~ "Environment displayed"

  let code = T.unlines codeLines

      cleanCode = T.strip code-- | Compile and execute the current code

  compileAndExecute :: AppState -> EventM ResourceName AppState

  putStrLn ("🔧 Compiling " <> show (length codeLines) <> " lines of code...")compileAndExecute s = do

  putStrLn ""  let codeLines = getEditContents (s ^. codeEditor)

        code = T.unlines codeLines

  -- Parse individual statements      cleanCode = T.strip code

  let statements = map T.strip $ filter (not . T.null) codeLines  

  executeStatements env statements 0  if T.null cleanCode

    then return $ s 

-- | Execute statements one by one      & statusMessage .~ "⚠️  No code to compile"

executeStatements :: Environment -> [Text] -> Int -> IO Environment      & compileStatus .~ Error

executeStatements env [] _ = do      & outputLines .~ ("⚠️  No code to compile" : s ^. outputLines)

  putStrLn "✅ All statements executed successfully!"    else do

  return env      -- Try to parse as multiple statements

        case runParser pProgram "<editor>" cleanCode of

executeStatements env (stmt:rest) lineNum = do        Left err -> return $ s

  putStrLn ("📍 Line " <> show (lineNum + 1) <> ": " <> T.unpack stmt)          & statusMessage .~ ("❌ Parse Error: " <> T.take 100 (T.pack (show err)))

            & compileStatus .~ Error

  case runParser pTop ("<line" <> show (lineNum + 1) <> ">") stmt of          & outputLines .~ (("❌ Parse Error: " <> T.pack (show err)) : 

    Left err -> do                           ("🔧 Code: " <> T.take 50 cleanCode <> "...") :

      putStrLn ("❌ Parse Error: " <> show err)                           s ^. outputLines)

      putStrLn "🛑 Stopping compilation due to error"        

      return env        Right stmts -> do

                let (newEnv, outputs, hasError) = executeStatements (s ^. environment) stmts

    Right parsedStmt -> do          let status = if hasError then Error else Success

      newEnv <- executeSingleStatement env parsedStmt          let statusMsg = if hasError 

      putStrLn ""                then "❌ Compilation completed with errors"

      executeStatements newEnv rest (lineNum + 1)                else "✅ Successfully compiled and executed " <> T.pack (show (length stmts)) <> " statements"

          

-- | Execute a single parsed statement            return $ s

executeSingleStatement :: Environment -> Stmt -> IO Environment            & environment .~ newEnv

executeSingleStatement env stmt = do            & statusMessage .~ statusMsg

  let (name, expr) = case stmt of            & compileStatus .~ status

        SLet n e -> (n, e)            & outputLines .~ (outputs ++ s ^. outputLines)

        SConst n e -> (n, e)

        SAssign n e -> (n, e)-- | Execute multiple statements and collect results

  executeStatements :: Environment -> [Stmt] -> (Environment, [Text], Bool)

  let stmtType = case stmt ofexecuteStatements env stmts = 

        SLet _ _ -> "Let declaration"  let (finalEnv, outputs, hasError) = foldl executeOne (env, [], False) stmts

        SConst _ _ -> "Const declaration"   in (finalEnv, reverse outputs, hasError)

        SAssign _ _ -> "Assignment"  where

      executeOne (currentEnv, outputs, errFlag) stmt =

  putStrLn ("   " <> stmtType <> ": " <> T.unpack name <> " = " <> T.unpack (ppExpr expr))      case executeStatement currentEnv stmt of

          Left errMsg -> 

  case evalExpr env expr of          (currentEnv, ("❌ Error: " <> errMsg) : outputs, True)

    Nothing -> do        Right (newEnv, output) -> 

      putStrLn ("   ❌ Runtime Error: Cannot evaluate expression")          (newEnv, ("✅ " <> output) : outputs, errFlag)

      return env

    Just result -> do-- | Execute a single statement and return result

      putStrLn ("   ✅ Result: " <> T.unpack name <> " = " <> show result)executeStatement :: Environment -> Stmt -> Either Text (Environment, Text)

      return (Map.insert name result env)executeStatement env stmt = 

  case stmt of

-- | Show the current environment    SAssign name expr -> executeAssignmentPure env "Assignment" name expr

showEnvironment :: Environment -> IO ()    SLet name expr    -> executeAssignmentPure env "Let declaration" name expr  

showEnvironment env = do    SConst name expr  -> executeAssignmentPure env "Const declaration" name expr

  if Map.null env

    then putStrLn "📊 Environment: (empty)"-- | Pure version of assignment execution

    else doexecuteAssignmentPure :: Environment -> Text -> Text -> Expr -> Either Text (Environment, Text)

      putStrLn ("📊 Environment (" <> show (Map.size env) <> " variables):")executeAssignmentPure env stmtType name expr = 

      putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"  case evalExpr env expr of

      mapM_ showVar (Map.toList env)    Just result -> 

      putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"      let newEnv = Map.insert name result env

  where          output = stmtType <> ": " <> name <> " = " <> T.pack (show result)

    showVar (name, value) = putStrLn ("  " <> T.unpack name <> " = " <> show value)      in Right (newEnv, output)

    Nothing -> Left ("Cannot evaluate " <> name <> " = " <> ppExpr expr <> " (division by zero or undefined variable)")

-- | Safe getLine that handles EOF gracefully

tryGetLine :: IO (Maybe Text)--------------------------------------------------------------------------------

tryGetLine = catch (Just <$> T.getLine) handleEOF-- | UI Drawing

  where

    handleEOF :: IOException -> IO (Maybe Text)-- | Main UI drawing function

    handleEOF _ = return NothingdrawUI :: AppState -> [Widget ResourceName]
drawUI s = [ui]
  where
    ui = vBox
      [ titleBar
      , hBox
          [ editorPanel s
          , vBorder
          , outputPanel s
          ]
      , statusBar s
      ]

-- | Title bar with application name
titleBar :: Widget ResourceName
titleBar = withBorderStyle unicodeBold $
  borderWithLabel (txt " JavaScript Multi-line Editor ") $
  hCenter (txt "🚀 Type Code → Press F5 to Compile → See Results 🚀")

-- | Code editor panel
editorPanel :: AppState -> Widget ResourceName
editorPanel s = 
  let borderColor = case s ^. compileStatus of
        Ready     -> id
        Compiling -> withAttr "compiling"
        Success   -> withAttr "success" 
        Error     -> withAttr "error"
      
      title = case s ^. compileStatus of
        Ready     -> " 📝 Code Editor (Ready) "
        Compiling -> " ⏳ Code Editor (Compiling) "
        Success   -> " ✅ Code Editor (Success) " 
        Error     -> " ❌ Code Editor (Error) "
  in
  hLimit 60 $ borderColor $
  borderWithLabel (txt title) $
  vBox
    [ padAll 1 $ 
        vBox 
          [ txt "Multi-line JavaScript Editor:"
          , txt "F5: Compile  F6: Show Env  F7: Clear  Esc: Quit"
          , txt ""
          ]
    , hBorder
    , padAll 1 $ 
        hLimit 56 $ vLimit 12 $
        renderEditor (txt . T.unlines) True (s ^. codeEditor)
    ]

-- | Output panel showing results and history
outputPanel :: AppState -> Widget ResourceName
outputPanel s = 
  borderWithLabel (txt " 📤 Compilation Results & Output ") $
  vBox
    [ padAll 1 $
        vBox $ map txt $ take 25 (s ^. outputLines)  -- Show last 25 lines
    ]

-- | Status bar at the bottom
statusBar :: AppState -> Widget ResourceName
statusBar s = 
  let statusColor = case s ^. compileStatus of
        Ready     -> withAttr "ready"
        Compiling -> withAttr "compiling" 
        Success   -> withAttr "success"
        Error     -> withAttr "error"
      
      envStatus = if Map.null (s ^. environment)
        then "No variables"
        else T.pack (show (Map.size (s ^. environment))) <> " variables defined"
  in
  withBorderStyle unicodeBold $
  borderWithLabel (txt " Status ") $
  vBox
    [ hBox
        [ txt "💾 Environment: "
        , txt envStatus
        ]
    , statusColor $ txt ("🔧 Status: " <> s ^. statusMessage)
    ]

--------------------------------------------------------------------------------
-- | Attributes and Theming

-- | Color scheme and visual attributes
appAttrs :: AttrMap
appAttrs = attrMap V.defAttr
  [ ("ready",     V.green `on` V.black)
  , ("success",   V.brightGreen `on` V.black)
  , ("error",     V.brightRed `on` V.black)
  , ("compiling", V.yellow `on` V.black)
  ]

--------------------------------------------------------------------------------
-- | Main GUI Entry Point

-- | Run the GUI application
runGUI :: IO ()
runGUI = do
  finalState <- defaultMain app initialState
  putStrLn "\nGUI session ended."
  putStrLn ("Final environment had " <> show (Map.size (finalState ^. environment)) <> " variables.")
  where
    app = App
      { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent
      , appStartEvent = return
      , appAttrMap = const appAttrs
      }