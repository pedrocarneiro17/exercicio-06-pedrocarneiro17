
import L.L2.Frontend.Lexer
import L.L2.Frontend.HappyParser
import L.L2.Frontend.Syntax
import L.L2.Frontend.Semantic
import L.L2.Interpreter.Interp
import L.L2.Backend.V1Codegen
import L.L2.Backend.CCodegen
import Utils.Var
import Utils.Value
import Data.Maybe (listToMaybe)
import System.Environment
import System.FilePath
import System.Process
import qualified Data.Map as Map
import Control.Monad (foldM_)
import Text.PrettyPrint.HughesPJ (render, vcat, text)

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    lexerOnly file
  [Parser file] ->
    parserOnly file
  [Interpret file] ->
    interpret file
  [V1Compile file] ->
    v1compiler file
  [CCompile file] ->
    ccompiler file
  _ -> helpMessage

lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  content <- readFile file
  let tokens = L.L2.Frontend.Lexer.lexer content
  case any (\t -> case lexeme t of TError _ -> True; _ -> False) tokens of
    True -> do
      let maybeErrorToken = listToMaybe [t | t <- tokens, case lexeme t of TError _ -> True; _ -> False]
      case maybeErrorToken of
        Just errorToken -> do
          let (line, col) = pos errorToken
          putStrLn $ "Erro léxico: caractere inválido '" ++ (case lexeme errorToken of TError s -> s; _ -> "") ++ "' Linha:" ++ show line ++ " Coluna:" ++ show col
        Nothing -> putStrLn "Erro léxico: nenhum token de erro encontrado"
    False -> mapM_ printToken tokens
  where
    printToken :: Token -> IO ()
    printToken t = let (line, col) = pos t in
      case lexeme t of
        TIdent s    -> putStrLn $ "Identificador " ++ s ++ " Linha:" ++ show line ++ " Coluna:" ++ show col
        TNumber n   -> putStrLn $ "Número " ++ show n ++ " Linha:" ++ show line ++ " Coluna:" ++ show col
        TString s   -> putStrLn $ "String \"" ++ s ++ "\" Linha:" ++ show line ++ " Coluna:" ++ show col
        TRead       -> putStrLn $ "Palavra reservada read Linha:" ++ show line ++ " Coluna:" ++ show col
        TPrint      -> putStrLn $ "Palavra reservada print Linha:" ++ show line ++ " Coluna:" ++ show col
        TDef        -> putStrLn $ "Palavra reservada def Linha:" ++ show line ++ " Coluna:" ++ show col
        TIn         -> putStrLn $ "Palavra reservada in Linha:" ++ show line ++ " Coluna:" ++ show col
        TEnd        -> putStrLn $ "Palavra reservada end Linha:" ++ show line ++ " Coluna:" ++ show col
        TAssign     -> putStrLn $ "Atribuição := Linha:" ++ show line ++ " Coluna:" ++ show col
        TSemicolon  -> putStrLn $ "Ponto e vírgula ; Linha:" ++ show line ++ " Coluna:" ++ show col
        TComma      -> putStrLn $ "Vírgula , Linha:" ++ show line ++ " Coluna:" ++ show col
        TLParen     -> putStrLn $ "Parêntese ( Linha:" ++ show line ++ " Coluna:" ++ show col
        TRParen     -> putStrLn $ "Parêntese ) Linha:" ++ show line ++ " Coluna:" ++ show col
        TPlus       -> putStrLn $ "Operador + Linha:" ++ show line ++ " Coluna:" ++ show col
        TMinus      -> putStrLn $ "Operador - Linha:" ++ show line ++ " Coluna:" ++ show col
        TTimes      -> putStrLn $ "Operador * Linha:" ++ show line ++ " Coluna:" ++ show col
        TDiv        -> putStrLn $ "Operador / Linha:" ++ show line ++ " Coluna:" ++ show col
        TEOF        -> putStrLn $ "Fim de arquivo Linha:" ++ show line ++ " Coluna:" ++ show col

parserOnly :: FilePath -> IO ()
parserOnly file = do
  content <- readFile file
  let tokens = L.L2.Frontend.Lexer.lexer content
  case any (\t -> case lexeme t of TError _ -> True; _ -> False) tokens of
    True -> do
      let maybeErrorToken = listToMaybe [t | t <- tokens, case lexeme t of TError _ -> True; _ -> False]
      case maybeErrorToken of
        Just errorToken -> do
          let (line, col) = pos errorToken
          putStrLn $ "Erro léxico: caractere inválido '" ++ (case lexeme errorToken of TError s -> s; _ -> "") ++ "' Linha:" ++ show line ++ " Coluna:" ++ show col
        Nothing -> putStrLn "Erro léxico: nenhum token de erro encontrado"
    False -> case runL2Parser tokens of
      ast -> putStr (printL2Tree ast)

interpret :: FilePath -> IO ()
interpret file = do
  content <- readFile file
  let tokens = L.L2.Frontend.Lexer.lexer content
  case any (\t -> case lexeme t of TError _ -> True; _ -> False) tokens of
    True -> do
      let maybeErrorToken = listToMaybe [t | t <- tokens, case lexeme t of TError _ -> True; _ -> False]
      case maybeErrorToken of
        Just errorToken -> do
          let (line, col) = pos errorToken
          putStrLn $ "Erro léxico: caractere inválido '" ++ (case lexeme errorToken of TError s -> s; _ -> "") ++ "' Linha:" ++ show line ++ " Coluna:" ++ show col
        Nothing -> putStrLn "Erro léxico: nenhum token de erro encontrado"
    False -> case runL2Parser tokens of
      stmts -> do
        case checkSemantics stmts of
          Left err -> putStrLn $ "Erro semântico: " ++ err
          Right _ -> do
            result <- evalL2 stmts
            case result of
              Left err -> putStrLn $ "Erro de execução: " ++ err
              Right _ -> pure ()

v1compiler :: FilePath -> IO ()
v1compiler file = do
  content <- readFile file
  let tokens = L.L2.Frontend.Lexer.lexer content
  case any (\t -> case lexeme t of TError _ -> True; _ -> False) tokens of
    True -> do
      let maybeErrorToken = listToMaybe [t | t <- tokens, case lexeme t of TError _ -> True; _ -> False]
      case maybeErrorToken of
        Just errorToken -> do
          let (line, col) = pos errorToken
          putStrLn $ "Erro léxico: caractere inválido '" ++ (case lexeme errorToken of TError s -> s; _ -> "") ++ "' Linha:" ++ show line ++ " Coluna:" ++ show col
        Nothing -> putStrLn "Erro léxico: nenhum token de erro encontrado"
    False -> case runL2Parser tokens of
      stmts -> do
        putStrLn "Executando checkSemantics..."
        case checkSemantics stmts of
          Left err -> putStrLn $ "Erro semântico: " ++ err
          Right _ -> do
            let code = v1Codegen (L2 stmts)
            putStrLn $ render $ vcat $ map (text . show) code

ccompiler :: FilePath -> IO ()
ccompiler file = do
  content <- readFile file
  let tokens = L.L2.Frontend.Lexer.lexer content
  case any (\t -> case lexeme t of TError _ -> True; _ -> False) tokens of
    True -> do
      let maybeErrorToken = listToMaybe [t | t <- tokens, case lexeme t of TError _ -> True; _ -> False]
      case maybeErrorToken of
        Just errorToken -> do
          let (line, col) = pos errorToken
          putStrLn $ "Erro léxico: caractere inválido '" ++ (case lexeme errorToken of TError s -> s; _ -> "") ++ "' Linha:" ++ show line ++ " Coluna:" ++ show col
        Nothing -> putStrLn "Erro léxico: nenhum token de erro encontrado"
    False -> case runL2Parser tokens of
      stmts -> do
        case checkSemantics stmts of
          Left err -> putStrLn $ "Erro semântico: " ++ err
          Right _ -> do
            let cCode = cCodegen (L2 stmts)
            let outputFile = replaceExtension file "c"
            writeFile outputFile cCode
            putStrLn $ "Código C gerado em: " ++ outputFile
            -- Compilar com gcc
            callCommand $ "gcc -o " ++ replaceExtension file "out" ++ " " ++ outputFile
            putStrLn $ "Executável gerado em: " ++ replaceExtension file "out"

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L2 language"
                       , "Usage: l2 [--lexer-only | --parse-only | --interpret | --v1compile | --ccompile | --help]"
                       , "--lexer-only: does the lexical analysis of the input program."
                       , "--parse-only: does the syntax analysis of the input program."
                       , "--interpret: does the syntax analysis and interpret the input program."
                       , "--v1compile: does the syntax analysis and compiles to V1 instructions."
                       , "--ccompile: does the syntax analysis and compiles to C code."
                       , "--help: prints this help message."
                       ]

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Interpret FilePath
  | V1Compile FilePath
  | CCompile FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--interpret" : arg : _) -> [Interpret arg]
    ("--v1compile" : arg : _) -> [V1Compile arg]
    ("--ccompile" : arg : _) -> [CCompile arg]
    _ -> [Help]