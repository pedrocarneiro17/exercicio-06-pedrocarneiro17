import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.Lexer
import L.L1.Frontend.RecursiveParser
import L.L1.Frontend.HappyParser (runHappyParser)
import L.L1.Frontend.Syntax
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V0.Instr
import Data.Maybe (listToMaybe) -- Adicionada importação para listToMaybe

import System.Environment
import System.FilePath
import System.Process

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

-- running the compiler / interpreter
runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    alexBasedLexer file
  [Recursive file] ->
    recursiveParser file 
  [LALR file] ->
    lalrParser file
  _ -> helpMessage

-- Implement the function to do lexical analysis for L1 programs
alexBasedLexer :: FilePath -> IO ()
alexBasedLexer file = do
  content <- readFile file
  let tokens = L.L1.Frontend.Lexer.lexer content
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
        TAssign     -> putStrLn $ "Atribuição := Linha:" ++ show line ++ " Coluna:" ++ show col
        TSemicolon  -> putStrLn $ "Ponto e vírgula ; Linha:" ++ show line ++ " Coluna:" ++ show col
        TComma      -> putStrLn $ "Vírgula , Linha:" ++ show line ++ " Coluna:" ++ show col
        TLParen     -> putStrLn $ "Parêntese ( Linha:" ++ show line ++ " Coluna:" ++ show col
        TRParen     -> putStrLn $ "Parêntese ) Linha:" ++ show line ++ " Coluna:" ++ show col
        TPlus       -> putStrLn $ "Operador + Linha:" ++ show line ++ " Coluna:" ++ show col
        TMinus      -> putStrLn $ "Operador - Linha:" ++ show line ++ " Coluna:" ++ show col
        TTimes      -> putStrLn $ "Operador * Linha:" ++ show line ++ " Coluna:" ++ show col
        TEOF        -> putStrLn $ "Fim de arquivo Linha:" ++ show line ++ " Coluna:" ++ show col

recursiveParser :: FilePath -> IO ()
recursiveParser file = do
  content <- readFile file
  case l1Parser content of
    Left errStr -> putStrLn errStr
    Right ast   -> putStr (printL1Tree ast)

-- Implement the LALR parser
lalrParser :: FilePath -> IO ()
lalrParser file = do
  content <- readFile file
  let tokens = L.L1.Frontend.Lexer.lexer content
  let ast = runHappyParser tokens
  putStr (printL1Tree ast)

-- help message
helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L1 language"
                       , "Usage: l1 [--lexer-only | --recursive | --lalr | --help]"
                       , "--lexer-only: does the lexical analysis of the input program using an Alex-based lexer."
                       , "--recursive: does the syntax analysis using a recursive descendent Megaparsec parser."
                       , "--lalr: does the syntax analysis using a LALR parser."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments
data Option
  = Help
  | Lexer FilePath
  | Recursive FilePath
  | LALR FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--recursive" : arg : _) -> [Recursive arg]
    ("--lalr" : arg : _) -> [LALR arg]
    _ -> [Help]