import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

lexico = T.makeTokenParser emptyDef

symbol = T.symbol lexico

tabela   = [[prefix "-" negate]
            , [binario "*" (*) AssocLeft, binario "/" (div) AssocLeft ]
            , [binario "+" (+) AssocLeft, binario "-" (-)   AssocLeft ]
           ]


binario  name fun assoc = Infix (do{symbol name; return fun }) assoc
prefix   name fun       = Prefix (do{symbol name; return fun })
   
expr    = buildExpressionParser tabela fator'
         <?> "expression"   
          
constante = do {d <- many1 digit; return (read d)}
            <?> "number"
    
   
fator    =  do {symbol "("; v <- expr; symbol ")"; return v}
            <|> constante
            <?> "term"
            
fator' = do {spaces; v <- fator; spaces; return v} 

partida :: Parsec String u Integer
partida = do {e <- expr; eof; return e}

parserE e = runParser partida [] "Expressoes" e

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> (print v)
                     
main = do putStr "Expressão:"
          e <- getLine 
          parserExpr e
