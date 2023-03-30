import Text.Parsec
import Type

partida :: Parsec String u Integer
partida = do {e <- expr; eof; return e}

expr = do {spaces; expr'}

expr' = do {char '+'; e1 <- expr; e2 <- expr; return (e1 + e2)}
        <|> do {char '-'; e1 <- expr; e2 <- expr; return (e1 - e2)}
        <|> do {char '*'; e1 <- expr; e2 <- expr; return (e1 * e2)}
        <|> do {char '+'; e1 <- expr; e2 <- expr; return (div e1 e2)}
        <|> do {constante}
       
          
constante = do {d <- many1 digit; return (read d)}

parserEP e = runParser partida [] "Expressões pre-fixadas" e

parserExpr s = case parserEP s of
                     Left er -> print er
                     Right v -> (print "resultado" >> print v)
