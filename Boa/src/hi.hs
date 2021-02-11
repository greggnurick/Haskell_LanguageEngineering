    codes = choice $ map string ["\'"]
    
    replacements $
      case res of
        "n" -> "\n"
    
    chars = escaped <|> noneOf "\""
    
    escaped = satisfy (\a -> isAscii a && codes)
    
    codes = choice $ map string ["\'"]
    
    replacements $
      case res of
        "n" -> "\n"
