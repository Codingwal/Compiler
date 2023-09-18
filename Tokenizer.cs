namespace Compiler
{
    public enum TokenType
    {
        // Keywords
        _exit,
        _if,
        _while,

        // Types
        _int,
        _bool,

        // user defined names and literals
        int_lit,
        bool_lit,
        ident,

        // basic symbols
        semicolon,
        open_paren,
        close_paren,
        open_curly,
        close_curly,
        equal,

        // Arithmetic symbols
        plus,
        minus,
        star,
        slash,
        is_equal,
        is_not_equal,
        less_than,
        greater_than,
        less_than_or_equal_to,
        greater_than_or_equal_to,

        // Logic symbols
        or,
        and,
        hat,
        exlamation_mark
    }
    public struct Token
    {
        public TokenType type;
        public int line;
        public string? value;
        public Token(TokenType type, int line, string value = "")
        {
            this.type = type;
            this.line = line;
            this.value = value;
        }
    }
    public class Tokenizer
    {
        int currentLine;
        private int index;
        private readonly string str;
        private List<Token> tokens;
        private Dictionary<string, TokenType> keywords = new()
        {
            {"exit", TokenType._exit},
            {"if", TokenType._if},
            {"while", TokenType._while},
            {"int", TokenType._int},
            {"bool", TokenType._bool},
        };
        public static Dictionary<string, TokenType> symbols = new()
        {
            {";", TokenType.semicolon},
            {"(", TokenType.open_paren},
            {")", TokenType.close_paren},
            {"{", TokenType.open_curly},
            {"}", TokenType.close_curly},
            {"=", TokenType.equal},

            {"+", TokenType.plus},
            {"-", TokenType.minus},
            {"*", TokenType.star},
            {"/", TokenType.slash},
            {"==", TokenType.is_equal},
            {"!=", TokenType.is_not_equal},
            {"<", TokenType.less_than},
            {">", TokenType.greater_than},
            {"<=", TokenType.less_than_or_equal_to},
            {">=", TokenType.greater_than_or_equal_to},

            {"||", TokenType.or},
            {"&&", TokenType.and},
            {"^", TokenType.hat},
            {"!", TokenType.exlamation_mark},
        };

        public Tokenizer(string str)
        {
            tokens = new();
            currentLine = 1;
            index = 0;
            this.str = str;
        }
        public List<Token> Tokenize()
        {
            index = 0;
            tokens = new();

            while (TokensLeft())
            {
                string buffer = "";

                if (Peek() == '/' && TokensLeft(1) && Peek(1) == '*')
                {
                    Consume();
                    Consume();

                    while (TokensLeft())
                    {
                        char c = Consume();
                        if (c == '*')
                        {
                            if (TokensLeft() && Peek() == '/')
                            {
                                Consume();
                                break;
                            }
                        }
                    }
                }
                if (Peek() == '/' && TokensLeft(1) && Peek(1) == '/')
                {
                    Consume();
                    Consume();

                    while (TokensLeft())
                    {
                        char c = Consume();
                        if (c == '\n')
                        {
                            break;
                        }
                    }
                }
                else if (Peek() == '\n')
                {
                    currentLine++;
                    Consume();
                }
                else if (char.IsLetter(Peek()))
                {
                    buffer += Consume();
                    while (TokensLeft() && (char.IsLetterOrDigit(Peek()) || Peek() == '_'))
                    {
                        buffer += Consume();
                    }
                    AddKeywordToken(buffer);
                }
                else if (char.IsDigit(Peek()))
                {
                    buffer += Consume();
                    while (TokensLeft() && char.IsDigit(Peek()))
                    {
                        buffer += Consume();
                    }
                    AddIntLitToken(buffer);
                }
                else if (IsUsedSymbol(Peek()))
                {
                    buffer += Consume();
                    while (TokensLeft() && IsUsedSymbol(Peek()))
                    {
                        buffer += Consume();
                    }
                    AddSymbolToken(buffer);
                }
                else if (char.IsWhiteSpace(Peek()))
                {
                    Consume();
                }
                else
                {
                    Compiler.Error("Invalid char: " + Peek(), currentLine);
                }
            }

            return tokens;
        }

        private void AddKeywordToken(string str)
        {
            if (keywords.ContainsKey(str))
            {
                tokens.Add(new(keywords[str], currentLine));
            }
            else
            {
                if (str == "true") tokens.Add(new(TokenType.bool_lit, currentLine, "1"));
                else if (str == "false") tokens.Add(new(TokenType.bool_lit, currentLine, "0"));
                else tokens.Add(new(TokenType.ident, currentLine, str));
            }
        }
        private void AddIntLitToken(string int_lit)
        {
            tokens.Add(new(TokenType.int_lit, currentLine, int_lit));
        }
        private void AddSymbolToken(string symbol)
        {
            if (!symbols.ContainsKey(symbol))
            {
                foreach (char c in symbol)
                {
                    if (!symbols.ContainsKey(c.ToString()))
                    {
                        Compiler.Error($"Invalid symbol: '{symbol}'", currentLine);

                    }
                    tokens.Add(new(symbols[c.ToString()], currentLine));
                }
                return;
            }
            tokens.Add(new(symbols[symbol], currentLine));
        }

        private static bool IsUsedSymbol(char c)
        {
            return c switch
            {
                ';' or '(' or ')' or '{' or '}'
                or '=' or '+' or '-' or '*' or '/'
                or '!' or '<' or '>' or '|' or '&' or '^' => true,
                _ => false,
            };

        }

        private bool TokensLeft(int ahead = 0)
        {
            return str.Length > index + ahead;
        }
        private char Peek(int ahead = 0)
        {
            if (TokensLeft())
            {
                return str[index + ahead];
            }
            Compiler.Error("Unexpected end of file", -1);
            return default;
        }
        private char Consume()
        {
            char c = str[index];
            index++;
            return c;
        }
    }
}