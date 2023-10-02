namespace Compiler
{
    public enum VariableType
    {
        Integer,
        Boolean,
        Void
    }
    public struct VariableToken
    {
        public string name;
        public VariableType type;
        public VariableToken(VariableType type, string name)
        {
            this.name = name;
            this.type = type;
        }
    }
    public struct FunctionToken
    {
        public string name;
        public VariableType type;
        public List<VariableType> parameters;
        public FunctionToken(VariableType type, string name, List<VariableType> parameters)
        {
            this.name = name;
            this.type = type;
            this.parameters = parameters;
        }
    }

    // --------------------------------------------------------------------- //
    // --------------------------------------------------------------------- //



    public class Parser
    {
        private List<Token> tokens;
        private int index;
        private Stack<VariableToken> variables;
        private List<FunctionToken> functions;
        public Parser(List<Token> tokens)
        {
            this.tokens = tokens;
            variables = new();
            functions = new();
        }
        NodeTerm ParseTerm()
        {
            if (Peek().type == TokenType.int_lit)
            {
                return new(new NodeTermIntLit(Consume()));
            }
            else if (Peek().type == TokenType.bool_lit)
            {
                return new(new NodeTermBoolLit(Consume()));
            }
            else if (Peek().type == TokenType.ident)
            {
                Token ident = Consume();

                VariableToken variable = GetVariable(ident.value!, ident.line);
                return new(new NodeTermIdent(variable.type, variable));
            }
            else if (Peek().type == TokenType.open_paren)
            {
                Consume(); // Consume '('
                NodeExpr nodeExpr = ParseExpr();
                Consume(); // Consume ')'
                return new(new NodeTermParen(nodeExpr.type, nodeExpr));
            }
            else if (Peek().type == TokenType.exlamation_mark)
            {
                Token token = Consume(); // Consume '!'
                NodeExpr nodeExpr = ParseExpr();
                if (nodeExpr.type != VariableType.Boolean)
                {
                    Compiler.Error($"Invalid type {nodeExpr.type}. Expected {VariableType.Boolean}.", token.line);
                }
                return new(new NodeTermInvert(nodeExpr));
            }
            Compiler.Error("Invalid term", Peek().line);
            return default;
        }
        NodeExpr ParseExpr(int minPrecidence = 0)
        {
            NodeTerm termLhs = ParseTerm();

            NodeExpr nodeExprLhs = new()
            {
                type = termLhs.var.Type,
                var = termLhs
            };

            while (true)
            {
                Token currentToken = Peek();
                int precidence;

                if (!TokensLeft())
                {
                    break;
                };

                precidence = GetPrecedence(currentToken.type);
                if (precidence < minPrecidence)
                {
                    break;
                }

                Token op = Consume();
                int nextMinPrecidence = precidence + 1;
                NodeExpr nodeExprRhs = ParseExpr(nextMinPrecidence);

                NodeExprBin expr;
                expr.expr = op.type switch
                {
                    TokenType.plus => new NodeBinExprAdd(nodeExprLhs, nodeExprRhs),
                    TokenType.minus => new NodeBinExprSub(nodeExprLhs, nodeExprRhs),
                    TokenType.star => new NodeBinExprMult(nodeExprLhs, nodeExprRhs),
                    TokenType.slash => new NodeBinExprDiv(nodeExprLhs, nodeExprRhs),
                    TokenType.is_equal => new NodeBinExprIsEqual(nodeExprLhs, nodeExprRhs),
                    TokenType.is_not_equal => new NodeBinExprIsNotEqual(nodeExprLhs, nodeExprRhs),
                    TokenType.less_than => new NodeBinExprLessThan(nodeExprLhs, nodeExprRhs),
                    TokenType.greater_than => new NodeBinExprGreaterThan(nodeExprLhs, nodeExprRhs),
                    TokenType.less_than_or_equal_to => new NodeBinExprLessThanOrEqualTo(nodeExprLhs, nodeExprRhs),
                    TokenType.greater_than_or_equal_to => new NodeBinExprGreaterThanOrEqualTo(nodeExprLhs, nodeExprRhs),
                    TokenType.or => new NodeBinExprOr(nodeExprLhs, nodeExprRhs),
                    TokenType.and => new NodeBinExprAnd(nodeExprLhs, nodeExprRhs),
                    TokenType.hat => new NodeBinExprXor(nodeExprLhs, nodeExprRhs),
                    _ => null!
                };
                if (expr.expr == null)
                {
                    Compiler.Error("Invalid operand", op.line);
                }
                nodeExprLhs.var = expr;
                nodeExprLhs.type = expr.expr!.Type;
            }
            return nodeExprLhs;
        }
        NodeScope ParseScope()
        {
            List<NodeStmt> stmts = new();

            Consume();
            while (true)
            {
                NodeStmt? stmt = ParseStmt(true);
                if (stmt == null)
                {
                    break;
                }
                stmts.Add(stmt!.Value);
            }
            if (!TokensLeft())
            {
                Compiler.Error("Scope must be closed");
            }
            TryConsume(TokenType.close_curly);

            return new(stmts);
        }
        NodeStmtExit ParseStmtExit()
        {
            Consume();
            TryConsume(TokenType.open_paren);
            NodeExpr expr = ParseExpr();
            TryConsume(TokenType.close_paren);
            TryConsume(TokenType.semicolon);

            return new(expr);
        }
        NodeStmtAssignment ParseStmtAssignment(bool needSemicolon = true)
        {
            Token ident = Consume();
            TryConsume(TokenType.equal);
            NodeExpr expr = ParseExpr();
            if (needSemicolon) TryConsume(TokenType.semicolon);

            VariableToken variable = GetVariable(ident.value!, ident.line);

            if (expr.type != variable.type)
            {
                Compiler.Error($"Invalid type {expr.type}. Expected {variable.type}.", ident.line);
            }

            return new(ident, expr);
        }
        NodeStmtDeclaration ParseStmtDeclaration(VariableType? type)
        {
            Consume();
            Token ident = Consume();
            TryConsume(TokenType.equal);

            NodeExpr expr = ParseExpr();
            if (type != null && expr.type != type)
            {
                Compiler.Error($"Invalid type {expr.type}. Expected {type}.", ident.line);
            }

            TryConsume(TokenType.semicolon);

            variables.Push(new(expr.type, ident.value!));

            return new(ident, expr);
        }
        NodeStmtIf ParseStmtIf()
        {
            Token ifToken = Consume();
            TryConsume(TokenType.open_paren);

            NodeExpr expr = ParseExpr();
            if (expr.type != VariableType.Boolean)
            {
                Compiler.Error($"Invalid type {expr.type}. Expected Boolean.", ifToken.line);
            }

            TryConsume(TokenType.close_paren);

            NodeScope scope = ParseScope();

            return new(expr, scope);
        }
        NodeStmtWhile ParseStmtWhile()
        {
            Token whileToken = Consume();
            TryConsume(TokenType.open_paren);

            NodeExpr expr = ParseExpr();
            if (expr.type != VariableType.Boolean)
            {
                Compiler.Error($"Invalid type {expr.type}. Expected Boolean.", whileToken.line);
            }

            TryConsume(TokenType.close_paren);

            NodeScope scope = ParseScope();

            return new(expr, scope);
        }
        NodeStmtFor ParseStmtFor()
        {
            Token forToken = Consume();
            TryConsume(TokenType.open_paren);

            NodeStmtDeclaration decl = ParseStmtDeclaration(null);

            NodeExpr expr = ParseExpr();
            TryConsume(TokenType.semicolon);
            if (expr.type != VariableType.Boolean)
            {
                Compiler.Error($"Invalid type {expr.type}. Expected Boolean.", forToken.line);
            }

            NodeStmtAssignment assign = ParseStmtAssignment(false);
            TryConsume(TokenType.close_paren);

            NodeScope scope = ParseScope();

            return new(decl, expr, assign, scope);
        }
        NodeStmtFunctionDeclaration ParseStmtFunctionDeclaration()
        {
            List<VariableToken> parameters = new();
            List<VariableType> parameterTypes = new();

            Token functionKeyword = Consume();

            // Get return type
            TokenType type = Consume().type;
            if (!IsTypeToken(type))
            {
                Compiler.Error($"Token {Peek().type} is not valid in a function declaration", functionKeyword.line);
            }

            // Get function name
            Token ident = Consume();
            if (ident.type != TokenType.ident)
            {
                Compiler.Error($"Expected function name", ident.line);
            }

            TryConsume(TokenType.open_paren);

            // Get parameters
            while (true)
            {
                if (Peek().type == TokenType.close_paren)
                {
                    Consume();
                    break;
                }

                Token typeToken = Consume();
                if (!IsTypeToken(typeToken.type))
                {
                    Compiler.Error($"Invalid token {typeToken.type}, expected type token", typeToken.line);
                }

                Token identToken = Consume();
                if (identToken.type != TokenType.ident)
                {
                    Compiler.Error($"Invalid token {identToken.type}, expected identifier", identToken.line);
                }

                parameters.Add(new(ToVariableType(typeToken.type), identToken.value!));
                parameterTypes.Add(ToVariableType(typeToken.type));
                variables.Push(new(ToVariableType(typeToken.type), identToken.value!));

                if (Peek().type == TokenType.close_paren)
                {
                    Consume();
                    break;
                }
                TryConsume(TokenType.comma);
            }

            NodeScope scope = ParseScope();

            functions.Add(new(ToVariableType(type), ident.value!, parameterTypes));
            return new(ident.value!, parameters, scope);
        }
        NodeStmtFunctionCall ParseStmtFunctionCall()
        {
            List<NodeExpr> parameters = new();

            Token ident = Consume();

            if (ident.type != TokenType.ident)
            {
                Compiler.Error($"Invalid token {ident.type}, expected identifier", ident.line);
            }

            TryConsume(TokenType.open_paren);

            FunctionToken functionToken = GetFunction(ident.value!, ident.line);
            for (int i = 0; i < functionToken.parameters.Count; i++)
            {
                VariableType parameter = functionToken.parameters[i];

                NodeExpr expr = ParseExpr();
                if (expr.type != parameter)
                {
                    Compiler.Error($"Invalid type {expr.type}, expected {parameter}");
                }
                parameters.Add(expr);
                if (Peek().type == TokenType.close_paren)
                {
                    if (i < functionToken.parameters.Count - 1)
                    {
                        Compiler.Error($"Missing parameters", Peek().line);
                    }
                    break;
                }
                TryConsume(TokenType.comma);
            }
            TryConsume(TokenType.close_paren);
            TryConsume(TokenType.semicolon);

            return new(ident.value!, functionToken.type, parameters);
        }
        NodeStmtReturn ParseReturn(bool isInFunction)
        {
            if (!isInFunction)
            {
                Compiler.Error("Invalid statement 'return' outside of function definition.");
            }
            Consume();
            NodeExpr expr = ParseExpr();
            TryConsume(TokenType.semicolon);
            return new(expr);
        }
        INodeStmt ParseIdent()
        {
            switch (Peek(1).type)
            {
                case TokenType.open_paren:
                    return ParseStmtFunctionCall();
                case TokenType.equal:
                    return ParseStmtAssignment();
                default:
                    Compiler.Error("Invalid statement. Expected function or assignment.", Peek().line);
                    return default!; // This is never reached

            }
        }
        NodeStmt? ParseStmt(bool isInFunction)
        {
            if (!TokensLeft())
            {
                return null;
            }
            return Peek().type switch
            {
                /* 
                    struct definition
                    function definition
                    function call
                    variable declaration
                    variable definition
                    variable assignment
                    scope
                    if statement
                    while loop
                    for loop
                    return statement
                */
                TokenType._exit => new(ParseStmtExit()),
                TokenType._int => new(ParseStmtDeclaration(VariableType.Integer)),
                TokenType._bool => new(ParseStmtDeclaration(VariableType.Boolean)),
                TokenType.ident => new(ParseIdent()),
                TokenType._if => new(ParseStmtIf()),
                TokenType._while => new(ParseStmtWhile()),
                TokenType._for => new(ParseStmtFor()),
                TokenType._function => new(ParseStmtFunctionDeclaration()),
                TokenType.open_curly => new(ParseScope()),
                TokenType._return => new(ParseReturn(isInFunction)),
                _ => null,
            };

        }
        public NodeProg Parse()
        {
            NodeProg prog = new();
            index = 0;

            while (TokensLeft())
            {
                NodeStmt? stmt = ParseStmt(false);
                if (stmt == null)
                {
                    Compiler.Error("Invalid statement");
                }
                prog.stmts.Add(stmt!.Value);
            }

            return prog;
        }

        static int GetPrecedence(TokenType token)
        {
            return token switch
            {
                TokenType.plus or TokenType.minus => 2,
                TokenType.star or TokenType.slash => 3,
                TokenType.is_equal or TokenType.is_not_equal or
                TokenType.less_than or TokenType.less_than_or_equal_to or
                TokenType.greater_than or TokenType.greater_than_or_equal_to => 1,
                TokenType.or or TokenType.and or TokenType.hat => 0,
                _ => -1
            };
        }

        private void TryConsume(TokenType token)
        {
            if (!TokensLeft()) Compiler.Error("Unexpected end of file");
            if (Peek().type == token)
            {
                Consume();
            }
            else
            {
                Compiler.Error($"Expected {token}, found {Peek().type}", Peek().line);
            }
        }
        private bool TokensLeft(int ahead = 0)
        {
            return tokens.Count > index + ahead;
        }
        private Token Peek(int ahead = 0)
        {
            if (TokensLeft())
            {
                return tokens[index + ahead];
            }

            return default;
        }
        private Token Consume()
        {
            Token c = tokens[index];
            index++;
            return c;
        }
        VariableToken GetVariable(string variableName, int line)
        {
            foreach (VariableToken variable in variables)
            {
                if (variable.name == variableName)
                {
                    return variable;
                }
            }
            Compiler.Error("Variable does not exist", line);
            return default;
        }
        FunctionToken GetFunction(string functionName, int line)
        {
            foreach (FunctionToken function in functions)
            {
                if (function.name == functionName)
                {
                    return function;
                }
            }
            Compiler.Error("Function does not exist", line);
            return default;
        }

        static bool IsTypeToken(TokenType type)
        {
            return type switch
            {
                TokenType._int or TokenType._bool => true,
                _ => false
            };
        }
        static VariableType ToVariableType(TokenType type)
        {
            return type switch
            {
                TokenType._int => VariableType.Integer,
                TokenType._bool => VariableType.Boolean,
                TokenType._void => VariableType.Void,
                _ => default
            };
        }
        static TokenType ToTokenType(VariableType type)
        {
            return type switch
            {
                VariableType.Integer => TokenType._int,
                VariableType.Boolean => TokenType._bool,
                VariableType.Void => TokenType._void,
                _ => default
            };
        }
    }
}