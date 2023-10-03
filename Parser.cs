namespace Compiler
{
    public class Parser
    {
        private List<Token> tokens;
        private int index;
        public Parser(List<Token> tokens)
        {
            this.tokens = tokens;
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

                return new(new NodeTermIdent(ident.value!));
            }
            else if (Peek().type == TokenType.open_paren)
            {
                TryConsume(TokenType.open_paren);

                NodeExpr nodeExpr = ParseExpr();

                TryConsume(TokenType.close_paren);

                return new(new NodeTermParen(nodeExpr));
            }
            else if (Peek().type == TokenType.exlamation_mark)
            {
                TryConsume(TokenType.exlamation_mark);

                NodeExpr nodeExpr = ParseExpr();

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
            }
            return nodeExprLhs;
        }
        NodeStmtVariableDeclaration ParseStmtVariableDeclaration(bool consumeSemicolon = true)
        {
            Token typeToken = Consume();

            Token ident = Consume();
            if (ident.type != TokenType.ident)
            {
                Compiler.Error($"Expected identifier, found {ident.type}.", ident.line);
            }

            if (consumeSemicolon)
            {
                TryConsume(TokenType.semicolon);
            }

            return new(ident.value!, typeToken.value!);
        }
        NodeStmtVariableDefinition ParseStmtVariableDefinition()
        {
            Token typeToken = Consume();

            Token ident = Consume();

            TryConsume(TokenType.equal);

            NodeExpr expr = ParseExpr();

            return new(ident.value!, typeToken.value!, expr);
        }
        NodeStmtVariableAssignment ParseStmtVariableAssignment()
        {
            Token ident = Consume();

            TryConsume(TokenType.equal);

            NodeExpr expr = ParseExpr();

            return new(ident.value!, expr);
        }
        NodeStmtStructDefinition ParseStmtStructDefinition()
        {
            TryConsume(TokenType._struct);

            Token ident = Consume();
            if (ident.type != TokenType.ident)
            {
                Compiler.Error($"Expected identifier, found {ident.type}.", ident.line);
            }

            NodeScope scope = ParseScope(typeof(INodeStmtStruct));

            return new(ident.value!, scope);
        }
        NodeStmtFunctionDefinition ParseStmtFunctionDefinition()
        {
            Token typeToken = Consume();

            Token ident = Consume();
            if (ident.type != TokenType.ident)
            {
                Compiler.Error($"Expected identifier, found {ident.type}", ident.line);
            }

            TryConsume(TokenType.open_paren);

            List<NodeStmtVariableDeclaration> parameters = new();
            while (Peek().type != TokenType.close_paren)
            {
                parameters.Add(ParseStmtVariableDeclaration(false));

                if (Peek().type != TokenType.close_paren)
                {
                    TryConsume(TokenType.comma);
                }
            }
            TryConsume(TokenType.close_paren);

            NodeScope scope = ParseScope(typeof(INodeStmtFunction));

            return new(ident.value!, parameters, scope, typeToken.value!);
        }
        NodeStmtFunctionCall ParseStmtFunctionCall()
        {
            Token ident = Consume();
            if (ident.type != TokenType.ident)
            {
                Compiler.Error($"Expected identifier, found {ident.type}", ident.line);
            }

            TryConsume(TokenType.open_paren);

            List<NodeExpr> parameters = new();
            while (Peek().type != TokenType.close_paren)
            {
                parameters.Add(ParseExpr());

                if (Peek().type != TokenType.close_paren)
                {
                    TryConsume(TokenType.comma);
                }
            }
            TryConsume(TokenType.close_paren);

            TryConsume(TokenType.semicolon);

            return new(ident.value!, parameters);
        }
        NodeStmtReturn ParseStmtReturn()
        {
            Consume();

            NodeExpr expr = ParseExpr();

            TryConsume(TokenType.semicolon);

            return new(expr);
        }
        NodeScope ParseScope(Type requiredInterface)
        {
            List<NodeStmt> stmts = new();

            Consume();
            while (TokensLeft() && Peek().type != TokenType.close_curly)
            {
                NodeStmt? stmt = ParseStmt(requiredInterface);

                stmts.Add(stmt!.Value);
            }

            if (!TokensLeft())
            {
                Compiler.Error("Scope must be closed");
            }
            TryConsume(TokenType.close_curly);

            return new(stmts);
        }
        NodeStmtIf ParseStmtIf(Type requiredInterface)
        {
            Token ifToken = Consume();
            TryConsume(TokenType.open_paren);

            NodeExpr expr = ParseExpr();

            TryConsume(TokenType.close_paren);

            NodeScope scope = ParseScope(requiredInterface);

            return new(expr, scope);
        }
        NodeStmtWhile ParseStmtWhile(Type requiredInterface)
        {
            Token whileToken = Consume();
            TryConsume(TokenType.open_paren);

            NodeExpr expr = ParseExpr();

            TryConsume(TokenType.close_paren);

            NodeScope scope = ParseScope(requiredInterface);

            return new(expr, scope);
        }
        NodeStmtFor ParseStmtFor(Type requiredInterface)
        {
            Token forToken = Consume();
            TryConsume(TokenType.open_paren);

            NodeStmtVariableDefinition definition = ParseStmtVariableDefinition();

            NodeExpr expr = ParseExpr();

            TryConsume(TokenType.semicolon);

            NodeStmtVariableAssignment assign = ParseStmtVariableAssignment();
            TryConsume(TokenType.close_paren);

            NodeScope scope = ParseScope(requiredInterface);

            return new(definition, expr, assign, scope);
        }
        INodeStmt ParseIdent()
        {
            if (Peek(1).type == TokenType.equal)
            {
                // ident = [Expr];
                return ParseStmtVariableAssignment();
            }
            if (Peek(1).type == TokenType.open_paren)
            {
                // ident([Parameters]);
                return ParseStmtFunctionCall();
            }
            if (Peek(2).type == TokenType.equal)
            {
                // ident ident = [Expr];
                return ParseStmtVariableDefinition();
            }
            if (Peek(2).type == TokenType.open_paren)
            {
                // ident ident([Parameters]) { }
                return ParseStmtFunctionDefinition();
            }
            // ident ident;
            return ParseStmtVariableDeclaration();
        }
        NodeStmt ParseStmt(Type requiredInterface)
        {
            if (!TokensLeft())
            {
                Compiler.Error("Unexpected end of file.");
            }
            NodeStmt? nodeStmt = Peek().type switch
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
                TokenType._struct => new(ParseStmtStructDefinition()),
                TokenType.ident => new(ParseIdent()),
                TokenType.open_curly => new(ParseScope(requiredInterface)),
                TokenType._if => new(ParseStmtIf(requiredInterface)),
                TokenType._while => new(ParseStmtWhile(requiredInterface)),
                TokenType._for => new(ParseStmtFor(requiredInterface)),
                _ => null,
            };

            if (nodeStmt == null)
            {
                Compiler.Error("Invalid statement.");
            }

            // Check if the stmt implements the interface
            if (!requiredInterface.IsAssignableFrom(nodeStmt!.Value.var.GetType()))
            {
                Compiler.Error("Statement is not valid in this context.");
            }

            return nodeStmt.Value!;
        }
        public NodeProg Parse()
        {
            NodeProg prog = new();
            index = 0;

            while (TokensLeft())
            {
                NodeStmt stmt = ParseStmt(typeof(INodeStmtProg));

                prog.stmts.Add(stmt);
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
    }
}