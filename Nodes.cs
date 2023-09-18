namespace Compiler
{
    public interface INodeTerm
    {
        public VariableType Type { get; set; }
    }
    public struct NodeTermIdent : INodeTerm
    {
        public VariableType Type { get; set; }
        public VariableToken ident;
        public NodeTermIdent(VariableType type, VariableToken ident)
        {
            Type = type;
            this.ident = ident;
        }
    }
    public struct NodeTermIntLit : INodeTerm
    {
        public VariableType Type { get; set; }
        public Token int_lit;
        public NodeTermIntLit(Token int_lit)
        {
            Type = VariableType.Integer;
            this.int_lit = int_lit;
        }
    }
    public struct NodeTermBoolLit : INodeTerm
    {
        public VariableType Type { get; set; }
        public Token bool_lit;
        public NodeTermBoolLit(Token bool_lit)
        {
            Type = VariableType.Boolean;
            this.bool_lit = bool_lit;
        }
    }
    public struct NodeTermParen : INodeTerm
    {
        public VariableType Type { get; set; }
        public NodeExpr expr;
        public NodeTermParen(VariableType type, NodeExpr expr)
        {
            Type = type;
            this.expr = expr;
        }
    }
    public struct NodeTermInvert : INodeTerm
    {
        public VariableType Type { get; set; }
        public NodeExpr expr;
        public NodeTermInvert(NodeExpr expr)
        {
            Type = VariableType.Boolean;
            this.expr = expr;
        }
    }
    public struct NodeTerm : INodeExpr
    {
        public INodeTerm var;
        public NodeTerm(INodeTerm var)
        {
            this.var = var;
        }
    }
    // --------------------------------------------------------------------- //
    public interface INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
    }
    public struct NodeBinExprAdd : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprAdd(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Integer)
            {
                Compiler.Error($"Operator '+' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }

            Type = lhs.type;
            Lhs = lhs;
            Rhs = rhs;

        }
    }
    public struct NodeBinExprSub : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprSub(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Integer)
            {
                Compiler.Error($"Operator '-' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }

            Type = lhs.type;
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprMult : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprMult(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Integer)
            {
                Compiler.Error($"Operator '*' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }

            Type = lhs.type;
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprDiv : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprDiv(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Integer)
            {
                Compiler.Error($"Operator '/' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }

            Type = lhs.type;
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprIsEqual : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprIsEqual(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type)
            {
                Compiler.Error($"Operator '==' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprIsNotEqual : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprIsNotEqual(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type)
            {
                Compiler.Error($"Operator '!=' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprLessThan : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprLessThan(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Integer)
            {
                Compiler.Error($"Operator '<' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprGreaterThan : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprGreaterThan(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Integer)
            {
                Compiler.Error($"Operator '>' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprLessThanOrEqualTo : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprLessThanOrEqualTo(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Integer)
            {
                Compiler.Error($"Operator '<=' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprGreaterThanOrEqualTo : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprGreaterThanOrEqualTo(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Integer)
            {
                Compiler.Error($"Operator '>=' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprOr : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprOr(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Boolean)
            {
                Compiler.Error($"Operator '||' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprAnd : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprAnd(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type || lhs.type != VariableType.Boolean)
            {
                Compiler.Error($"Operator '&&' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprXor : INodeExprBin
    {
        public VariableType Type { get; set; }
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprXor(NodeExpr lhs, NodeExpr rhs)
        {
            if (lhs.type != rhs.type)
            {
                Compiler.Error($"Operator '^' cannot be applied to operands of type '{lhs.type}' and '{rhs.type}'");
            }
            Type = VariableType.Boolean;

            Lhs = lhs;
            Rhs = rhs;
        }
    }

    public struct NodeExprBin : INodeExpr
    {
        public INodeExprBin expr;
    }

    // --------------------------------------------------------------------- //
    public interface INodeExpr { }
    public struct NodeExpr
    {
        public VariableType type;
        public INodeExpr var;
    }
    // --------------------------------------------------------------------- //
    public struct NodeScope : INodeStmt
    {
        public List<NodeStmt> stmts;
        public NodeScope(List<NodeStmt> stmts)
        {
            this.stmts = stmts;
        }
    }

    // --------------------------------------------------------------------- //
    public interface INodeStmt { }
    public struct NodeStmtAssignment : INodeStmt
    {
        public Token ident;
        public VariableType type;
        public NodeExpr expr;
        public NodeStmtAssignment(Token ident, NodeExpr expr)
        {
            this.ident = ident;
            type = expr.type;
            this.expr = expr;
        }
    }
    public struct NodeStmtDeclaration : INodeStmt
    {
        public Token ident;
        public VariableType type;
        public NodeExpr expr;
        public NodeStmtDeclaration(Token ident, NodeExpr expr)
        {
            this.ident = ident;
            type = expr.type;
            this.expr = expr;
        }
    }
    public struct NodeStmtExit : INodeStmt
    {
        public NodeExpr expr;
        public NodeStmtExit(NodeExpr expr)
        {
            this.expr = expr;
        }
    }
    public struct NodeStmtIf : INodeStmt
    {
        public NodeExpr expr;
        public NodeScope scope;
        public NodeStmtIf(NodeExpr expr, NodeScope scope)
        {
            this.expr = expr;
            this.scope = scope;
        }
    }
    public struct NodeStmtWhile : INodeStmt
    {
        public NodeExpr expr;
        public NodeScope scope;
        public NodeStmtWhile(NodeExpr expr, NodeScope scope)
        {
            this.expr = expr;
            this.scope = scope;
        }
    }
    public struct NodeStmtFor : INodeStmt
    {
        public NodeStmtDeclaration decl;
        public NodeExpr expr;
        public NodeStmtAssignment assign;
        public NodeScope scope;
        public NodeStmtFor(NodeStmtDeclaration decl, NodeExpr expr, NodeStmtAssignment assign, NodeScope scope)
        {
            this.decl = decl;
            this.expr = expr;
            this.assign = assign;
            this.scope = scope;
        }
    }
    public struct NodeStmt
    {
        public INodeStmt var;
        public NodeStmt(INodeStmt var)
        {
            this.var = var;
        }
    }

    // --------------------------------------------------------------------- //
    public struct NodeProg
    {
        public List<NodeStmt> stmts;
        public NodeProg()
        {
            stmts = new();
        }
    }
}