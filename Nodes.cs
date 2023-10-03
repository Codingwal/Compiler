namespace Compiler
{
    public interface INodeTerm { }
    public struct NodeTermIdent : INodeTerm
    {
        public string ident;
        public NodeTermIdent(string ident)
        {
            this.ident = ident;
        }
    }
    public struct NodeTermIntLit : INodeTerm
    {
        public Token int_lit;
        public NodeTermIntLit(Token int_lit)
        {
            this.int_lit = int_lit;
        }
    }
    public struct NodeTermBoolLit : INodeTerm
    {
        public Token bool_lit;
        public NodeTermBoolLit(Token bool_lit)
        {
            this.bool_lit = bool_lit;
        }
    }
    public struct NodeTermParen : INodeTerm
    {
        public NodeExpr expr;
        public NodeTermParen(NodeExpr expr)
        {
            this.expr = expr;
        }
    }
    public struct NodeTermInvert : INodeTerm
    {
        public NodeExpr expr;
        public NodeTermInvert(NodeExpr expr)
        {
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
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
    }
    public struct NodeBinExprAdd : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprAdd(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprSub : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprSub(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprMult : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprMult(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprDiv : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprDiv(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprIsEqual : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprIsEqual(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprIsNotEqual : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprIsNotEqual(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprLessThan : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprLessThan(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprGreaterThan : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprGreaterThan(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprLessThanOrEqualTo : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprLessThanOrEqualTo(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprGreaterThanOrEqualTo : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprGreaterThanOrEqualTo(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprOr : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprOr(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprAnd : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprAnd(NodeExpr lhs, NodeExpr rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }
    }
    public struct NodeBinExprXor : INodeExprBin
    {
        public NodeExpr Lhs { get; set; }
        public NodeExpr Rhs { get; set; }
        public NodeBinExprXor(NodeExpr lhs, NodeExpr rhs)
        {
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
        public INodeExpr var;
    }
    // --------------------------------------------------------------------- //


    // --------------------------------------------------------------------- //
    public interface INodeStmt { }
    public interface INodeStmtProg { }
    public interface INodeStmtStruct { }
    public interface INodeStmtFunction { }
    public struct NodeStmtVariableDeclaration : INodeStmt, INodeStmtStruct
    {
        public string name;
        public string type;
        public NodeStmtVariableDeclaration(string name, string type)
        {
            this.name = name;
            this.type = type;
        }
    }

    public struct NodeStmtVariableDefinition : INodeStmt, INodeStmtProg, INodeStmtFunction
    {
        public string name;
        public string type;
        public NodeExpr expr;
        public NodeStmtVariableDefinition(string name, string type, NodeExpr expr)
        {
            this.name = name;
            this.type = type;
            this.expr = expr;
        }
    }
    public struct NodeStmtVariableAssignment : INodeStmt, INodeStmtProg, INodeStmtFunction
    {
        public string name;
        public NodeExpr expr;
        public NodeStmtVariableAssignment(string name, NodeExpr expr)
        {
            this.name = name;
            this.expr = expr;
        }
    }
    public struct NodeScope : INodeStmt, INodeStmtProg, INodeStmtFunction
    {
        public List<NodeStmt> stmts;
        public NodeScope(List<NodeStmt> stmts)
        {
            this.stmts = stmts;
        }
    }
    public struct NodeStmtFunctionDefinition : INodeStmt, INodeStmtProg
    {
        public string name;
        public List<NodeStmtVariableDeclaration> parameters;
        public NodeScope scope;
        public string returnType;
        public NodeStmtFunctionDefinition(string name, List<NodeStmtVariableDeclaration> parameters, NodeScope scope, string returnType)
        {
            this.name = name;
            this.parameters = parameters;
            this.scope = scope;
            this.returnType = returnType;
        }
    }
    public struct NodeStmtFunctionCall : INodeStmt, INodeStmtProg, INodeStmtFunction
    {
        public string name;
        public List<NodeExpr> parameters;
        public NodeStmtFunctionCall(string name, List<NodeExpr> parameters)
        {
            this.name = name;
            this.parameters = parameters;
        }
    }
    public struct NodeStmtReturn : INodeStmt, INodeStmtFunction
    {
        public NodeExpr expr;
        public NodeStmtReturn(NodeExpr expr)
        {
            this.expr = expr;
        }
    }
    public struct NodeStmtStructDefinition : INodeStmt, INodeStmtProg
    {
        public string name;
        public NodeScope scope;
        public NodeStmtStructDefinition(string name, NodeScope scope)
        {
            this.name = name;
            this.scope = scope;
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
        public NodeStmtVariableDefinition definition;
        public NodeExpr expr;
        public NodeStmtVariableAssignment assign;
        public NodeScope scope;
        public NodeStmtFor(NodeStmtVariableDefinition definition, NodeExpr expr, NodeStmtVariableAssignment assign, NodeScope scope)
        {
            this.definition = definition;
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