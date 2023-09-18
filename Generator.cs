using System.Diagnostics;
using System.Reflection.Metadata;

namespace Compiler
{
    struct Variable
    {
        public string name;
        public VariableType type;
        public int stackPosition;
        public Variable(string name, VariableType type, int stackPosition)
        {
            this.name = name;
            this.type = type;
            this.stackPosition = stackPosition;
        }
    }
    public class Generator
    {
        NodeProg prog;
        string output;
        int stackSize = 0;
        Stack<Variable> variables;
        Stack<int> scopes;
        int ifStmtCount = 0;
        int whileLoopCount = 0;
        private Dictionary<VariableType, int> variableSizes = new()
        {
            {VariableType.Integer, 8},
            {VariableType.Boolean, 2}
        };
        public Generator(NodeProg prog)
        {
            this.prog = prog;
            output = "";
            variables = new();
            scopes = new();
        }
        void GenerateTerm(NodeTerm term)
        {
            Type termType = term.var.GetType();

            if (termType == typeof(NodeTermIntLit))
            {
                NodeTermIntLit termIntLit = (NodeTermIntLit)term.var;

                output += "\n    ; Load literal\n";
                output += $"    mov rax, {termIntLit.int_lit.value}\n";
                Push("rax", 8);
            }
            else if (termType == typeof(NodeTermBoolLit))
            {
                NodeTermBoolLit termBoolLit = (NodeTermBoolLit)term.var;

                output += "\n    ; Load literal\n";
                string value = termBoolLit.bool_lit.value switch
                {
                    "0" => "0",
                    "1" => "255",
                    _ => throw new("Invalid boolean value")
                };
                output += $"    mov ax, {value}\n";
                Push("ax", 2);
            }
            else if (termType == typeof(NodeTermIdent))
            {
                NodeTermIdent termIdent = (NodeTermIdent)term.var;

                string variableName = termIdent.ident.name;
                if (!DoesVariableExist(variableName))
                {
                    Compiler.Error($"Undeclared identifier: {variableName}");
                    return;
                }
                Variable variable = GetVariable(variableName);
                int variableSize = variableSizes[variable.type];
                int stackPos = stackSize - variable.stackPosition - variableSize;

                output += $"\n    ; Push variable of type {variable.type} to the top of the stack\n";
                Push($"{GetWord(variableSize)} [rsp + {stackPos}]", variableSize);
            }
            else if (termType == typeof(NodeTermParen))
            {
                NodeTermParen termParen = (NodeTermParen)term.var;
                GenerateExpr(termParen.expr);
            }
            else if (termType == typeof(NodeTermInvert))
            {
                NodeTermInvert termParen = (NodeTermInvert)term.var;
                GenerateExpr(termParen.expr);

                output += $"\n    ; Invert boolean\n";
                Pop("ax", 2);
                output += $"    not al\n";
                Push("ax", 2);
            }
            else
            {
                Compiler.Error("Invalid term");
            }
        }
        void GenerateBinExpr(NodeExprBin expr)
        {
            Type exprType = expr.expr.GetType();

            if (exprType == typeof(NodeBinExprAdd))
            {
                NodeBinExprAdd exprAdd = (NodeBinExprAdd)expr.expr;
                GenerateExpr(exprAdd.Lhs);
                GenerateExpr(exprAdd.Rhs);

                output += "\n    ; Add\n";
                Pop("rbx", 8);
                Pop("rax", 8);
                output += "    add rax, rbx\n"; // rax = rax + rbx
                Push("rax", 8);
            }
            else if (exprType == typeof(NodeBinExprSub))
            {
                NodeBinExprSub exprAdd = (NodeBinExprSub)expr.expr;
                GenerateExpr(exprAdd.Lhs);
                GenerateExpr(exprAdd.Rhs);

                output += "\n    ; Substract\n";
                Pop("rbx", 8);
                Pop("rax", 8);
                output += "    sub rax, rbx\n"; // rax = rax - rbx
                Push("rax", 8);
            }
            else if (exprType == typeof(NodeBinExprMult))
            {
                NodeBinExprMult exprMult = (NodeBinExprMult)expr.expr;
                GenerateExpr(exprMult.Lhs);
                GenerateExpr(exprMult.Rhs);

                output += "\n    ; Multiply\n";
                Pop("rbx", 8);
                Pop("rax", 8);
                output += "    mul rbx\n"; // rax = rax * rbx
                Push("rax", 8);
            }
            else if (exprType == typeof(NodeBinExprDiv))
            {
                NodeBinExprDiv exprMult = (NodeBinExprDiv)expr.expr;
                GenerateExpr(exprMult.Lhs);
                GenerateExpr(exprMult.Rhs);

                output += "\n    ; Divide\n";
                Pop("rbx", 8);
                Pop("rax", 8);
                output += "    xor rdx, rdx\n";
                output += "    div rbx\n"; // rax = rax / rbx
                Push("rax", 8);
            }
            else if (exprType == typeof(NodeBinExprIsEqual))
            {
                NodeBinExprIsEqual exprIsEqual = (NodeBinExprIsEqual)expr.expr;
                GenerateExpr(exprIsEqual.Lhs);
                GenerateExpr(exprIsEqual.Rhs);

                output += "\n    ; Check if equal\n";
                output += "    xor rax, rax\n";
                output += "    xor rbx, rbx\n";
                output += "    xor rcx, rcx\n";

                int typeSize = variableSizes[exprIsEqual.Lhs.type];
                Pop(GetRegister('c', typeSize), typeSize);

                typeSize = variableSizes[exprIsEqual.Rhs.type];
                Pop(GetRegister('b', typeSize), typeSize);

                output += "    cmp rbx, rcx\n";
                output += "    setne al\n";
                output += "    dec al\n";
                Push("ax", 2);
            }
            else if (exprType == typeof(NodeBinExprIsNotEqual))
            {
                NodeBinExprIsNotEqual exprIsNotEqual = (NodeBinExprIsNotEqual)expr.expr;
                GenerateExpr(exprIsNotEqual.Lhs);
                GenerateExpr(exprIsNotEqual.Rhs);

                output += "\n    ; Check if not equal\n";
                output += "    xor rax, rax\n";
                output += "    xor rbx, rbx\n";
                output += "    xor rcx, rcx\n";

                int typeSize = variableSizes[exprIsNotEqual.Lhs.type];
                Pop(GetRegister('c', typeSize), typeSize);

                typeSize = variableSizes[exprIsNotEqual.Rhs.type];
                Pop(GetRegister('b', typeSize), typeSize);

                output += "    cmp rbx, rcx\n";
                output += "    sete al\n";
                output += "    dec al\n";
                Push("ax", 2);
            }
            else if (exprType == typeof(NodeBinExprLessThan))
            {
                NodeBinExprLessThan exprIsLessThan = (NodeBinExprLessThan)expr.expr;
                GenerateExpr(exprIsLessThan.Lhs);
                GenerateExpr(exprIsLessThan.Rhs);

                output += "\n    ; Check if less than\n";
                output += "    xor rax, rax\n";
                output += "    xor rbx, rbx\n";
                output += "    xor rcx, rcx\n";

                int typeSize = variableSizes[exprIsLessThan.Lhs.type];
                Pop(GetRegister('c', typeSize), typeSize);

                typeSize = variableSizes[exprIsLessThan.Rhs.type];
                Pop(GetRegister('b', typeSize), typeSize);

                output += "    cmp rbx, rcx\n";
                output += "    setnl al\n";
                output += "    dec al\n";
                Push("ax", 2);
            }
            else if (exprType == typeof(NodeBinExprGreaterThan))
            {
                NodeBinExprGreaterThan exprIsGreaterThan = (NodeBinExprGreaterThan)expr.expr;
                GenerateExpr(exprIsGreaterThan.Lhs);
                GenerateExpr(exprIsGreaterThan.Rhs);

                output += "\n    ; Check if greater than\n";
                output += "    xor rax, rax\n";
                output += "    xor rbx, rbx\n";
                output += "    xor rcx, rcx\n";

                int typeSize = variableSizes[exprIsGreaterThan.Lhs.type];
                Pop(GetRegister('c', typeSize), typeSize);

                typeSize = variableSizes[exprIsGreaterThan.Rhs.type];
                Pop(GetRegister('b', typeSize), typeSize);

                output += "    cmp rbx, rcx\n";
                output += "    setng al\n";
                output += "    dec al\n";
                Push("ax", 2);
            }
            else if (exprType == typeof(NodeBinExprLessThanOrEqualTo))
            {
                NodeBinExprLessThanOrEqualTo exprIsLessThanOrEqualTo = (NodeBinExprLessThanOrEqualTo)expr.expr;
                GenerateExpr(exprIsLessThanOrEqualTo.Lhs);
                GenerateExpr(exprIsLessThanOrEqualTo.Rhs);

                output += "\n    ; Check if less than or equal to\n";
                output += "    xor rax, rax\n";
                output += "    xor rbx, rbx\n";
                output += "    xor rcx, rcx\n";

                int typeSize = variableSizes[exprIsLessThanOrEqualTo.Lhs.type];
                Pop(GetRegister('c', typeSize), typeSize);

                typeSize = variableSizes[exprIsLessThanOrEqualTo.Rhs.type];
                Pop(GetRegister('b', typeSize), typeSize);

                output += "    cmp rbx, rcx\n";
                output += "    setnle al\n";
                output += "    dec al\n";
                Push("ax", 2);
            }
            else if (exprType == typeof(NodeBinExprGreaterThanOrEqualTo))
            {
                NodeBinExprGreaterThanOrEqualTo exprIsGreaterThanOrEqualTo = (NodeBinExprGreaterThanOrEqualTo)expr.expr;
                GenerateExpr(exprIsGreaterThanOrEqualTo.Lhs);
                GenerateExpr(exprIsGreaterThanOrEqualTo.Rhs);

                output += "\n    ; Check if greater than or equal to\n";
                output += "    xor rax, rax\n";
                output += "    xor rbx, rbx\n";
                output += "    xor rcx, rcx\n";

                int typeSize = variableSizes[exprIsGreaterThanOrEqualTo.Lhs.type];
                Pop(GetRegister('c', typeSize), typeSize);

                typeSize = variableSizes[exprIsGreaterThanOrEqualTo.Rhs.type];
                Pop(GetRegister('b', typeSize), typeSize);

                output += "    cmp rbx, rcx\n";
                output += "    setnge al\n";
                output += "    dec al\n";
                Push("ax", 2);
            }
            else if (exprType == typeof(NodeBinExprOr))
            {
                NodeBinExprOr exprOr = (NodeBinExprOr)expr.expr;
                GenerateExpr(exprOr.Lhs);
                GenerateExpr(exprOr.Rhs);

                output += "\n    ; Do logical or\n";
                Pop("bx", 2);
                Pop("ax", 2);
                output += "    or ax, bx\n";
                Push("ax", 2);
            }
            else if (exprType == typeof(NodeBinExprAnd))
            {
                NodeBinExprAnd exprOr = (NodeBinExprAnd)expr.expr;
                GenerateExpr(exprOr.Lhs);
                GenerateExpr(exprOr.Rhs);

                output += "\n    ; Do logical and\n";
                Pop("bx", 2);
                Pop("ax", 2);
                output += "    and ax, bx\n";
                Push("ax", 2);
            }
            else if (exprType == typeof(NodeBinExprXor))
            {
                NodeBinExprXor exprXor = (NodeBinExprXor)expr.expr;
                GenerateExpr(exprXor.Lhs);
                GenerateExpr(exprXor.Rhs);

                output += "\n    ; Do logical xor\n";
                Pop("bx", 2);
                Pop("ax", 2);
                output += "    xor ax, bx\n";
                Push("ax", 2);
            }
        }
        void GenerateExpr(NodeExpr expr)
        {
            Type exprType = expr.var.GetType();

            if (exprType == typeof(NodeTerm))
            {
                GenerateTerm((NodeTerm)expr.var);
            }
            else if (exprType == typeof(NodeExprBin))
            {
                GenerateBinExpr((NodeExprBin)expr.var);
            }
        }
        void GenerateScope(NodeScope scope)
        {
            // Begin scope
            scopes.Push(variables.Count);

            // Generate scope contents
            foreach (NodeStmt nodeStmt in scope.stmts)
            {
                GenerateStmt(nodeStmt);
            }

            // End scope
            int popCount = variables.Count - scopes.Pop();

            output += "\n    ; End scope (Pop scope variables from stack)\n";
            output += $"    add rsp, {popCount * 8}\n";

            for (int i = 0; i < popCount; i++)
            {
                Variable variable = variables.Pop();
                stackSize -= variableSizes[variable.type];
            }
        }
        void GenerateStmt(NodeStmt stmt)
        {
            Type stmtType = stmt.var.GetType();

            if (stmtType == typeof(NodeStmtExit))
            {
                NodeStmtExit stmtExit = (NodeStmtExit)stmt.var;

                GenerateExpr(stmtExit.expr);

                output += "\n    ; Exit statement\n";
                Pop("rcx", 8);
                output += "    call ExitProcess\n"; // Get the expr value
            }
            else if (stmtType == typeof(NodeStmtDeclaration))
            {
                NodeStmtDeclaration stmtDeclaration = (NodeStmtDeclaration)stmt.var;
                if (DoesVariableExist(stmtDeclaration.ident.value!))
                {
                    Compiler.Error($"Identifier already used: {stmtDeclaration.ident.value!}");
                    return;
                }
                variables.Push(new(stmtDeclaration.ident.value!, stmtDeclaration.expr.type, stackSize));
                output += $"\n    ; Create variable {stmtDeclaration.ident.value!}\n";
                GenerateExpr(stmtDeclaration.expr);
            }
            else if (stmtType == typeof(NodeStmtAssignment))
            {
                NodeStmtAssignment stmtAssignment = (NodeStmtAssignment)stmt.var;
                if (!DoesVariableExist(stmtAssignment.ident.value!))
                {
                    Compiler.Error($"Undeclared variable: {stmtAssignment.ident.value!}");
                    return;
                }
                GenerateExpr(stmtAssignment.expr);
                Variable variable = GetVariable(stmtAssignment.ident.value!);

                int variableSize = variableSizes[variable.type];
                int stackPos = stackSize - variable.stackPosition - variableSize * 2;

                string register = GetRegister('a', variableSize);
                output += $"\n    ; Assign value to variable {variable.name}\n";
                Pop(register, variableSize);
                output += $"    mov {GetWord(variableSize)} [rsp + {stackPos}], {register}\n";
            }
            else if (stmtType == typeof(NodeStmtIf))
            {
                NodeStmtIf stmtIf = (NodeStmtIf)stmt.var;
                GenerateExpr(stmtIf.expr);

                int ifStmtIndex = ifStmtCount;
                ifStmtCount++;

                output += "\n    ; If statement\n";
                Pop("ax", 2);
                output += $"    sub al, 0\n";
                output += $"    jz if_end{ifStmtIndex}\n";

                GenerateScope(stmtIf.scope);

                output += $"if_end{ifStmtIndex}:\n";
            }
            else if (stmtType == typeof(NodeStmtWhile))
            {
                NodeStmtWhile stmtIf = (NodeStmtWhile)stmt.var;

                int whileLoopIndex = whileLoopCount;
                ifStmtCount++;

                output += "\n    ; While loop\n";
                output += $"while_start{whileLoopIndex}:\n";
                GenerateExpr(stmtIf.expr);

                output += "\n";
                Pop("ax", 2);
                output += $"    sub al, 0\n";
                output += $"    jz while_end{whileLoopIndex}\n";

                GenerateScope(stmtIf.scope);
                output += $"    jmp while_start{whileLoopIndex}\n";
                output += $"while_end{whileLoopIndex}:\n";
            }
            else if (stmtType == typeof(NodeScope))
            {
                NodeScope scope = (NodeScope)stmt.var;
                GenerateScope(scope);
            }
        }
        public string Generate()
        {
            output = "global WinMain\nextern ExitProcess\nsection .text\nWinMain:\n";

            foreach (NodeStmt stmt in prog.stmts)
            {
                GenerateStmt(stmt);
            }

            // Always return with default exit code if not done explicitly
            output += "\n    ; Default exit statement\n";
            output += "    xor rcx, rcx\n";
            output += "    call ExitProcess\n";
            return output;
        }
        Variable GetVariable(string variableName)
        {
            foreach (Variable variable in variables)
            {
                if (variable.name == variableName)
                {
                    return variable;
                }
            }
            throw new Exception("Variable does not exist");
        }
        bool DoesVariableExist(string variableName)
        {
            foreach (Variable variable in variables)
            {
                if (variable.name == variableName)
                {
                    return true;
                }
            }
            return false;
        }
        static string GetRegister(char register, int sizeInBytes)
        {
            return sizeInBytes switch
            {
                2 => $"{register}x",
                8 => $"r{register}x",
                _ => throw new Exception("Invalid register size")
            };
        }
        static string GetWord(int sizeInBytes)
        {
            return sizeInBytes switch
            {
                2 => $"WORD",
                4 => $"DWORD",
                8 => $"QWORD",
                _ => throw new Exception("Invalid word size")
            };
        }
        void Push(string register, int size)
        {
            output += $"    push {register}\n";
            stackSize += size;
        }
        void Pop(string register, int size)
        {
            output += $"    pop {register}\n";
            stackSize -= size;
        }
    }
}