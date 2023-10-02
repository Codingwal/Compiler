using System.Diagnostics;

namespace Compiler
{
    public class Variable
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
        List<Variable> variables;
        Stack<int> scopes;
        int ifStmtCount = 0;
        int whileLoopCount = 0;
        int forLoopCount = 0;
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
                int stackPos = variable.stackPosition - variableSize;

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
        void GenerateScope(NodeScope scope, int additionalVarsToDelete = 0)
        {
            // Begin scope
            scopes.Push(variables.Count - additionalVarsToDelete);

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
                Variable variable = PopVariable();
                ChangeStackPointer(-variableSizes[variable.type]);
            }
        }

        void GenerateStmtExit(NodeStmtExit stmt)
        {
            GenerateExpr(stmt.expr);

            output += "\n    ; Exit statement\n";
            Pop("rcx", 8);
            output += "    call ExitProcess\n"; // Get the expr value
        }
        void GenerateStmtDeclaration(NodeStmtDeclaration stmt)
        {
            CreateVariable(stmt.ident.value!, stmt.expr.type);
            output += $"\n    ; Create variable {stmt.ident.value!}\n";
            GenerateExpr(stmt.expr);
        }
        void GenerateStmtAssignment(NodeStmtAssignment stmt)
        {
            if (!DoesVariableExist(stmt.ident.value!))
            {
                Compiler.Error($"Undeclared variable: {stmt.ident.value!}");
                return;
            }
            GenerateExpr(stmt.expr);
            Variable variable = GetVariable(stmt.ident.value!);

            int variableSize = variableSizes[variable.type];
            int stackPos = variable.stackPosition - variableSize * 2;

            string register = GetRegister('a', variableSize);
            output += $"\n    ; Assign value to variable {variable.name}\n";
            Pop(register, variableSize);
            output += $"    mov {GetWord(variableSize)} [rsp + {stackPos}], {register}\n";
        }
        void GenerateStmtIf(NodeStmtIf stmt)
        {
            GenerateExpr(stmt.expr);

            int ifStmtIndex = ifStmtCount;
            ifStmtCount++;

            output += "\n    ; If statement\n";
            Pop("ax", 2);
            output += $"    sub al, 0\n";
            output += $"    jz if_end{ifStmtIndex}\n";

            GenerateScope(stmt.scope);

            output += $"if_end{ifStmtIndex}:\n";
        }
        void GenerateStmtWhile(NodeStmtWhile stmt)
        {
            int whileLoopIndex = whileLoopCount;
            ifStmtCount++;

            output += "\n    ; While loop\n";
            output += $"while_start{whileLoopIndex}:\n";
            GenerateExpr(stmt.expr);

            output += "\n";
            Pop("ax", 2);
            output += $"    sub al, 0\n";
            output += $"    jz while_end{whileLoopIndex}\n";

            GenerateScope(stmt.scope);
            output += $"    jmp while_start{whileLoopIndex}\n";
            output += $"while_end{whileLoopIndex}:\n";
        }
        void GenerateStmtFor(NodeStmtFor stmt)
        {
            int forLoopIndex = forLoopCount;
            forLoopCount++;

            output += "\n    ; For loop\n";
            GenerateStmtDeclaration(stmt.decl);
            output += $"\n    jmp for_scope_start{forLoopIndex}\n";
            output += $"for_start{forLoopIndex}:\n";
            GenerateStmtAssignment(stmt.assign);
            GenerateExpr(stmt.expr);
            output += "\n";
            Pop("ax", 2);
            output += $"    sub al, 0\n";
            output += $"    jz for_end{forLoopIndex}\n";
            output += $"for_scope_start{forLoopIndex}:\n";

            GenerateScope(stmt.scope);
            output += $"    jmp for_start{forLoopIndex}\n";
            output += $"for_end{forLoopIndex}:\n";

            // Delete variable i
            output += "    add rsp, 8\n";
            Variable variable = PopVariable();
            ChangeStackPointer(-variableSizes[variable.type]);
        }
        void GenerateFunctionDeclaration(NodeStmtFunctionDeclaration stmt)
        {
            output += "\n    ; Function declaration\n";
            output += $"    jmp main_end_{stmt.name}\n";
            output += $"main_{stmt.name}:\n";

            // Loop through parameters in reverse order (Last parameter is on top)
            ChangeStackPointer(8);
            int stackPos = 0;
            for (int i = stmt.parameters.Count - 1; i >= 0; i--)
            {
                VariableToken parameter = stmt.parameters[i];
                int parameterSize = variableSizes[parameter.type];
                CreateVariable(parameter.name, parameter.type, stackPos + parameterSize);

                stackPos += parameterSize; // + because stack grows downwards
                ChangeStackPointer(parameterSize);
            }

            GenerateScope(stmt.scope);
            output += "    ret\n";
            ChangeStackPointer(-8);
            ChangeStackPointer(-stackPos);
            output += $"main_end_{stmt.name}:\n";
        }
        void GenerateFunctionCall(NodeStmtFunctionCall stmt)
        {
            output += "\n    ; Function call\n";
            int parameterSize = 0;
            foreach (NodeExpr parameter in stmt.parameters)
            {
                GenerateExpr(parameter);
                parameterSize += variableSizes[parameter.type];
            }
            // output += "    add rsp, 8 ; Return value placeholder";
            output += $"    call main_{stmt.name}\n";
            output += $"    add rsp, {parameterSize}\n";
            ChangeStackPointer(-parameterSize);
        }
        void GenerateReturn(NodeStmtReturn stmt)
        {
            output += "\n    ; Return\n";

            // Store expression in a register
            GenerateExpr(stmt.expr);
            int returnValueSize = variableSizes[stmt.expr.type];
            string register = GetRegister('a', returnValueSize);
            Pop(register, returnValueSize);

            // End scope
            // int popCount = variables.Count - scopes.Pop();
            // output += "\n    ; Pop scope variables from stack\n";
            // output += $"    add rsp, {popCount * 8}\n";
            // for (int i = 0; i < popCount; i++)
            // {
            //     Variable variable = PopVariable();
            //     ChangeStackPointer(-variableSizes[variable.type]);
            // }

            // Store return value on stack and return
            output += $"    mov {GetWord(returnValueSize)} [rsp + 8], {register}\n";
            output += "    ret\n";
        }

        void GenerateStmt(NodeStmt stmt)
        {
            Type stmtType = stmt.var.GetType();

            if (stmtType == typeof(NodeStmtExit))
            {
                GenerateStmtExit((NodeStmtExit)stmt.var);
            }
            else if (stmtType == typeof(NodeStmtDeclaration))
            {
                GenerateStmtDeclaration((NodeStmtDeclaration)stmt.var);
            }
            else if (stmtType == typeof(NodeStmtAssignment))
            {
                GenerateStmtAssignment((NodeStmtAssignment)stmt.var);
            }
            else if (stmtType == typeof(NodeStmtIf))
            {
                GenerateStmtIf((NodeStmtIf)stmt.var);
            }
            else if (stmtType == typeof(NodeStmtWhile))
            {
                GenerateStmtWhile((NodeStmtWhile)stmt.var);
            }
            else if (stmtType == typeof(NodeStmtFor))
            {
                GenerateStmtFor((NodeStmtFor)stmt.var);
            }
            else if (stmtType == typeof(NodeScope))
            {
                GenerateScope((NodeScope)stmt.var);
            }
            else if (stmtType == typeof(NodeStmtFunctionDeclaration))
            {
                GenerateFunctionDeclaration((NodeStmtFunctionDeclaration)stmt.var);
            }
            else if (stmtType == typeof(NodeStmtFunctionCall))
            {
                GenerateFunctionCall((NodeStmtFunctionCall)stmt.var);
            }
            else if (stmtType == typeof(NodeStmtReturn))
            {
                GenerateReturn((NodeStmtReturn)stmt.var);
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
        void CreateVariable(string name, VariableType type, int stackPos = 0)
        {
            if (DoesVariableExist(name))
            {
                Compiler.Error($"Identifier already used: {name}");
                return;
            }
            variables.Add(new(name, type, stackPos));
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
            ChangeStackPointer(size);
        }
        void Pop(string register, int size)
        {
            output += $"    pop {register}\n";
            ChangeStackPointer(-size);
        }
        Variable PopVariable()
        {
            Variable variable = variables[^1];
            variables.Remove(variable);
            return variable;
        }
        void ChangeStackPointer(int size)
        {
            for (int i = 0; i < variables.Count; i++)
            {
                Variable variable = variables.ElementAt(i);
                variable.stackPosition += size;
            }
        }
    }
}