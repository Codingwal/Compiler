$$
\begin{align}
    [\text{Prog}] &\to [\text{Stmt}]^* \\

    [\text{Stmt}] &\to
    \begin {cases}
        \text{exit}([\text{Expr}]) \\
        [\text{Type}]\space\text{ident = }([\text{Expr}]) \\
        \text{ident = }([\text{Expr}]) \\
        \text{if }([\text{Expr}])\space[\text{Scope}] \\
        [\text{Scope}] \\
    \end{cases} \\

    [\text{Scope}] &\to \{[\text{Stmt}^*]\} \\

    [\text{Expr}] &\to
    \begin {cases}
        [\text{Term}] \\
        [\text{BinExpr}] \\
    \end{cases} \\
    
    [\text{BinExpr}] &\to
    \begin {cases}
        [\text{Expr}] * [\text{Expr}] & \text{prec} = 3 \\
        [\text{Expr}] / [\text{Expr}] & \text{prec} = 3 \\
        [\text{Expr}] + [\text{Expr}] & \text{prec} = 2 \\
        [\text{Expr}] - [\text{Expr}] & \text{prec} = 2 \\
        [\text{Expr}] == [\text{Expr}] & \text{prec} = 1 \\
        [\text{Expr}] != [\text{Expr}] & \text{prec} = 1 \\
        [\text{Expr}] < [\text{Expr}] & \text{prec} = 1 \\
        [\text{Expr}] > [\text{Expr}] & \text{prec} = 1 \\
        [\text{Expr}] <= [\text{Expr}] & \text{prec} = 1 \\
        [\text{Expr}] >= [\text{Expr}] & \text{prec} = 1 \\
        [\text{Expr}]\space ||\space [\text{Expr}] & \text{prec} = 0 \\
        [\text{Expr}] \&\&[\text{Expr}] & \text{prec} = 0 \\
        [\text{Expr}]\space \^{} \space [\text{Expr}] & \text{prec} = 0 \\
    \end{cases} \\

    [\text{Term}] &\to
    \begin {cases}
        \text{[Type]-literal} \\
        \text{ident} \\
        ([\text{Expr}]) \\
    \end{cases} \\
    [\text{Type}] &\to
    \begin {cases}
        \text{int} \\
        \text{bool} \\
    \end{cases} \\
\end{align}
$$