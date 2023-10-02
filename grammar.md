$$
\begin{align}
    \text{[Prog]} &\to \text{[Stmt]*} \\

    \text{[Stmt]} &\to
    \begin {cases}
        \text{exit ([Scope]);} \\
        \text{[Type] ident = [Expr];} \\
        \text{if ([Expr]) [Scope]} \\
        \text{while ([Expr]) [Scope]} \\
        \text{for ([Declaration]; [Expr]; [Assignment]) [Scope]} \\
        \text{function [Type] ident([params])}\space[\text{Scope}] \\
        \text{ident([params]);} \\
        \text{[Scope]} \\
    \end{cases} \\

    [\text{Scope}] &\to \{[\text{Stmt}^*]\} \\

    [\text{Expr}] &\to
    \begin {cases}
        [\text{Term}] \\
        [\text{BinExpr}] \\
    \end{cases} \\
    
    [\text{BinExpr}] &\to
    \begin {cases}
        \text{[Expr] * [Expr]} & \text{prec} = 3 \\
        \text{[Expr] / [Expr]} & \text{prec} = 3 \\
        \text{[Expr] + [Expr]} & \text{prec} = 2 \\
        \text{[Expr] - [Expr]} & \text{prec} = 2 \\
        \text{[Expr] == [Expr]} & \text{prec} = 1 \\
        \text{[Expr] != [Expr]} & \text{prec} = 1 \\
        \text{[Expr] < [Expr]} & \text{prec} = 1 \\
        \text{[Expr] > [Expr]} & \text{prec} = 1 \\
        \text{[Expr] <= [Expr]} & \text{prec} = 1 \\
        \text{[Expr] >= [Expr]} & \text{prec} = 1 \\
        \text{[Expr] || [Expr]} & \text{prec} = 0 \\
        \text{[Expr] \&\& [Expr]} & \text{prec} = 0 \\
        \text{[Expr] \^{} [Expr]} & \text{prec} = 0 \\
    \end{cases} \\

    [\text{Term}] &\to
    \begin {cases}
        \text{[Type]-literal} \\
        \text{ident} \\
        \text{([Expr])} \\
    \end{cases} \\
    \text{[Type]} &\to
    \begin {cases}
        \text{int} \\
        \text{bool} \\
    \end{cases} \\
\end{align}
$$