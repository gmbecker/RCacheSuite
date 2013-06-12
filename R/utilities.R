unparse = function(expr)
    {
        if(!is(expr, "expression"))
            stop(sprintf("Cannot unparse non-expression of class %s", class(expr)))
        sapply(expr, deparse)
    }
        