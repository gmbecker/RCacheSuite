unparse = function(expr)
    {
        if(!is(expr, "expression"))
            stop(sprintf("Cannot unparse non-expression of class %s", class(expr)))
        sapply(expr, function(x)
               {
                   paste(deparse(x), collapse="\n")
               })
    }
        
validCodeCache = function(dir)
{
    valid = TRUE
    reason = character()
    if(file.exists(dir))
    {
        filnames = list.files(path=dir)
        if(!length(grep("code_", dir)))
        {
            valid = FALSE
            reason = c(reason,"Directory does not follow the code_<hash> naming convention.")
        }
        if(!("code.R" %in% filnames))
        {
            valid = FALSE
            reason = c(reason, "No code.R file found.")
        }
        if(!length(grep("^cache_.*rda$", filnames)))
        {
            valid = FALSE
            reason = c(reason, "No cached result files (cache_<hash>.rda) found.")
        }
        #we don't want to warn every time there's not an existing cache for a code block, so we put the warning in here instead of the end of the function
        if(!valid)
        {
            reason = paste(reason, collapse = " ")
            warning(sprintf("Directory %s does not appear to contain a valid code-level cache set. Problem(s): %s", dir, reason))
        }
    } else {
        valid = FALSE
    }
    valid
}
