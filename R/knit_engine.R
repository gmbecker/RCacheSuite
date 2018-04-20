##' @importFrom evaluate evaluate
##' @title Knitr language engine for state-aware caching evaluation of R code
##' @description This function defines an alternative knitr language engine
##' for evaluating R code using RCacheSuite. It can be specified
##' as the engine for a particular 'language' /chunk type via knitr, but
##' should never be called directly by end users.
##' @param options Options object passed by knitr to language engine
##' @export
cachesuite_engine = function(options) {
    if(!requireNamespace("knitr"))
        stop("knitr missing. should never get here")
    if(is.null(options$eval) || isTRUE(options$eval)) {
        ## XXX really not sure why I need to do this...
        if(is.null(options$fig.ext) && is.character(options$dev))
            options$fig.ext = options$dev
    
        evalfn = function(code, env,...) {
            list(chunkopts = options,
                 res = evaluate(input = code, envir = env))
        }
        rethndl = function(val, graphics, env, evaled, last, ...) {
            res = lapply(val$res, function(x) {
                if(inherits(x, "recordedplot"))
                    fixupRecordedPlot(x)
                else
                    x
                })
            knitr::engine_output(options = val$chunkopts, out = res)
        }
        code = options$code
        evalWithCache(code, eval_fun = evalfn,
                      return_handler = rethndl,
                      env = knitr::knit_global())
    }
}
                                                                                  
    
