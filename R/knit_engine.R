##' @importFrom evaluate evaluate
##' @export
cachesuite_engine = function(options) {
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
            engine_output(options = val$chunkopts, out = res)
        }
        code = options$code
        evalWithCache(code, eval_fun = evalfn,
                      return_handler = rethndl,
                      env = knit_global())
    }
}
                                                                                  
    
