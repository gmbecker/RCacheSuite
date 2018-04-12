#Other caching mechanisms in R:
# Biobase::cache Checks in cache for LHS of LHS<-RHS expression and only evaluates RHS if not found in cache
#R.cache package
#cacheSweave
#knitr with cache=TRUE
#SOAR package, not really the same thing, basically create lazyload dbs for things on the fly and take them out of memory?
#cacher
#memoize - Caches output of function call, immediately returns output if function is called again with same inputs. Not clear if it can track more than one set of inputs. I think it may actually modify the function itself akin to trace/debug

#this is a list of classes we know correspond to plots, and whose show functions are assumed to draw the plot.
#grobs (grid low level objects for image components) are drawn by default when they are created, but their show/print commands do NOT draw them, so they are excluded here
gclasses = c("trellis", "ggplot", "gg", "ggbio", "recordedplot")



#' @title Evaluation functions and return value handlers
#'
#' @description RCacheSuite allows users to specify custom evaluation functions,
#' which can be used to capture information about side effects, and return value
#' handlers, which can then use that information to recreate the side effects.
#'
#' In practice, the behavior of the default evaluation function
#' (\code{parseWithVis}) and return handler (\code{withVizHandler}) should meet
#' users' needs the majority of the time.
#'
#' @param code The code to evaluate
#' @param env The evaluation environment/environment the return handler should
#' populate
#' @param ... Generally unused.
#' @param val ANY. The object returned from the evaluation function
#' @param graphics PlotList. The graphics captured during evaluation
#' @param evaled Logical. Indicates whether evaluation occured (it is passed
#' \code{FALSE} when values are loaded from cache).
#' @param last Logical. Is the value being processed the output of the final
#' expression in the code being handled.
#'
#' @details
#' Evaluation functions must accept \code{code}, \code{env}, and \code{...}.
#'
#' Return value handlers must accept \code{val}, \code{graphics},
#' \code{env}, and \code{...}. They will also be passed \code{evaled}
#' and \code{last} when called by the caching machinery, and can
#' handle those specifically or catch them in \code{...}.
#'
#'\code{parseEval} simply parses and evaluates the code, returning the
#'  value returned by the call to \code{eval}.
#'
#'  \code{parseWithVis} evaluates the code using \code{withVis} and
#'  returns a \code{WithVisValue} object with the resulting return
#'  value and visibility information.
#'
#'  \code{withVisHandler} accepts the \code{WithVisValue} object
#'  returned by \code{parseWithVis} as well as printing the final
#'  result if the \code{val$visible} is \code{TRUE}. It also draws any
#'  plots stored in the cache in the case that \code{evaled} is
#'  \code{FALSE} (if it were \code{TRUE} those plots would have
#'  already been generated during the evaluation). Appropriate for use
#'  with \code{parseWithVis}.
#'
#'  \code{returnRaw} recreates any plots recorded in the cache (always,
#'     not differentiating between evaluation and cache loading) and
#'     returns \code{val} unmodified. Appropriate for use with \code{parseEval}
#'
#'  \code{noGraphicsRaw} returns \code{val} unmodified without
#'  recreating any plots.
#' 
#' @return Evaluation functions should return the result of evaluating
#'     the code, along with information sufficient to recreate any
#'     non-plotting side-effects the user wishes to record and
#'     recreate on loading from cache. Return value handlers should
#'     recreate those side effects, and can then call \code{returnRaw}
#'     or \code{withVisRaw} to recreate plots and return the value
#'     from a raw return \code{val} or \code{WithVisValue} val object,
#'     respectively. \code{WithVisValue} objects are created
#'     automatically by the default evaluate function.
#' @author Gabriel Becker
#' @export
#' @rdname evalhandlers
parseEval = function(code, env, ...) eval(parse(text=code), envir=env)

#' @export
#' @rdname evalhandlers
parseWithVis = function(code, env, ...) {
#    ret = withVisible(eval(parse(text=code), envir= env))
    ret = withVisible(eval(code, envir = env))
    new("WithVisValue", value = ret$value, visible = ret$visible)
}

#@val is an arbitrary object in the exact form returned by the evaluation function
#@graphics is a PlotList  object (essentially a list) with 0 or more recordedplot objects in it
#' @export
#' @rdname evalhandlers

withVisHandler = function(val, graphics, env, evaled = FALSE, last = FALSE, ...)
{
    raw = withVisRaw(val, graphics, env, evaled, last,  ...)
    invisible(raw@value)
}

#' @export
#' @rdname evalhandlers

withVisRaw = function(val, graphics, env, evaled = FALSE,  last = FALSE, ...)
{
    if(!is(val, "WithVisValue"))
        stop("the withVisHandler return handler function expects an object of class 'WithVisValue', got an object of class: ", class(val))
    if(length(graphics) && !evaled)
        lapply(graphics, redrawPlot)
    if(val@visible && last)
        show(val@value)
    if(!is(graphics, "PlotList")) {
        if(!is.null(graphics))
            graphics = list(graphics)
        else
            graphics = list()
        graphics = as(graphics, "PlotList")
    }

    val = new("WithVisPlusGraphics", value = val@value, visible = val@visible && last, graphics = graphics)
    invisible(val)
}

#' @export
#' @rdname evalhandlers

returnRaw = function(val, graphics, env, evaled = FALSE, ...)
{
    if(length(graphics) && !evaled)
        lapply(graphics, redrawPlot)
    val
}
#' @export
#' @rdname evalhandlers

noGraphicsRaw = function(val, graphics, env, evaled = FALSE, ...)
{
    val
}

missingornull = function(x) missing(x) || is.null(x)

#' @title evalWithCache
#' 
#' @description This function accepts a piece of R code and performs
#'     state (input variable value) dependent cached evaluation of it.
#' 
#' @param code The code to evaluate
#' @param codeInfo A ScriptNodeInfo object, defaults to the result of
#'     calling \code{getInputs} on the \code{code} with default
#'     handling options
#' @param inputVars A vector of variable names which are inputs to the
#'     code block. Overrides automatically detected inputs
#' @param outputVars A vector of variable name which are outputs
#'     generated by the code block. Overrides automatically detected
#'     outputs
#' @param cache The CachingEngine to use when evaluating the
#'     code. Defaults to an engine which writes caches out
#'     immediately, mimicing caching behavior of other systems. See
#'     note.
#' @param eval_fun A function to perform evaluation of the
#'     code. Defaults to the code evaluator associated with
#'     \code{cache}
#' @param return_handler A function to be called on the value returned
#'     by evaluating the code (or loading from cache). Defaults to
#'     return handler associated with \code{cache}
#' @param env The environment in which to evaluate the code or load
#'     the cached outputs
#' @param force A logical indicating whether to force a cache
#'     refresh. If \code{TRUE}, cached values will NOT be used, but a
#'     new cache will be written after evaluating
#'     \code{code}. Defaults to \code{FALSE}
#' @param cacheRand A logical indicating whether to create/use caches
#'     for expressions with a detected random component. Defaults to
#'     FALSE
#' @param verbose Should extra informative messages be
#'     emitted. Defaults to \code{FALSE}
#' @param gexts A vector of graphics file extensions for caching
#'     graphics
#' @param gdev A vector of graphics devices to use to generate the
#'     files indicated by \code{gexts} for caching graphics. Defaults
#'     to functions named directly for their extensions (e.g.,
#'     \code{png}).
#' @param last logical. Is this the last expression in the codeblock
#'     being evaluated. Passed to return handler. Generally this
#'     should not be manually set.
#' @param stopMissingInput Should an error be thrown when an input
#'     variable is not present in the evaluation scope. If
#'     \code{FALSE} a warning will be emitted. Defaults to
#'     \code{FALSE}
#' @param singleEntFun A function, or NULL. If a function, it is
#'     called on \code{codeInfo} to determine if \code{code} should be
#'     treated as a single entity for caching purposes. If
#'     \code{NULL}, or a function that returns \code{FALSE},
#'     expressions in \code{code} are evaluated and cached
#'     separately. Defautls to \code{sameOutVar}
#' @param unCacheable A function which accepts a ScriptNodeInfo object
#'     and returns a logical value indicating whether the computation
#'     is cacheable. Defaults to \code{mustForce}
#' @param ... Passed to \code{eval_fun} if \code{code} is evaluated.
#' @details This function performs state-aware cached evaluation of R
#'     code. This means that a cache is considered to apply if and
#'     only if both the code (after parsing) and the values of any
#'     variables used as inputs to the code are identical to those
#'     present when the cache was created.
#'
#' Cached evaluation is done via a caching engine (CachingEngine
#' object), which has a set of caches it already knows about, the
#' ability to create new caches, and a set of behaviors for when - if
#' ever - a cache should be written to disk, an evaluator function
#' which evaluates the code, and a return handler which processes the
#' object returned from the evaluator or loaded from a matching cache.
#'
#' The evaluator function is charged with evaluating code and
#' capturing any side-effects of the code that the user wishes to
#' cache and recreate, encoding them in the object it returns. The
#' return handler is tasked with processing that return value,
#' recreating any side effects (including plots, messages printed to
#' the console, etc) and then returning the raw result of the
#' evaluation.
#' @return The value generated by \code{cache$return_handler} when
#'     called on the value returned by evaluating \code{code} using
#'     \code{cache$eval_fun}.
#'
#'  In other words, the value
#'  \preformatted{
#'    cache$return_handler(cache$eval_fun(code, env, ...))
#'  }
#'  is returned, though if a valid cache is found,
#'  \code{cache$eval_fun} is not called and the cached return value (of the
#'  evaluator function at the time of caching) is
#'  passed to \code{cache$return_handler}.
#'  
#'  The return value of \code{cache$return_handler} is returned
#'  invisibly. The \code{cache$return_handler} function itself is
#'  responsible for the duplication of any side effects, including any
#'  printing of errors, warnings, or messages as well as any necessary
#'  regeneration of graphical output.
#' @author Gabriel Becker
#' @seealso \code{\link{cachingEngine}}, \code{\link[CodeDepends]{getInputs}}
#' @examples
#' \dontrun{
#'    res = evalWithCache("x = 5; y = x+7;y")
#' }
#' @export
evalWithCache = function(code,
    codeInfo = NULL,
    inputVars = NULL, #overrides automatically detected inputs
    outputVars = NULL, #overrides automatically detected outputs
    ## The default here is write_on_cache=TRUE because we don't return
    ## the cache engine so if it isn't passed in its not retained
    ## anywhere.
    ## This replicates existing caching behaviors where the
    ## only caching action s are one-time read from/write to disk.
    ## Passing in an existing CachingEngine objects modifies that
    ## object in place and gives access to the full spectrum of
    ## caching behaviors offered by the package.
    cache = cachingEngine(write_on_cache = TRUE),
    eval_fun = cache$eval_fun,
    return_handler = cache$return_handler,
    env = .GlobalEnv,
    force = FALSE,
    cacheRand = FALSE,
    verbose = FALSE,
    gexts = "png",
    gdev = sapply(gexts, function(nm) get(nm, mode="function")),
    last = TRUE,
    stopMissingInput = FALSE,
    singleEntFun= sameOutVar,
    unCacheable = mustForce,
    ...)
{
    if(is.character(code))
    {
        code = readScript("", "R", txt = code)
    }
    
    if(is(code, "Script") && length(code) > 1)
    {
        if( (is.function(singleEntFun) && singleEntFun(code)))
        {
            oneExpr = paste("{", paste(as(code, "character"), collapse = "\n"), "}", collapse = "\n")
            code = readScript("", "R", oneExpr)
        } else {
            #cache is a refclass, this modifies it in place!!
            lapply(code[-length(code)], evalWithCache, cache = cache,
                   eval_fun = eval_fun,
                   return_handler = return_handler, env = env,
                   force = force, cacheRand = cacheRand,
                   verbose = verbose, gexts = gexts, gdev = gdev,
                   last = FALSE, ...)
            return(evalWithCache(code[[length(code)]], cache = cache,
                                 eval_fun = eval_fun,
                                 return_handler = return_handler,
                                 env = env, force = force,
                                 cacheRand = cacheRand,
                                 verbose = verbose, gexts = gexts,
                                 gdev = gdev, last = TRUE, ...)
                   )
        }
    }

    if(is.null(codeInfo))
    {
        codeInfo = getInputs(code)
        if(!is(codeInfo, "ScriptNodeInfo"))
            codeInfo = codeInfo[[1]]
    }


    ## we can't cache certain types of expressions, calls to library,
    ## load, par, options, etc. The unCacheable argument accepts a
    ## function which accepts codeInfo for an expression and returns
    ## TRUE if the expression is uncacheable.
    if(!force && unCacheable(codeInfo))
    {
        force = TRUE
    }
    
    if(is.null(inputVars)||is.null(outputVars))
    {
        if(is.null(inputVars))
            inputVars = c(codeInfo@inputs, codeInfo@updates)
        if(is.null(outputVars))
            outputVars = c(codeInfo@outputs, codeInfo@updates)
    }

    if(is(code, "Script"))
        code = code[[1]]
    ## parse and deparse the code to get rid of the annoying "adding a
    ## space invalidates the cache" issue other systems have

    pcode = get_parsed_code(code)
    if(verbose)
        cat(paste(pcode, ":  "))
    ## do the required inputs even exist in the current session?
    in_exist = sapply(inputVars, exists, envir = env)
    if(!all(in_exist))
    {
        if(stopMissingInput)
            stop(paste("Missing variable(s) required to evaluate codeblock:",
                       inputVars[!in_exist], collapse = " "))
        else
            warning(paste("Missing inputs? Variables detected as inputs but not found:",
                          inputVars[!in_exist], collapse = " "))
    }
            
    
    ## make the list we will fastdigest to get the hash. It includes
    ## the (properly handled) code as well as the current values of
    ## all the input variables
    chash = fastdigest(pcode)
    ihash = fastdigest(mget(inputVars, envir = env, ifnotfound=list(NULL)))
    
    fnd = cache$find_data(chash, ihash)
    if(is(fnd, "CachedData") && !force)
    {
        if(verbose)
            cat("using matching cache.\n")
        fnd$retrieve_data(env)
        xxx_returnvalue = get("xxx_returnvalue", env)
        ## The handler handles reproducing side effects such as
        ## printing, warning/error message, and graphics.
        if(!is.null(env$xxx_handler))
            returnvalue = env$xxx_handler(xxx_returnvalue,
                                          env$xxx_graphics,
                                          env = env,
                                          evaled = FALSE,
                                          last = last)
        else
            returnvalue = xxx_returnvalue
    } else {
        if(verbose)
            cat("evaluating.\n")

        if(exists(".Random.seed"))
            oldRS = .Random.seed
        else
            oldRS = NULL
        
        oldplot = if(dev.cur() > 1) recordPlot() else NULL
        olddev = dev.cur()
        
        xxx_returnvalue = eval_fun(code = code, env = env, evaled= TRUE, last = last, ...)
        
        assign("xxx_returnvalue", xxx_returnvalue, envir = env)
        assign("xxx_handler", return_handler, envir = env)
        xxx_graphics = as(list(), "PlotList")
        #check if there an active graphics device and if so if its contents are different than
        #the contents
        # Hadley's evaluate package does a more sophisticated version of this check, but do we want to depend on it just for that?
        newdev = dev.cur()
        if(newdev > 1 && (is.null(oldplot) || newdev != olddev || ! identical(oldplot, recordPlot() ) ) )
            xxx_graphics[[length(xxx_graphics) + 1]] =  recordPlot()
        
        assign("xxx_graphics", xxx_graphics, envir =  env)
        
        #XXX previously outlist included pcode. why did I think i needed the pcode value? Am I using it somehwere?
        outlist = c("xxx_returnvalue", "xxx_handler", "xxx_graphics",  outputVars)
        
        if(!is.list(gdev))
            gdev = list(gext = gdev)
        
                                        #XXX This won't catch everything, but it will catch a lot.
                #See note/details in ?set.seed
        rand = FALSE
        if(exists(".Random.seed") && ! identical(oldRS, .Random.seed))
            rand = TRUE
        #What should we do when we know the result is random?
        #Cache it with information on the object indicating its random, or not cache at all?
        #we can control this with an argument but still need to decide the default...
        if(!rand || cacheRand )
        {
            cset = cache$get_or_create_set(pcode, inputVars, outputVars)
            #XXX right now it always assigns the cache to the first location in cache_dirs, even if there are more than one
            newcd = cachedData$new(code_hash = chash,
                                   inputs_hash = ihash,
                                   disk_location = file.path(cache$base_dir,
                                                             sprintf("code_%s", chash)),
                                   tmp_disk_location = file.path(cache$tmp_base_dir,
                                                                 sprintf("code_%s", chash)),
                                   .data = new.env(),
                                   file_stale = TRUE,
                                   plot = xxx_graphics, gdevs = gdev,
                                   write_allowed = cset$write_allowed)
            for(o in outlist)
                assign(o, get(o, envir = env), envir = newcd$.data)
            cset$add_data(newcd)
        }
        returnvalue = return_handler(xxx_returnvalue, xxx_graphics, env, evaled = TRUE, last = last)
    }
    #clean up ugly detrius once we're done.
    rm(list = c("xxx_returnvalue", "xxx_graphics", "xxx_handler"), envir = env)
    invisible(returnvalue)
}


nocache = c("par", "options", "load", "rm", "source")


## This function checks for whether there are uncachable functions
## called by the expression.

##' @title mustForce
##' @description A simple heuristic function which returns false
##'     (i.e., uncacheable) if the code calls any of the functions in
##'     \code{nonCachedFuns}, which defaults to \code{par},
##'     \code{options}, \code{load}, \code{rm}, and \code{source}.
##' @param codeInf A ScriptNodeInfo object for the code being
##'     assessed
##' @param nonCachedFuns a vector of function names which should be
##'     considered uncacheable
##' @return \code{TRUE} if any of the functions in \code{nonCachedFuns} are
##'     called, \code{FALSE} otherwise.
##' @export
mustForce = function(codeInf, nonCachedFuns = nocache)
{
    ret = FALSE
    if(length(codeInf@libraries))
        ret = TRUE
    if(any(nonCachedFuns %in% codeInf@functions))
        ret = TRUE
    ret
}

#' @title sameOutVar
#' @description this function provides a simple heuristic for when a
#'     block of code should be cached as a single entity. It returns
#'     true when all expressions in the block create or modify the
#'     same variable (or none at all), e.g.,
#' \code{
#' x=data.frame(y=rnorm(100), z=rnorm(100))
#' x$y[x$y > 3] = NA
#' x$col3 = sample(1:4, 100, TRUE)
#' x = x[1:50,]
#' }
#' @param code The code or ScriptNodeInfo to be assessed
#' @return TRUE if one (or less) unique variable is output or updated across all expressions in \code{code}, FALSE if two or more unique variables are.
#' @export
#think there's going to be a general "always right" heuristic, which
#is why the user can specify their own, or none at all.
sameOutVar = function(code)
{
    cinfo = getInputs(code)
    outputs = unlist(lapply(cinfo, function(x) c(x@outputs, x@updates)))
    if(length(unique(outputs)) == 1)
        TRUE
    else
        FALSE

}
