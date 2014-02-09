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


parseEval = function(code, env, ...) eval(parse(text=code), envir=env)

parseWithVis = function(code, env, ...) {
#    ret = withVisible(eval(parse(text=code), envir= env))
    ret = withVisible(eval(code, envir = env))
    new("WithVisValue", value = ret$value, visible = ret$visible)
}

#@val is an arbitrary object in the exact form returned by the evaluation function
#@graphics is a PlotList  object (essentially a list) with 0 or more recordedplot objects in it

withVisHandler = function(val, graphics, env, evaled = FALSE, last = FALSE, ...)
{
    raw = withVisRaw(val, graphics, env, evaled, last,  ...)
    invisible(raw@value)
}


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


returnRaw = function(val, graphics, env, evaled = FALSE, ...)
{
    if(length(graphics) && !evaled)
        lapply(graphics, redrawPlot)
    val
}

noGraphicsRaw = function(val, graphics, env, evaled = FALSE, ...)
{
    val
}

evalWithCache = function(code,
    codeInfo,
    inputVars, #overrides automatically detected inputs
    outputVars, #overrides automatically detected outputs
    #The default here is write_on_cache=TRUE because we don't return the cache engine so if it isn't passed in its not retained anywhere;.
    #This replicates existing caching behaviors where the only caching action s are one-time read from/write to disk.
    #Passing in an existing CachingEngine objects modifies that object in place and gives access to the full spectrum of caching behaviors offered by the package.
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
    asSingleEnt = FALSE,
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
        if(asSingleEnt || (is.function(singleEntFun) && singleEntFun(code)))
        {
            oneExpr = paste("{", paste(as(code, "character"), collapse = "\n"), "}", collapse = "\n")
            code = readScript("", "R", oneExpr)
        } else {

            lapply(code[-length(code)], evalWithCache, cache = cache, eval_fun = eval_fun, return_handler = return_handler, env = env, force = force, cacheRand = cacheRand, verbose = verbose, gexts = gexts, gdev = gdev, last = FALSE, ...)
            return(evalWithCache(code[[length(code)]], cache = cache, eval_fun = eval_fun, return_handler = return_handler, env = env, force = force, cacheRand = cacheRand, verbose = verbose, gexts = gexts, gdev = gdev, last = TRUE, ...))
        }
    }

    if(missing(codeInfo))
    {
        codeInfo = getInputs(code)
        if(!is(codeInfo, "ScriptNodeInfo"))
            codeInfo = codeInfo[[1]]
    }


    #we can't cache certain types of expressions, calls to library, load, par, options, etc. The unCacheable argument accepts a function which accepts codeInfo for an expression and returns TRUE if the expression is uncacheable.
    if(!force)
    {
        if(unCacheable(codeInfo))
            force = TRUE
    }
    
    if(missing(inputVars)||missing(outputVars))
    {
        if(missing(inputVars))
            inputVars = c(codeInfo@inputs, codeInfo@updates)
        if(missing(outputVars))
            outputVars = c(codeInfo@outputs, codeInfo@updates)
    }

    if(is(code, "Script"))
        code = code[[1]]
    #parse and deparse the code to get rid of the annoying "adding a space invalidates the cache" issue other systems have

    pcode = get_parsed_code(code)
    if(verbose)
        cat(paste(pcode, ":  "))
    #do the required inputs even exist in the current session?
    in_exist = sapply(inputVars, exists, envir = env)
    if(!all(in_exist))
    {
        if(stopMissingInput)
            stop(paste("Missing variable(s) required to evaluate codeblock:", inputVars[!in_exist], collapse = " "))
        else
            warning(paste("Missing inputs? Variables detected as inputs but not found:", inputVars[!in_exist], collapse = " "))
    }
            
    
    #make the list we will fastdigest to get the hash. It includes the (properly handled) code as well as the current values of all the input variables
    chash = fastdigest(pcode)
    ihash = fastdigest(mget(inputVars, envir = env, ifnotfound=list(NULL)))
    
    fnd = cache$find_data(chash, ihash)
    if(is(fnd, "CachedData") && !force)
    {
        if(verbose)
            cat("using matching cache.\n")
        fnd$retrieve_data(env)
        xxx_returnvalue = get("xxx_returnvalue", env)
        #The handler handles reproducing side effects such as printing, warning/error message, and graphics.
        if(!is.null(env$xxx_handler))
            returnvalue = env$xxx_handler(xxx_returnvalue, env$xxx_graphics, , env = env, evaled = FALSE, last = last)
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
            newcd = cachedData$new(code_hash = chash, inputs_hash = ihash, disk_location = file.path(cache$base_dir, sprintf("code_%s", chash)), tmp_disk_location = file.path(cache$tmp_base_dir, sprintf("code_%s", chash)), .data = new.env(), file_stale = TRUE, plot = xxx_graphics, gdevs = gdev, write_allowed = cset$write_allowed)
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
#This function checks for whether there are uncachable functions called by the expression.
mustForce = function(codeInf, nonCachedFuns = nocache)
{
    ret = FALSE
    if(length(codeInf@libraries))
        ret = TRUE
    if(any(nonCachedFuns %in% codeInf@functions))
        ret = TRUE
    ret
}

#this function provides a simple heuristic for when a block of code should be cached as a single entity. 
#It returns true when all expressions in the block create or modify the same variable (or none at all), i.e.
#x=data.frame(y=rnorm(100), z=rnorm(100))
#x$y[x$y > 3] = NA
#x$col3 = sample(1:4, 100, TRUE)
#This could be made more sophisticated but I don't think there's going to be a general "always right" heuristic, which is why the user can specify their own, or none at all.
sameOutVar = function(code)
{
    cinfo = getInputs(code)
    outputs = unlist(lapply(cinfo, function(x) c(x@outputs, x@updates)))
    if(length(unique(outputs)) == 1)
        TRUE
    else
        FALSE

}
