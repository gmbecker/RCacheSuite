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


parseEval = function(code, env, ...) eval(parse(text=code), env=env)

parseWithVis = function(code, env, ...) withVisible(eval(parse(text=code), env= env))

parseEval2 = function(code, env, ...)
    {
        pcode = parse(text=code)
        res = withVisible(eval(pcode, envir = env))
        ret = res$value
        if(res$visible)
            show(ret)
        invisible(ret) #if it needed to be shown we already did, so we return it invisibly
    }

withVisHandler = function(val, graphics, evaled = FALSE, ...)
    {
        if(length(graphics) && !evaled)
            redrawPlot(graphics)
        if(val$visible)
            show(val$value)
        val$value
    }


#because we don't return it, the cache MUST already exist!
#Actually, I guess if we didn't, it could just write the cache to disk as it is released, reproducing behavior of other caching systems
evalWithCache = function(
    code,
    codeInfo,
    inputVars,
    outputVars,
    #The default here is write_on_cache=TRUE because we don't return the cache engine so if it isn't passed in its not retained anywhere;.
    #This replicates existing caching behaviors where the only caching actions are one-time read from/write to disk.
    #Passing in an existing CachingEngine objects modifies that object in place and gives access to the full spectrum of caching behaviors offered by the package.
    cache = cachingEngine(write_on_cache = TRUE),
    env = .GlobalEnv,
    force = FALSE,
    cacheRand = FALSE,
    verbose = FALSE,
    gexts = "png",
    gdev = sapply(gexts, function(nm) get(nm, mode="function")),

    ...)
    {
        if(length(code) > 1)
            {
                if(all(grepl("\\n", code[-length(code)])))
                    code = paste(code, collapse="")
                else
                    code = paste(code, collapse="\n")
            }


        if(missing(inputVars)||missing(outputVars))
            {
                if(missing(codeInfo))
                    {
                        code2 = paste("{", code, "}", collapse="\n")
                                        #hack to get CodeDepends to treat the code as a single block...
                                
                        scr = readScript("", type="R", txt=code2)
                        codeInfo = getInputs(scr)[[1]]
                    }
                inputVars = codeInfo@inputs
                outputVars = codeInfo@outputs
                
            }
        
         #parse and deparse the code to get rid of the annoying "adding a space invalidates the cache" issue other systems have
        #pcode = deparse(parse(text=code, keep.source=FALSE))
      #  pcode = sapply(parse(text=code, keep.source=FALSE), deparse)
        pcode = unparse(parse(text=code, keep.source=FALSE))
    #do the required inputs even exist in the current session?
        in_exist = sapply(inputVars, exists, envir = env)
        if(!all(in_exist))
            stop(paste("Missing variable(s) required to evaluate codeblock:", inputVars[!in_exist], collapse = " "))
        
        #make the list we will digest to get the hash. It includes the (properly handled) code as well as the current values of all the input variables
        #diglist = c(pcode, lapply(inputVars, get))
#        chash = digest(sapply(pcode, deparse))
        chash = digest(pcode)
        ihash = digest(lapply(inputVars, get, envir = env))

        fnd = cache$find_data(chash, ihash)
        if(is(fnd, "CachedData") && !force)
            {
                if(verbose)
                    cat(paste(sprintf("\nExisting cache found: %s %s", chash, ihash),"\n"))
                fnd$retrieve_data(env)
                xxx_returnvalue = get("xxx_returnvalue", env)
                #The handler handles side effects such as printing, warning/error message, and graphcis.
                if(!is.null(env$xxx_handler))
                    returnvalue = env$xxx_handler(xxx_returnvalue, env$xxx_graphics, evaled = FALSE)
                else
                    {
                        if(length(env$xxx_graphics))
                                        #  replayPlot(env$xxx_graphics)
                            redrawPlot(env$xxx_graphics)
                    }
            } else {
                if(verbose)
                    cat(paste(sprintf("\nNo cache found. Creating new cache: %s %s",chash, ihash), "\n"))
                if(exists(".Random.seed"))
                    oldRS = .Random.seed
                else
                    oldRS = NULL
                
                oldplot = if(dev.cur() > 1) recordPlot() else NULL
                olddev = dev.cur()

                xxx_returnvalue = cache$eval_fun(code = code, env = env, ...)
                                         
                assign("xxx_returnvalue", xxx_returnvalue, env = env)
                assign("xxx_handler", cache$return_handler, env = env)
                xxx_graphics = NULL
                #check if there an active graphics device and if so if its contents are different than
                #the contents
                # Hadley's evaluate package does a more sophisticated version of this check, but do we want to depend on it just for that?
                newdev = dev.cur()
                if(newdev > 1 && (is.null(oldplot) || newdev != olddev || ! identical(oldplot, recordPlot() ) ) )
                    xxx_graphics = recordPlot()
                
                assign("xxx_graphics", xxx_graphics, env)
                
                #XXX previously outlist included pcode. why did I think i needed the pcode value? Am I using it somehwere?
                outlist = c("xxx_returnvalue", "xxx_handler",  outputVars)
                if(length(xxx_graphics))
                    outlist = c(outlist, "xxx_graphics")
                
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
                        newcd = cachedData$new(code_hash = chash, inputs_hash = ihash, disk_location = file.path(cache$base_dir, sprintf("code_%s", chash)), tmp_disk_location = file.path(cache$tmp_base_dir, sprintf("code_%s", chash)), .data = new.env(), file_stale = TRUE, plot = xxx_graphics, gdevs = gdev)
                        for(o in outlist)
                            assign(o, get(o, env = env), newcd$.data)
                        cset$add_data(newcd)
                    }
                # this is probably going to duplicate side-effects which occur during the actual evaluation process??
                # Add a mandatory evaled argument and leave it up to the handler to know what to recreate
                returnvalue = cache$return_handler(xxx_returnvalue, xxx_graphics, evaled = TRUE)
            }
        invisible(returnvalue)
    }
