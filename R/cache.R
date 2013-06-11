#Other caching mechanisms in R:
# Biobase::cache Checks in cache for LHS of LHS<-RHS expression and only evaluates RHS if not found in cache
#R.cache package
#cacheSweave
#knitr with cache=TRUE
#SOAR package, not really the same thing, basically create lazyload dbs for things on the fly and take them out of memory?
#cacher

parseEval = function(code, env, ...)
    {
        eval(parse(text = code), envir=env)
    }


##Old Version, New version below uses cache classes and supports in memory and on disk caching
if(FALSE)
    {
evalWithCache = function(code, codeInfo, inputvars, outputvars, cachedir = "./", evaluator = function(x) parseEval(x, envir), clearStaleCache = FALSE, verbose = FALSE, envir = .GlobalEnv, ...)
  {
    if(missing(inputvars)||missing(outputvars))
      {
        if(missing(codeInfo))
          {
            code2 = paste("{", code, "}", collapse="\n")
            #hack to get CodeDepends to treat the code as a single block...
            #if(!(grepl("\\{", code2)[1] && grepl("\\}", code2)[length(code2)]))
             # code2 = paste("{", code2, "}", collapse="\n")

            scr = readScript("", type="R", txt=code2)
            codeInfo = getInputs(scr)[[1]]
          }
        inputvars = codeInfo@inputs
        outputvars = codeInfo@outputs
       
      }

    #parse and deparse the code to get rid of the annoying "adding a space invalidates the cache" issue other systems have
    pcode = deparse(parse(text=code, keep.source=FALSE))

    #do the required inputs even exist in the current session?
    in_exist = sapply(inputvars, exists)
    if(!all(in_exist))
      stop(paste("Missing variable(s) required to evaluate codeblock:", inputvars[!in_exist], collapse = " "))
    
    musteval = TRUE
    #make the list we will digest to get the hash. It includes the (properly handled) code as well as the current values of all the input variables
    diglist = c(pcode, lapply(inputvars, get))
    hash = digest(diglist)
    hashdir = file.path(cachedir, paste("cache", hash, sep="_"))
    if(file.exists(hashdir))
      {
        if(verbose)
          cat(paste0("\nMatching cache found for hash:\n\t", hash, "\n\tLoading previously cached results.\n"))

        load(file.path(hashdir, paste0(hash, ".rda")))
        musteval = FALSE
        
      }
  
    if(musteval)
      {
        if(verbose)
          cat(paste0("\nNo matching cache found for hash:\n\t", hash, "\n"))
        xxx_returnvalue = evaluator(code, ...)
        outlist = c("xxx_returnvalue", outputvars, "pcode")
        dir.create(hashdir)
        save(list = outlist, file = file.path(hashdir, paste0(hash, ".rda")))
      }
        
    
    if(musteval && clearStaleCache)
      {
        #code to delete old cache goes here
        NULL
      }
    return(xxx_returnvalue)
  }

}

#because we don't return it, the cache MUST already exist!
#Actually, I guess if we didn't, it could just write the cache to disk as it is released, reproducing behavior of other caching systems
evalWithCache = function(code, codeInfo, inputVars, outputVars, cache, evaluator = parseEval, env = .GlobalEnv, verbose = FALSE, force = FALSE, ...)
    {

        if(missing(inputVars)||missing(outputVars))
            {
                if(missing(codeInfo))
                    {
                        code2 = paste("{", code, "}", collapse="\n")
                                        #hack to get CodeDepends to treat the code as a single block...
                                        #if(!(grepl("\\{", code2)[1] && grepl("\\}", code2)[length(code2)]))
                                        # code2 = paste("{", code2, "}", collapse="\n")
                        
                        scr = readScript("", type="R", txt=code2)
                        codeInfo = getInputs(scr)[[1]]
                    }
                inputVars = codeInfo@inputs
                outputVars = codeInfo@outputs
                
            }
        
         #parse and deparse the code to get rid of the annoying "adding a space invalidates the cache" issue other systems have
        pcode = deparse(parse(text=code, keep.source=FALSE))

    #do the required inputs even exist in the current session?
        in_exist = sapply(inputVars, exists)
        if(!all(in_exist))
            stop(paste("Missing variable(s) required to evaluate codeblock:", inputVars[!in_exist], collapse = " "))
        
        #make the list we will digest to get the hash. It includes the (properly handled) code as well as the current values of all the input variables
        #diglist = c(pcode, lapply(inputVars, get))
        chash = digest(pcode)
        ihash = digest(lapply(inputVars, get))

        fnd = cache$find_data(chash, ihash)
        if(is(fnd, "CachedData") && !force)
            {
                if(verbose)
                    cat(paste(sprintf("\nExisting cache found: %s %s", chash, ihash),"\n"))
                fnd$retrieve_data(env)
                xxx_returnvalue = get("xxx_returnvalue", env)
            } else {
                if(verbose)
                    cat(paste(sprintf("\nNo cache found. Creating new cache: %s %s",chash, ihash), "\n"))
                
                xxx_returnvalue = evaluator(code, env,  ...)
                outlist = c("xxx_returnvalue", outputVars, "pcode")
                #XXX right now it always assigns the cache to the first location in cache_dirs, even if there are more than one
                cset = cache$get_or_create_set(chash, inputVars, outputVars)
                newcd = cachedData$new(code_hash = chash, inputs_hash = ihash, disk_location = file.path(cache$base_dir, sprintf("code_%s", chash)), tmp_disk_location = file.path(cache$tmp_base_dir, sprintf("code_%s", chash)), .data = new.env(), file_stale = TRUE)
                for(o in outlist)
                    assign(o, get(o), newcd$.data)
               # cache$add_data(newcd)
                cset$add_data(newcd)
            }
        return(xxx_returnvalue)
    }
