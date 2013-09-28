setClass("WithVisValue", representation(value = "ANY", visible = "logical"))
setClass("PlotList", contains = "list")
setClass("WithVisPlusGraphics", representation(graphics = "PlotList"), contains = "WithVisValue")

cacheClass = setRefClass("CachingEngine",
    fields = list(
        .base_dir = "character",
        base_dir = function(value)
        {
            if(missing(value))
                .base_dir
            else
                .base_dir <<- value
        },
        .tmp_base_dir = "character",
        tmp_base_dir = function(value)
        {
            if(missing(value))
                .tmp_base_dir
            else
                .tmp_base_dir <<- value
        },
        .cache_sets = "list",
        cache_sets = function(value)
        {
            if(missing(value))
                .cache_sets
            else
                {
                    if(is(value, "CodeCacheSet"))
                        {
                            hsh = value$hash
                            value = list(value)
                            names(value) = hsh
                        }
                    .cache_sets <<- value
                }
        },
        gdevs = "list",
        .write_allowed = "logical",
        write_allowed = function(value)
        {
            if(missing(value))
                .write_allowed
            else
            {
                if(is.na(value))
                {
                    warning("NULL and NAs not allowed for write_allowed field of CachingEngine objects, interpreting as TRUE")
                    value = TRUE
                }
                .write_allowed <<- value
                if(!.write_allowed)
                    sapply(.cache_set, function(x) x$write_allowed = FALSE)
            }
        },      
        .write_on_cache = "logical",
        write_on_cache = function(value)
        {
            if(missing(value))
                .write_on_cache
            else
            {
                if(!length(value) || is.na(value))
                {
                    warning("NAs not allowed for write_on_cache field of CachingEngine objects, interpeting as FALSE")
                    value = FALSE
                }
                .write_on_cache <<- value
                sync_permissions(.self, .self$cache_sets)
                value
            }
        },
        .eval_fun = "function",
        eval_fun = function(value)
        {
            if(missing(value))
                .eval_fun
            else
                {
                    if(!all(c("code", "env", "...") %in% names(formals(value))))
                        stop("eval function has incompatible signature. Must accept arguments 'code', 'env', and '...'")
                    .eval_fun <<- value
                }
        },
        .return_handler = "function",
        return_handler = function(value)
        {
            if(missing(value))
                .return_handler
            else
                {
                    if(!all(c("val", "graphics", "env", "...") %in% names(formals(value))))
                        stop("return handler function has incorrect signature. Must accept arguments 'val', 'graphics', 'env', and '...'")
                    else
                        .return_handler <<- value
                }
        }
        
        ),
    
    methods =  list(
        populate= function( dirs = c(.self$base_dir, .self$tmp_base_dir), hashes = NULL, refresh = FALSE, load_data = FALSE)
        {
            for(d in dirs)
                {
                    fils = list.dirs(d)
                    fils = fils[grepl("code_", fils)]
                    if(!is.null(hashes))
                        fils = fils[grepl(paste0("(", paste(hashes, collapse = "|"), ")"), fils)]
                    
                    newcachesets = lapply(fils, readCodeCache, load_data = load_data, write_allowed = write_allowed)
                    if(is(newcachesets, "list"))
                        newcachesets = newcachesets[!sapply(newcachesets, is.null)]
                    
                    newhashes = sapply(newcachesets, function(x) x$hash)
                    if(!refresh)
                        {
                                        #only read hashes we don't already have caches for in memory (in this cache)
                            new = which(!(newhashes %in% sapply(cache_sets, function(x) x$hash)))
                            newcachesets = newcachesets[new]
                            newhashes = newhashes[new]
                        }
                    if(length(newhashes))
                        .cache_sets[newhashes] <<-  newcachesets
                }
        },
        add_set = function(newset)
        {
           if(!write_allowed)
                newset$write_allowed <- FALSE
           else if (!length(newset$write_allowed))
               newset$write_allowed = write_allowed
            .cache_sets[[newset$hash]] <<- newset
            if(.self$write_on_cache)
                .cache_sets[[newset$hash]]$to_disk()
 
            newset
        },
        add_data = function(newdat)
        {
            chash = newdat$code_hash
            if(chash %in% names(.cache_sets))
                {
                    #caches and code-level cache sets can be more permissive but not less permissive than the CachingEngine
                    if(!write_allowed)
                        newdat$write_allowed = FALSE
                    else
                        newdat$write_allowed = .cache_sets[[chash]]$write_allowed
                    .cache_sets[[chash]]$add_data(newdat)
                        
                    #if we are adding an entirely new set, this is taken care of by that. ugly I know but we don't want to write to disk twice....
                    if(.self$write_on_cache)
                        .cache_sets[[chash]][[newdata$inputs_hash]]$to_disk()
                }
           else
                {
                    newset = new("CodeCacheSet", hash = chash, cache_dir = file.path(.self$base_dir, sprintf("code_%s", chash)), write_allowed = write_allowed)
                    .self$add_set(newset)
                }
 
            .cache_sets[[chash]]
        },
        get_or_create_set = function(code, inputs = NULL, outputs = NULL)
        {
            if(is.null(inputs)|| is.null(outputs))
                {
                    code2 = paste("{", code, "}", collapse="\n")
                    #hack to get CodeDepends to treat the code as a single block...
                    scr = readScript("", type="R", txt=code2)
                    codeInfo = getInputs(scr)[[1]]
                    
                    inputs = codeInfo@inputs
                    outputs = codeInfo@outputs
                }
            pcode = unparse(parse(text=code, keep.source=FALSE))
            if(!identical(pcode, code))
                warning("unparse(parse(code)) and code are not identical. This could potentially cause a hash mismatch. Perhaps unparsed code was passed into get_or_create_set (this should not happen)?")
            chash = digest(pcode)
            if(chash %in% names(cache_sets))
                cache_sets[[chash]]
            else
                {
                    newset = new("CodeCacheSet",
                        code = pcode,
                        hash = chash,
                        cache_dir = file.path(.self$base_dir, sprintf("code_%s", chash)),
                        inputs = inputs,
                        outputs = outputs,
                        write_on_cache = .self$write_on_cache,
                        write_allowed = .self$write_allowed)
                    .self$add_set(newset)
                    newset
                }
        },                    
        initialize = function(..., populate = TRUE)
        {
            res = callSuper(...)
            if(populate)
                .self$populate()
            res
        },
        find_data = function(code_hash, inputs_hash, check_disk = FALSE, extra_dirs = NULL)
        {
            if(code_hash %in% names(.self$cache_sets))
                .self$cache_sets[[code_hash]]$find_data(inputs_hash, check_disk, extra_dirs)
            else
                NA
        },
        to_disk = function(clear_mem = TRUE, tmp=FALSE)
        {
            if(!write_allowed)
                stop("Attempted to write to disk on a CachingEngine which has writing disabled (writing_allowed is FALSE)")
  #          if(!tmp)
  #              dir = .self$base_dir
  #          else
  #              dir = .self$tmp_base_dir
            
            invisible(sapply(cache_sets, function(x, clr) x$to_disk(clear_mem= clr), clr = clear_mem))
        }
        
        )
    )

cachingEngine = function(base_dir="./r_caches", tmp_base_dir = tempdir(),
    write_allowed = TRUE, write_on_cache = FALSE, gdevs = list(png), eval_fun = parseWithVis, return_handler = withVisHandler, populate = TRUE)
    {
        engine = cacheClass(base_dir = base_dir, tmp_base_dir = tmp_base_dir, write_allowed = write_allowed, write_on_cache = write_on_cache, gdevs = gdevs, eval_fun = eval_fun, return_handler = return_handler, populate = populate)
        engine
    }


codeCacheSet = setRefClass("CodeCacheSet",
    fields = list(
        .hash = "character",
        hash = function(value)
        {
            if(missing(value))
                .hash
            else
                .hash <<- value
        },
        .code = "character",
        code = function(value)
        {
            if(missing(value))
                .code
            else
                .code <<- value
        },
        
        .cache_data = "list",
        cache_data = function(value)
        {
            if(missing(value))
                .cache_data
            else
                {
                    if(is(value, "CachedData"))
                        {
                            hsh = value$hash
                            value = list(value)
                            names(value) = hsh
                        }
                    .cache_data <<- value
                    sync_permissions(.self, .cache_data)
                }
        },
        .tmp_cache_dir = "character",
        tmp_cache_dir = function(value)
        {
            if(missing(value))
                .tmp_cache_dir
            else
                .tmp_cache_dir <<- value
        },
        .cache_dir= "character",
        cache_dir= function(value)
        {
            if(missing(value))
                .cache_dir
            else
                .cache_dir <<- value
        },
        .inputs = "character",
        inputs = function(value)
        {
            if(missing(value))
                .inputs
            else
                .inputs <<- value
        },
        .outputs = "character",
        outputs = function(value)
        {
            if(missing(value))
                .outputs
            else
                .outputs <<- value
        },
        gdevs = "list",
       .write_allowed = "logical",
        write_allowed = function(value)
        {
            if(missing(value))
                .write_allowed
            else
            {
                if(!length(value) || is.na(value))
                {
                    warning("NAs not allowed for write_allowed field of CodeCacheSet objects, interpreting as TRUE")
                    value = TRUE
                }
                .write_allowed <<- value
                sync_permissions(.self, .self$cache_data)
                value
            }
        },      
        .write_on_cache = "logical",
        write_on_cache = function(value)
        {
            if(missing(value))
                .write_on_cache
            else
            {
                if(!length(value) || is.na(value))
                {
                    warning("NAs not allowed for write_on_cache field of CodeCacheSet objects, interpeting as FALSE")
                    value = FALSE
                }
                .write_on_cache <<- value
                sync_permissions(.self, .self$cache_data)
                value
            }
        }
    ),
    methods = list(
        find_data= function(hash, check_disk = TRUE, extra_dirs = NULL)
        {
            if(check_disk)
                .self$populate(refresh_existing = FALSE)
            if(hash %in% names(.self$cache_data))
                .self$cache_data[[hash]]
            else
                NA
        },
        to_disk = function(dir, clear_mem)
        {
            if(!write_allowed)
                stop("Attempted to write cache to disk via a CodeCacheSet with writing disallowed")
            if(missing(dir))
                dir = .self$cache_dir
            if(is.na(dir))
                stop("No directory to write to specified and no default location is available")

            if(!file.exists(dir))
                dir.create(dir, recursive=TRUE)
            sapply(.self$cache_data, function(x) x$to_disk(location = dir, clear_mem = clear_mem))
            cat(paste("##################################################",
                      "## This file was automatically generated for provenance purposes ##",
                      "## Do Not Edit ##",
                      sprintf("## Hash: %s ##", .self$hash),
                      sprintf("## Input Variables: %s ##", paste(inputs, collapse = ", ")),
                      sprintf("## Output Variables: %s ##", paste(outputs, collapse = ", ")),
                      
                      "##################################################",
                      paste(.self$code, collapse="\n"), sep="\n"),
                      file = file.path(dir, "code.R"))
        },
        add_data = function(dat)
        {
            sync_permissions(.self, dat)
            cache_data[[dat$inputs_hash]] <<- dat
            if(.self$write_on_cache)
                cache_data[[dat$inputs_hash]]$to_disk(clear_mem = FALSE)
            dat
        },
        mem_size = function()
        {
            sum(sapply(.self$cache_data, function(x) x$mem_size()))
        },
        initialize = function(..., populate = TRUE)
        {
            res = callSuper(...)
            if(populate)
                .self$populate()
            sync_permissions(.self, .self$cache_data)
            res
        },
        load_in_mem = function(hashes = names(.self$cache_data))
        {
            sapply(cache_data[hashes], function(x) x$from_disk())
        },
        unload_mem = function(write_to_disk = TRUE)
        {
            if(write_to_disk)
                .self$to_disk(clear_mem = TRUE)
            else
                sapply(cache_data, function(x) x$unload_mem())
        },
        #Want a way to only read in caches that aren't already in the set.
        populate = function(refresh_existing = FALSE, load_data = FALSE)
        {
     
          #  if(!file.exists(.self$cache_dir))
           #     dir.create(.self$cache_dir, recursive=TRUE)
            if(refresh_existing)
                excl = character()
            else
                excl = names(cache_data)
            cds = readCachedData(.self$cache_dir, load_data = load_data, exclude_hashes = excl, write_allowed = .self$write_allowed)
            hshs = sapply(cds, function(x) x$inputs_hash)
            lapply(cds, function(x) .self$add_data(x))
            #permissions are synced in add_data
            cache_data
        }
        
    )
)


cachedData = setRefClass("CachedData",
    fields = list(
        .code_hash = "character",
        code_hash = function(value)
        {
            if(missing(value))
                .code_hash
            else
                .code_hash <<- value
        },
        .inputs_hash = "character",
        inputs_hash = function(value)
        {
            if(missing(value))
                .inputs_hash
            else
                .inputs_hash <<- value
        },
        disk_location = "character",
        tmp_disk_location = "character",
        .data = "environment",
        last_used = "ANY",
        plot = "ANY",
        gdevs = "list",
        file_stale = "logical",
       .write_allowed = "logical",
        write_allowed = function(value)
        {
            if(missing(value))
                .write_allowed
            else
            {
                if(!length(value) || is.na(value))
                {
                    warning("NAs not allowed for write_allowed field of CachedData objects, interpreting as TRUE")
                    value = TRUE
                }
                .write_allowed <<- value
            }
        },
#XXX This seems really unnecessary, CachedData objects don't ever "cache", its purely a hack to allow sync_permissions to be general. Find a better solution!
        .write_on_cache = "logical",
        write_on_cache = function(value)
        {
            if(missing(value))
                .write_on_cache
            else
            {
                if(!length(value) || is.na(value))
                {
                    warning("NAs not allowed for write_on_cache field of CachedData objects, interpeting as FALSE")
                    value = FALSE
                }
                .write_on_cache <<- value
            }
        }
    ),
methods = list(
        mem_size = function()
        {
            if(!length(ls(envir = .data)))
                0
            else
                sum(sapply(.data, object.size))
        },
        retrieve_data = function( env = parent.frame()) #XXX this may go all wonky and grab the wrong frame. Check it!!!
        {
            if(!length(ls(env=.data)) && length(disk_location))
                .self$from_disk()

            if(!length(ls(env=.data)))
                stop(sprintf("No data found in this cache (hash %s)", inputs_hash))

            nams = ls(env=.data)
            for(n in nams)
                assign(n, get(n, .data), env)
            last_used <<- Sys.time()
            nams
        },
        unload_mem = function()
        {
            rm(list = ls(envir = .data), envir = .data)
        },
        to_disk = function(tmp = FALSE, clear_mem = TRUE,force = FALSE, location)
        {
            if(!write_allowed)
                stop("Attempted to write cache to disk via CachedData with writing disallowed")
            #check if there is something to write, otherwise immediately return
            if(!is.na(file_stale) && !file_stale && !force && (location == disk_location || (tmp && location == tmp_disk_location)))
                return()
            
            if(!missing(location))
                {
                    if(tmp)
                        {
                            if(length(tmp_disk_location) && !identical(tmp_disk_location, location))
                                warning(sprintf("Modifying tmp disk location of existing cache for hash %s from %s to %s", inputs_hash, tmp_disk_location, location))
                            .self$tmp_disk_location = location
                        } else {
                            if(length(disk_location) && !identical(disk_location, location))
                                warning(sprintf("Modifying disk location of existing cache for hash %s from %s to %s", inputs_hash, disk_location, location))
                            .self$disk_location = location
                        }
                } else {
                    if(tmp)
                        location = .self$tmp_disk_location
                    else
                        location = .self$disk_location
                }
            nams = ls(env = .data)
            if(!file.exists(location))
                dir.create(location, recursive=TRUE)
            save(list=nams, envir=.data, file = file.path(location, paste0(paste("cache",inputs_hash, sep="_"), ".rda")))
            if(length(.data$xxx_graphics))
                {
                    mapply(function(ext, f)
                           {
                               for(i in seq(along = .data$xxx_graphics))
                               {
                                   f(file.path(location, paste0("plot_",i, "_", inputs_hash, ".", ext)))
                                   redrawPlot(.data$xxx_graphics[[i]])
                                   dev.off()
                               }
                           }, names(gdevs), gdevs)
                }
            if(clear_mem)
#                rm(list = nams, envir = .data)
                .self$unload_mem()
            nams
        },
        from_disk = function(location, hsh)
        {
            if(missing(location))
                {
                    if(length(disk_location))
                        location = disk_location
                    else if (length(tmp_disk_location))
                        location = tmp_disk_location
                    else
                        stop("Unable to determine cache location on disk (tmp or permanent).")
                }
            if(missing(hsh) && length(inputs_hash))
                hsh = inputs_hash
            else if (length(inputs_hash))
                {
                    warning("modifying hash associated with an existing cache is dangerous and is likely to invalidate the stored data.")
                    inputs_hash <<- hsh
                } else if (missing(hsh)) {
                    stop("Attempted to call from_disk with no hash specified or associated with the Cache object")
                }
            load(file=file.path(location, paste0(
                    "cache_", hsh, ".rda")), envir = .data)
        })
    )


sync_permissions = function(from, to, full_sync = FALSE)
{
    if(!is(to, "list"))
        to = list(to)
    if(full_sync|| (length(from$write_allowed) && !from$write_allowed))
        sapply(to, function(x) x$write_allowed = from$write_allowed)
    if(full_sync|| (length(from$write_on_cache) && from$write_on_cache))
        sapply(to, function(x) x$write_on_cache= from$write_on_cache)
}

                    
