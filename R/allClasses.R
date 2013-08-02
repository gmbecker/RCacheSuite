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
        gdevs = "list"),
    
    methods =  list(
        populate= function( dirs = c(.self$base_dir, .self$tmp_base_dir), hashes = NULL, refresh = FALSE, load_data = FALSE)
        {
            for(d in dirs)
                {
                    fils = list.dirs(d)
              #      fils = fils[grepl("cache", fils)]
                    fils = fils[grepl("code_", fils)]
                    if(!is.null(hashes))
                        fils = fils[grepl(paste0("(", paste(hashes, collapse = "|"), ")"), fils)]
                    
                   #newcdata = lapply(fils, readCachedData, load_data= load_data)
                    newcachesets = lapply(fils, readCodeCache, load_data = load_data)
                    newhashes = sapply(newcachesets, function(x) x$hash)
                    if(!refresh)
                        {
                                        #only read hashes we don't already have caches for in memory (in this cache)
                            new = which(!(newhashes %in% sapply(cache_sets, function(x) x$hash)))
                            newcachesets = newcachesets[new]
                            newhashes = newhashes[new]
                        }
                    
                    .cache_sets[newhashes] <<-  newcachesets
                }
        },
        add_set = function(newset)
        {
            .cache_sets[[newset$hash]] <<- newset
        },
        add_data = function(newdat)
        {
            chash = newdat$code_hash
            if(chash %in% names(.cache_sets))
                .cache_sets[[chash]]$add_data(newdat)
            else
                {
                    newset = new("CodeCacheSet", hash = chash, cache_dir = file.path(.self$base_dir, sprintf("code_%s", chash)))
                    
                }
        },
        get_or_create_set = function(code, inputs = NULL, outputs = NULL)
        {
        
                        
            if(is.null(inputs)|| is.null(outputs))
                {
                    code2 = paste("{", code, "}", collapse="\n")
                    #hack to get CodeDepends to treat the code as a single block...
                    #if(!(grepl("\\{", code2)[1] && grepl("\\}", code2)[length(code2)]))
                    # code2 = paste("{", code2, "}", collapse="\n")
                        
                    scr = readScript("", type="R", txt=code2)
                    codeInfo = getInputs(scr)[[1]]
                    
                    inputs = codeInfo@inputs
                    outputs = codeInfo@outputs
                }
            pcode = unparse(parse(text=code, keep.source=FALSE))
            if(!identical(pcode, code))
                warning("unparse(parse(code)) and code are not identical. This could potentially cause a hash mismatch. Perhaps unparsed code was passed into get_or_create_set?")
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
                        outputs = outputs)
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
            if(!tmp)
                dir = .self$base_dir
            else
                dir = .self$tmp_base_dir
            
        #    invisible(sapply(cache_sets, function(x, dir, clr) x$to_disk(dir, clr), dir = dir, clr = clear_mem))
            invisible(sapply(cache_sets, function(x, clr) x$to_disk(clear_mem= clr), clr = clear_mem))
        }
        
        )
    )


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
        gdevs = "list"
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
        #    ret = NA
        #    if(check_disk)
        #        {
        #            dirs = c(.self$tmp_cache_dir, extra_dirs)
        #            allcache_data = unlist(lapply(dirs, function(d) {
        #                ret = list.dirs(d)
        #                ret[grepl("cache", ret)]
        #            }))
        #            hasHash = grepl(hash, allcache_data)
        #            if(any(hoasHash))
        #                {
        #                    ret = readCachedData(allcache_data[hasHash][1])
        #                    .self$add_data(ret)
        #                }
        #        }
        #    
        #    ret
        },
        to_disk = function(dir, clear_mem)
        {
            if(missing(dir))
                dir = .self$cache_dir
            if(is.na(dir))
                stop("No directory to write to specified and no default location is available")
            
            sapply(.self$cache_data, function(x) x$to_disk(location = dir, clear_mem = clear_mem))
            cat(paste("##################################################",
                      "## This file was automatically generated for provenance purposes ##",
                      "## Do Not Edit ##",
                      sprintf("## Hash: %s ##", .self$hash),
                      "##################################################",
                      paste(.self$code, collapse="\n"), sep="\n"),
                      file = file.path(dir, "code.R"))
        },
        add_data = function(dat)
        {
            cache_data[[dat$inputs_hash]] <<- dat
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
          #  fils = list.files(.self$cache_dir)
           # cachefils = fils[grepl("cache_", fils)]
            #cds = lapply(cachefils, readCachedata)
            if(!file.exists(.self$cache_dir))
                dir.create(.self$cache_dir, recursive=TRUE)
            cds = readCachedData(.self$cache_dir, load_data = load_data)
            hshs = sapply(cds, function(x) x$inputs_hash)
            if(!refresh_existing)
                {
                    inds = which(!(hshs %in% names(cache_data)))
                    hshs = hshs[inds]
                    cds = cds[inds]
                }
            cache_data[hshs] <<- cds
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
        file_stale = "logical"),
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
            #check if there is something to write, otherwise immediately return
            if(!is.na(file_stale) && !file_stale && !force && (location == disk_location || (tmp && location == tmp_disk_location)))
                return()
            
            if(!missing(location))
                {
                    if(tmp)
                        {
                            if(length(disk_location))
                                warning(sprintf("Modifying disk location of existing cache for hash %s", inputs_hash))
                            .self$disk_location = location
                        } else {
                            if(length(tmp_disk_location))
                                warning(sprintf("Modifying tmp disk location of existing cache for hash %s", inputs_hash))
                            .self$tmp_disk_location = location
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
                               f(file.path(location, paste0("plot_", inputs_hash, ".", ext)))
                               redrawPlot(.data$xxx_graphics)
                               dev.off()
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
        
                    
