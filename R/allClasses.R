cacheClass = setRefClass("Cache",
            fields = list(
                .cache_data = "list",
                 cache_data = function(value)
                {
                    if(missing(value))
                        .cache_data
                    else
                        .cache_data <<- value
                },
                .tmp_cache_dir = "character",
                tmp_cache_dir = function(value)
                {
                    if(missing(value))
                        .tmp_cache_dir
                    else
                        .tmp_cache_dir <<- value
                },
                .type = "character",
                type = function(value)
                {
                    if(missing(value))
                        .type
                    else
                        {
                            value = match.arg(value, c("both", "memory", "disk", "disk-tmp"))
                            .type <<- value
                        }
                },
                .cache_dirs= "character",
                cache_dirs= function(value)
                {
                    if(missing(value))
                        .cache_dirs
                    else
                        .cache_dirs <<- value
                }),
            methods = list(
                find_data= function(hash, check_disk = TRUE, extra_dirs = NULL)
                {
                    if(hash %in% names(.self$cache_data))
                        return(.self$cache_data[[hash]])
                    ret = NA
                    if(check_disk)
                        {
                            dirs = c(.self$tmp_cache_dir, extra_dirs)
                            allcache_data = unlist(lapply(dirs, function(d) {
                                ret = list.dirs(d)
                                ret[grepl("cache", ret)]
                            }))
                            hasHash = grepl(hash, allcache_data)
                            if(any(hasHash))
                                {
                                    ret = readCachedData(allcache_data[hasHash][1])
                                    .self$add_data(ret)
                                }
                        }

                    ret
                },
                to_disk = function(dir, clear_mem)
                {
                    if(missing(dir))
                        dir = .self$cache_dirs[1]
                    if(is.na(dir))
                        stop("No directory to write to specified and no default location is available")

                    sapply(.self$cache_data, function(x) x$to_disk(location = dir, clear_mem = clear_mem))
                },
                populate= function( dirs = c(.self$cache_dirs, .self$tmp_cache_dir), hashes = NULL, refresh = FALSE, load_data = FALSE)
                {
                    for(d in dirs)
                        {
                            fils = list.dirs(d)
                            fils = fils[grepl("cache", fils)]
                            if(!is.null(hashes))
                                fils = fils[grepl(paste0("(", paste(hashes, collapse = "|"), ")"), fils)]

                            newcdata = lapply(fils, readCachedData, load_data= load_data)
                            newhashes = sapply(newcdata, function(x) x$hash)
                            if(!refresh)
                                {
                                    #only read hashes we don't already have caches for in memory (in this cache)
                                    new = which(!(newhashes %in% sapply(cache_data, function(x) x$hash)))
                                    newcdata = newcdata[new]
                                    newhashes = newhashes[new]
                                }
                            
                            .cache_data[newhashes] <<-  newcdata
                        }
                },
                add_data = function(dat)
                {
                    cache_data[[dat$hash]] <<- dat
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
                }
                
                )
    )


cachedData = setRefClass("CachedData",
    fields = list(
        .hash = "character",
        hash = function(value)
        {
            if(missing(value))
                .hash
            else
                .hash <<- value
        },
        disk_location = "character",
        tmp_disk_location = "character",
        .data = "environment",
        last_used = "ANY",
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
                stop(sprintf("No data found in this cache (hash %s)", hash))

            nams = ls(env=.data)
            for(n in nams)
                assign(n, get(n, .data), env)
            last_used <<- Sys.time()
            nams
        },
        unload_mem = function()
        {
            rm(list = ls(envir = .data), env = .data)
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
                                warning(sprintf("Modifying disk location of existing cache for hash %s", hash))
                            .self$disk_location = location
                        } else {
                            if(length(tmp_disk_location))
                                warning(sprintf("Modifying tmp disk location of existing cache for hash %s", hash))
                            .self$tmp_disk_location = location
                        }
                } else {
                    if(tmp)
                        location = .self$tmp_disk_location
                    else
                        location = .self$disk_location
                }
            nams = ls(env = .data)
            save(list=nams, envir=.data, file = file.path(location, paste0(paste("cache",hash, sep="_"), ".rda")))
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
            if(missing(hsh) && length(hash))
                hsh = hash
            else if (length(hash))
                {
                    warning("modifying hash associated with an existing cache is dangerous and is likely to invalidate the stored data.")
                    hash <<- hsh
                } else if (missing(hsh)) {
                    stop("Attempted to call from_disk with no hash specified or associated with the Cache object")
                }
            load(file=file.path(location, paste0(hsh, ".rda")), envir = .data)
        })
    )
        
                    
