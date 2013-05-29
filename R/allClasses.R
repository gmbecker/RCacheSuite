cache = setRefClass("Cache",
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
                        .type <<- value
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
                find_cache = function(hash, check_disk = TRUE, extra_dirs = NULL)
                {
                    if(hash %in% names(.self$cache_data))
                        return(.self$cache_data[[hash]])
                    ret = NA
                    if(check_disk)
                        {
                            dirs = c(.self$temp_cach_dir, extra_dirs)
                            allcache_data = unlist(lapply(dirs, function(d) {
                                ret = list.dirs(d)
                                ret[grepl("cache", ret)]
                            }))
                            hasHash = grepl(hash, allcache_data)
                            if(any(hasHash))
                                ret = readCache(allcache_data[hasHash][1])
                        }

                    ret
                },
                to_disk = function(dir)
                {
                    if(missing(dir))
                        dir = .self$cache_dirs[1]
                    if(is.na(dir))
                        stop("No directory to write to specified and no default location is available")

                    sapply(.self$cache_data, function(x) x$to_disk(dir))
                },
                from_disk = function( dirs, hashes = NULL)
                {
                    for(d in dirs)
                        {
                            fils = list.dirs(d)
                            fils = fils[grepl("cache", fils)]
                            if(!is.null(hashes))
                                fils = fils[grepl(paste0("(", paste(hashes, collapse = "|"), ")"), fils)]

                            .self$cache_data = c(.self$cache_data, lapply(fils, readCache))
                        }
                },
                mem_size = function()
                {
                    sum(sapply(.self$cache_data, function(x) x$mem_size()))
                ))


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
        last_used = "ANY"),
    methods = list(
        mem_size = function()
        {
            if(is.na(data))
                0
            else
                as.integer(object.size(.self$.data))
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
        to_disk = function(tmp = FALSE, clear_mem = TRUE,location)
        {
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
                rm(list = nams, envir = .data)
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
            load(file=file.path(location, paste0(paste("cache", hsh, sep="_"), ".rda")), envir = .data)
        })
    )
        
                    
