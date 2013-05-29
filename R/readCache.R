readCachedData = function(dir, load_data = TRUE)
    {
        hash = gsub(".*_([[:alnum:]]*).*", "\\1", dir)
        cacheout = new("CachedData", hash = hash, .data = new.env(),disk_location = dir, file_stale = FALSE)
        if(load_data)
            load(file = file.path(dir, paste0(hash, ".rda")), envir = cacheout$.data)
        cacheout
    }
