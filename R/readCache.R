readCachedData = function(dir, load_data = TRUE)
    {
        hash = gsub(".*_([[:alnum:]]*).*", "\\1", dir)
        cacheout = new("CachedData", hash = hash, .data = new.env(),disk_location = dir, file_stale = FALSE)
        if(load_data)
            load(file = file.path(dir, paste0(hash, ".rda")), envir = cacheout$.data)
        cacheout
    }


readCodeCache = function(dir, load_data = TRUE)
    {
      hash = gsub(".*_([[:alnum:]]*).*", "\\1", dir)
      code = deparse(parse(file.path(dir, "code.R")))
      cSetOut = new("CodeCacheSet", hash = hash, cache_dir = dir, code = code)
      cSetOut$populate(load_data = load_data)
      cSetOut
    }
