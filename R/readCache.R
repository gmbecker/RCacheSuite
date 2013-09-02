readCachedData = function(dir, load_data = TRUE, exclude_hashes = character(), write_allowed = TRUE, write_on_cache = FALSE)
    {
        if(!file.exists(dir))
            return(list())
        chash = gsub(".*_([[:alnum:]]*).*", "\\1", normalizePath(file.path(dir, "..")))
        fils = list.files(dir, pattern="cache_", full.names=TRUE)
        if(length(exclude_hashes))
        {
            excl = match(exclude_hashes, fils)
            fils = fils[ excl == 0 ]
        }
        fils = fils[!is.na(fils)]
        ihashes = gsub(".*_([[:alnum:]]*).*", "\\1", fils)
        
        if(!length(fils))
            return(list())
        mapply(function(file, ihash, chash)
               {
                   cacheout = new("CachedData", code_hash = chash, inputs_hash = ihash, .data = new.env(),disk_location = dirname(file), file_stale = FALSE, write_allowed = write_allowed, write_on_cache = write_on_cache)
                   if(load_data)
                       load(file = file, envir = cacheout$.data)
                   cacheout
               }, file = fils, ihash = ihashes, chash = chash)
    }


readCodeCache = function(dir, load_data = TRUE, write_allowed = TRUE, write_on_cache = FALSE)
{
    if(!validCodeCache(dir))
        return(NULL)
    hash = gsub(".*_([[:alnum:]]*).*", "\\1", dir)
    code = unparse(parse(file.path(dir, "code.R"), keep.source=FALSE))
    cSetOut = new("CodeCacheSet", hash = hash, cache_dir = dir, code = code, write_allowed = write_allowed, write_on_cache = write_on_cache)
    cSetOut$populate(load_data = load_data)
    cSetOut
}
