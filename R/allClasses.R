
#'@import CodeDepends fastdigest methods
#' @import grDevices
NULL

## defines the columns (including order) that
## a provenance store dataframe is expected
## to have.
provdf = function(outputvar = character(),
                  outputvarhash = character(),
                  inputvar = character(),
                  inputvarhash = character(),
                  agent = character(),
                  code = character(),
                  codehash = vapply(code, fastdigest, character(1))) {
    data.frame(outputvar = outputvar,
               outputvarhash = outputvarhash,
               inputvar = inputvar,
               inputvarhash = inputvarhash,
               agent = agent,
               code = code,
               codehash = codehash,
               stringsAsFactors = FALSE)
}
                       

ProvStoreDF = function(outputvar = character(),
                  outputvarhash = character(),
                  inputvar = character(),
                  inputvarhash = character(),
                  agent = character(),
                  code = character(),
                  codehash = vapply(code, fastdigest, character(1)),
                  df = NULL) {
    if(is.null(df)) {
        df = provdf(outputvar = outputvar,
               outputvarhash = outputvarhash,
               inputvar = inputvar,
               inputvarhash = inputvarhash,
               agent = agent,
               code = code,
               codehash = codehash)
    }
    new("ProvStoreDF", df)
}
                  


setOldClass("data.frame")
setClass("ProvStoreDF",
         contains = "data.frame",
         validity = function(object) {
    provdfrow = provdf()
    if(!identical(names(object), names(provdfrow)))
        stop("wrong column names on provdfrow: ", paste(names(object), collapse = ","))
    stopifnot(identical(names(object), names(provdfrow)),
              identical(sapply(object, class), sapply(provdfrow, class)))
    TRUE
})



#' ObjectClasses
#' @description these object classes are used by the RCacheSuite package. most should not be created or interacted with manually.
#' @docType methods
#' @exportClass WithVisValue
#' @rdname objclasses 
setClass("WithVisValue", representation(value = "ANY", visible = "logical"))
#' @exportClass PlotList
#' @rdname objclasses 
setClass("PlotList", contains = "list")
#' @exportClass WithVisPlusGraphics
#' @rdname objclasses 
setClass("WithVisPlusGraphics", representation(graphics = "PlotList"), contains = "WithVisValue")

virtuniversal = setRefClass("RCSUniversalFields",
                            fields = list(
                                gdevs = "list",
                                .write_allowed = "logical",
                                ## ugh, hacky, CacheDataSets don't have children...
                                ## same for the cache permissions stuff...
                                ## XXX
                                ## TODO:FIXME
                                children = "list",
                                write_allowed = function(value) {
                                if(missing(value))
                                    .write_allowed
                                else {
                                    if(is.na(value)) {
                                        ##                    warning("NULL and NAs not allowed for write_allowed field of CachingEngine objects, interpreting as TRUE")
                                        value = TRUE
                                    }
                                    .write_allowed <<- value
                                    if(!.write_allowed)
                                        sapply(children, function(x) x$write_allowed = FALSE)
                                }
                            },      
                            .write_on_cache = "logical",
                            write_on_cache = function(value) {
                                if(missing(value))
                                    .write_on_cache
                                else {
                                    if(!length(value) || is.na(value)) {
                                        ##    warning("NAs not allowed for write_on_cache field of CachingEngine objects, interpeting as FALSE")
                                        value = FALSE
                                    }
                                    .write_on_cache <<- value
                                    sync_permissions(.self)
                                    value
                                }
                            },
                            provstore = "ProvStoreDF"))



#' @exportClass CachingEngine
#' @rdname objclasses 
cacheClass = setRefClass("CachingEngine",
                         contains = "RCSUniversalFields",
    fields = list(
        base_dir = "character",
        tmp_base_dir = "character",
        .eval_fun = "function",
        eval_fun = function(value)
        {
            if(missing(value))
                .eval_fun
            else {
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
            else {
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
            for(d in dirs) {
                fils = list.dirs(d)
                fils = fils[grepl("code_", fils)]
                if(!is.null(hashes))
                    fils = fils[grepl(paste0("(", paste(hashes, collapse = "|"), ")"), fils)]
                
                newcachesets = lapply(fils, readCodeCache, load_data = load_data, write_allowed = write_allowed)
                if(is(newcachesets, "list"))
                    newcachesets = newcachesets[!sapply(newcachesets, is.null)]
                
                newhashes = sapply(newcachesets, function(x) x$hash)
                if(!refresh) {
                    ##only read hashes we don't already have caches for in memory (in this cache)
                    new = which(!(newhashes %in% sapply(children, function(x) x$hash)))
                    newcachesets = newcachesets[new]
                    newhashes = newhashes[new]
                }
                if(length(newhashes))
                    children[newhashes] <<-  newcachesets
            }
            ##XXX this is going to be wrong when there is already data in the engine
            ## FIXME
            if(file.exists(file.path(.self$base_dir, "allprov.csv")))
                provstore <<- new("ProvStoreDF", read.csv(file.path(.self$base_dir, "allprov.csv")))
        },
        add_set = function(newset)
        {
            if(!is(newset, "CodeCacheSet"))
                stop("children of CachingEngine object must be CodeCacheSet objects")
            if(!write_allowed)
                newset$write_allowed <- FALSE
            else if (!length(newset$write_allowed))
                newset$write_allowed = write_allowed
            children[[newset$hash]] <<- newset
            provstore <<- ProvStoreDF(df = rbind(provstore, newset$provstore))
            if(.self$write_on_cache)
                children[[newset$hash]]$to_disk()
 
            newset
        },
        add_data = function(newdat)
        {
            chash = newdat$code_hash
            cset = .self$get_or_create_set(code = newdat$code,
                                           inputs = newdat$inputs,
                                           outputs = newdat$outputs)
            if(!write_allowed)
                newdat$write_allowed = FALSE
            else
                newdat$write_allowed = cset$write_allowed
            
            cset$add_data(newdat)
            provstore <<- ProvStoreDF(df = rbind(provstore, newdat$provstore))
            ## if(chash %in% names(children)) {
            ##     ##caches and code-level cache sets can be more permissive but not less permissive than the CachingEngine
            ##     if(!write_allowed)
            ##         newdat$write_allowed = FALSE
            ##     else
            ##         newdat$write_allowed = children[[chash]]$write_allowed
            ##     children[[chash]]$add_data(newdat)
            ##     provstore <<- ProvStoreDF(df = rbind(provstore, newdat$provstore))
            ##     ##if we are adding an entirely new set, this is taken
            ##     ##care of by that. ugly I know but we don't want to
            ##     ##write to disk twice....
            ##     if(.self$write_on_cache)
            ##         children[[chash]][[newdata$inputs_hash]]$to_disk()
            ## } else {
            ##     newset = new("CodeCacheSet", hash = chash, cache_dir = file.path(.self$base_dir, sprintf("code_%s", chash)), write_allowed = write_allowed)
            ##     newset$add_data(newdat)
            ##     provstore <<- ProvStoreDF(df = rbind(provstore, newset$provstore))
            ##     .self$add_set(newset)
            ## }
            
            children[[chash]]
        },
        get_or_create_set = function(code, inputs = NULL, outputs = NULL)
        {
            if(is.null(inputs)|| is.null(outputs)) {
                ##           code2 = paste("{", code, "}", collapse="\n")
                ##hack to get CodeDepends to treat the code as a single block...
                if(is.character(code))
                    scr = readScript("", type="R", txt=code)
                else
                    scr = code
                codeInfo = getInputs(scr)[[1]]
                
                inputs = codeInfo@inputs
                outputs = codeInfo@outputs
            }

            ## if code is not a character, then it is assumed to be an already
            ## parsed expression, either of class "expression" or "call", "if",
            ## "=", etc from readScript. get_parsed_code handles abstracts
            ## across all of these.
            pcode = get_parsed_code(code)
            
            if(!identical(pcode, code))
                warning("unparse(parse(code)) and code are not identical. This could potentially cause a hash mismatch. Perhaps unparsed code was passed into get_or_create_set (this should not happen)?")
            chash = fastdigest(pcode)
            if(chash %in% names(children))
                children[[chash]]
            else {
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
        initialize = function(provstore = ProvStoreDF(), ..., populate = FALSE)
        {
            res = callSuper(provstore = provstore, ...)
            if(populate)
                .self$populate()
            res
        },
        find_data = function(code_hash, inputs_hash, check_disk = FALSE, extra_dirs = NULL)
        {
            if(code_hash %in% names(.self$children))
                .self$children[[code_hash]]$find_data(inputs_hash, check_disk, extra_dirs)
        else
            NA
        },
        get_location = function(tmp)
        {
            if(tmp)
                tmp_base_dir
            else
                base_dir
        },
        to_disk = function(clear_mem = TRUE, tmp=FALSE)
        {
            if(!write_allowed)
                stop("Attempted to write to disk on a CachingEngine which has writing disabled (writing_allowed is FALSE)")
            dir = .self$get_location(tmp)

            ## the child methods ensure the base directory exists.
            ## XXX should that be the caching-engine's job?
            res = sapply(children, function(x, clr) x$to_disk(clear_mem= clr), clr = clear_mem)
            invisible(write.csv(provstore, file = file.path(dir, "allprov.csv", ),
                                row.names = FALSE))
        }))
    

#' @title CachingEngine Constructor
#' @description Function for creating a Caching engine with specific
#'     behaviors
#' @param base_dir character value . Path to which permanent caches
#'     should be written (if applicable)
#' @param tmp_base_dir character value. Path to which temporary caches
#'     should be written (if applicable)
#' @param write_allowed logical. Should the engine allow caches to be
#'     written to disk. Defaults to \code{TRUE}
#' @param write_on_cache logical. Should the engine write caches to
#'     disk immediately upon creation. Defaults to \code{FALSE}
#' @param gdevs list. A list of graphics device functions to use when
#'     caching plots. Defaults to \code{list(png)}
#' @param eval_fun function. The evaluator function to be used when no
#'     cache is found. Defaults to \code{parseWithVis}.
#' @param return_handler function. The function to process values
#'     returned by \code{eval_fun}. This function will be called on
#'     values returned from \code{eval_fun} directly and those loaded
#'     from cache. Defaults to \code{withVisHandler} which
#'     supports plot caching.
#' @param populate logical. Should the engine include existing caches
#'     from \code{base_dir} upon creation. Defaults to \code{TRUE}
#' @return A CachingEngine object suitable for use with
#'     \code{evalWithCache}
#' @seealso \code{\link{evalWithCache}}
#' @author Gabriel Becker
#' @export
cachingEngine = function(base_dir="./r_caches",
                         tmp_base_dir = tempdir(),
                         write_allowed = TRUE, write_on_cache = FALSE,
                         gdevs = list(png), eval_fun = parseWithVis,
                         return_handler = withVisHandler,
                         populate = TRUE)
    {
        engine = cacheClass(base_dir = base_dir,
                            tmp_base_dir = tmp_base_dir,
                            write_allowed = write_allowed,
                            write_on_cache = write_on_cache,
                            gdevs = gdevs, eval_fun = eval_fun,
                            return_handler = return_handler,
                            populate = populate)
        engine
    }
                                
virtcodeinfo = setRefClass("CodeInfoFields",
                           contains = "RCSUniversalFields",
                           fields = list(
                               .code = "character",
                               code = function(value) {
                               if(missing(value))
                                   .code
                               else
                                   .code <<- value
                           },
                           .inputs = "character",
                           inputs = function(value) {
                               if(missing(value))
                                   .inputs
                               else
                                   .inputs <<- value
                           },
                           .outputs = "character",
                           outputs = function(value) {
                               if(missing(value))
                                   .outputs
                               else
                                   .outputs <<- value
                           }))
                           
                               





#' @exportClass CodeCacheSet
#' @rdname objclasses 
codeCacheSet = setRefClass("CodeCacheSet",
                           contains = "CodeInfoFields",
                           fields = list(
        .hash = "character",
        hash = function(value)
        {
            if(missing(value))
                .hash
            else
                .hash <<- value
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
        }),
        methods = list(
            find_data= function(hash, check_disk = TRUE, extra_dirs = NULL)
        {
            if(check_disk)
                .self$populate(refresh_existing = FALSE)
            if(hash %in% names(.self$children))
                .self$children[[hash]]
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
            sapply(.self$children, function(x) x$to_disk(location = dir, clear_mem = clear_mem))
            cat(paste("##################################################",
                      "## This file was automatically generated for provenance purposes ##",
                      "## Do Not Edit ##",
                      sprintf("## Hash: %s ##", .self$hash),
                      sprintf("## Input Variables: %s ##", paste(inputs, collapse = ", ")),
                      sprintf("## Output Variables: %s ##", paste(outputs, collapse = ", ")),
                      
                      "##################################################",
                      paste(.self$code, collapse="\n"), sep="\n"),
                file = file.path(dir, "code.R"))
            invisible(write.csv(provstore, file = file.path(dir, "allprov.csv"),
                                row.names = FALSE))
        },
        add_data = function(dat)
        {
            sync_permissions(.self, dat)
            children[[dat$inputs_hash]] <<- dat
            provstore <<- ProvStoreDF(df = rbind(provstore, dat$provstore))
            if(.self$write_on_cache)
                children[[dat$inputs_hash]]$to_disk(clear_mem = FALSE)
            dat
        },
        mem_size = function()
        {
            sum(sapply(.self$children, function(x) x$mem_size()))
        },
        initialize = function(provstore = ProvStoreDF(), ..., populate = TRUE)
        {
            res = callSuper(provstore = provstore, ...)
            if(populate)
                .self$populate()
            sync_permissions(.self)
            res
        },
        load_in_mem = function(hashes = names(.self$children))
        {
            sapply(children[hashes], function(x) x$from_disk())
        },
        unload_mem = function(write_to_disk = TRUE)
        {
            if(write_to_disk)
                .self$to_disk(clear_mem = TRUE)
            else
                sapply(children, function(x) x$unload_mem())
        },
        #Want a way to only read in caches that aren't already in the set.
        populate = function(refresh_existing = FALSE, load_data = FALSE)
        {
     
          #  if(!file.exists(.self$cache_dir))
           #     dir.create(.self$cache_dir, recursive=TRUE)
            if(refresh_existing)
                excl = character()
            else
                excl = names(children)
            cds = readCachedData(.self$cache_dir, load_data = load_data, exclude_hashes = excl, write_allowed = .self$write_allowed)
            hshs = sapply(cds, function(x) x$inputs_hash)
            lapply(cds, function(x) .self$add_data(x))
            #permissions are synced in add_data
            children
        }
        
    )
)



#' @exportClass CachedData
#' @rdname objclasses 

cachedData = setRefClass("CachedData",
                         contains = "CodeInfoFields",
                         fields = list(
                             code_hash = "character",
                             inputs_hash = "character",
                             disk_location = "character",
                             tmp_disk_location = "character",
                             .data = "environment",
                             last_used = "ANY",
                             plot = "ANY",
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
                             unload_mem = function() rm(list = ls(envir = .data),
                                                        envir = .data),
                             to_disk = function(tmp = FALSE, clear_mem = TRUE,
                                                force = FALSE, location)
                             {
                                 if(!write_allowed)
                                     stop("Attempted to write cache to disk via CachedData with writing disallowed")
                                 ##check if there is something to write, otherwise immediately return
                                 if(isFALSE(file_stale) &&
                                    !force &&
                                    (location == disk_location ||
                                     (tmp && location == tmp_disk_location)))
                                     return()
                                 
                                 if(!missing(location)) {
                                     if(tmp) {
                                         ##if(length(tmp_disk_location) && !identical(tmp_disk_location, location))
                                         if(!identical(tmp_disk_location, location))
                                             warning(sprintf("Modifying tmp disk location of existing cache for hash %s from %s to %s", inputs_hash, tmp_disk_location, location))
                                         .self$tmp_disk_location = location
                                     } else {
                                         ##if(length(disk_location) && !identical(disk_location, location))
                                         if(!identical(disk_location, location))
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
                                 filname = file.path(location, paste0(paste("cache",inputs_hash, sep="_"), ".rda"))
                                 save(list=nams, envir=.data, file = filname)
                                 if(length(.data$xxx_graphics)) {
                                     mapply(function(ext, f) {
                                         for(i in seq(along = .data$xxx_graphics)) {
                                             f(file.path(location, paste0("plot_",i, "_", inputs_hash, ".", ext)))
                                             redrawPlot(.data$xxx_graphics[[i]])
                                             dev.off()
                                         }
                                     }, names(gdevs), gdevs)
                                 }

                                 ##write data-specific csv prov store
                                 write.csv(provstore,
                                           file = file.path(location,
                                                            paste0(inputs_hash,
                                                                   "_prov.csv")
                                                            ),
                                           row.names = FALSE
                                           )
                                 
                                 if(clear_mem)
                                     ##                rm(list = nams, envir = .data)
                                     .self$unload_mem()
                                 nams
                             },
                             from_disk = function(location, hsh)
                             {
                                 if(missing(location)) {
                                     if(length(disk_location))
                                         location = disk_location
                                     else if (length(tmp_disk_location))
                                         location = tmp_disk_location
                                     else
                                         stop("Unable to determine cache location on disk (tmp or permanent).")
                                 }
                                 if(missing(hsh) && length(inputs_hash))
                                     hsh = inputs_hash
                                 else if (length(inputs_hash)) {
                                     warning("modifying hash associated with an existing cache is dangerous and is likely to invalidate the stored data.")
                                     inputs_hash <<- hsh
                                 } else if (missing(hsh)) {
                                     stop("Attempted to call from_disk with no hash specified or associated with the Cache object")
                                 }
                                 load(file = file.path(location,
                                                       paste0("cache_", hsh, ".rda")),
                                      envir = .data)
                             })
                         )



sync_permissions = function(from, to = from$children, full_sync = FALSE)
{
    if(!is(to, "list"))
        to = list(to)
    if(full_sync|| isFALSE(from$write_allowed))
        sapply(to, function(x) x$write_allowed = from$write_allowed)
    if(full_sync|| isTRUE(from$write_on_cache))
        sapply(to, function(x) x$write_on_cache= from$write_on_cache)
}

                    
