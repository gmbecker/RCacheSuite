makeProvDF = function(invarhashes = NULL, outvarhashes = NULL, code = NULL,
                      codehash = fastdigest(code),
                      invars = names(invarhashes),
                      outvars = names(outvarhashes),
                      agent = getUser()) {
    if(length(invarhashes) == 0 && length(outvarhashes) == 0) {
        ## empty
        return(ProvStoreDF())
    }
    if(is.null(invarhashes) || is.null(outvarhashes) || is.null(code))
        stop("One or more required argument was not specified",
             "(required args: invarhashes, otvarhashes, code)")
    
    if(length(invars) != length(invarhashes) || sum(nzchar(invars)) != length(invarhashes))
        stop(sprintf(paste("Number of non-empty input variable names",
                           "(%d) does not match number of input var hashes (%d)"),
                     nzchar(invars), length(invarhashes)))
    if(length(outvars) != length(outvarhashes) ||
       sum(nzchar(outvars)) != length(outvarhashes))
        stop(sprintf(paste("Number of non-empty output variable names",
                       "(%d) does not match number of output var hashes (%d)"),
                     nzchar(outvars), length(outvarhashes)))
    if(length(invars) == 0) {
        invars = ""
        invarhashes = ""
    }
        
    nouts = length(outvars)
    ninputs = length(invars)
    outvars = rep(outvars, ninputs)
    outvarhashes = rep(outvarhashes, ninputs)
    invarhashes = rep(invarhashes, each = nouts)
    invars = rep(invars, each = nouts)
    if(is(code, "list"))
        code = sapply(code, .fixcode)
    else
        code = .fixcode(code)
    
    code = rep(code, times = nouts * ninputs)
    cat("\noutvars: ", outvars, "\ninvars: ", invars,
        "\ncode(nchar): ", code,"(", nchar(code), ")", "\ncodehash: ", codehash,
        "\nagent: ", agent, sep = " ")
    ProvStoreDF(outputvar = outvars, outputvarhash = outvarhashes,
                inputvar = invars, inputvarhash = invarhashes,
                agent = agent, code = code,
                codehash = codehash)
}

.fixcode = function(code) paste(code, collapse="\n")

##' @import igraph
##' @title Create the full (multiple values per variable) provenance
##'     graph for a cache
##' @description This generates and returns the \emph{full} provenance
##'     graph reflecting the information stored in the cache or
##'     data.frame specified. This can include the same variable
##'     multiple times if the corresponding expression is run with
##'     different inputs.
##' @param cacheobj a CachingEngine or CodeSetCache object. Ignored if
##'     \code{df} is set.
##' @param df ProvStoreDF (or appropriately formatted
##'     data.frame). Generally \code{cacheobj} should be specified and
##'     the default of \code{cacheobj$provstore} should be used.
##' @return an igraph object representing the provenance graph
##' @export
fullprovgraph = function(cacheobj, df = cacheobj$provstore) {
    
    df2 = df[df$inputvar != "", ]
    edges = cbind(paste(df2$inputvar, df2$inputvarhash, sep=":"),
                  paste(df2$outputvar, df2$outputvarhash, sep=":"))
    gr = graph_from_edgelist(edges)
    orphans = provextranodes(df)
    if(length(orphans) > 0)
        gr = gr + vertices(orphans)
    gr
        
}

provextranodes = function(df) {

    allinp = paste(inputvars(df), inputvarhashes(df), sep = ":")
    allout = paste(outputvars(df), outputvarhashes(df), sep = ":")
    outvals = allout[inputvars(df) == ""]
    invals = allinp[inputvars(df) != ""]
    outvals = outvals[!(outvals %in% invals)]
    outvals

}

setGeneric("outputvars", function(obj) standardGeneric("outputvars"))
setMethod("outputvars", "ProvStoreDF", function(obj) obj$outputvar)
setMethod("outputvars", "CachingEngine", function(obj) outputvars(obj$provstore))
setMethod("outputvars", "CodeCacheSet", function(obj) outputvars(obj$provstore))
setMethod("outputvars", "CachedData", function(obj) outputvars(obj$provstore))

setGeneric("outputvarhashes", function(obj) standardGeneric("outputvarhashes"))
setMethod("outputvarhashes", "ProvStoreDF", function(obj) obj$outputvarhash)
setMethod("outputvarhashes", "CachingEngine", function(obj) outputvarhashes(obj$provstore))
setMethod("outputvarhashes", "CodeCacheSet", function(obj) outputvarhashes(obj$provstore))
setMethod("outputvarhashes", "CachedData", function(obj) outputvarhashes(obj$provstore))


setGeneric("inputvars", function(obj) standardGeneric("inputvars"))
setMethod("inputvars", "ProvStoreDF", function(obj) obj$inputvar)
setMethod("inputvars", "CachingEngine", function(obj) inputvars(obj$provstore))
setMethod("inputvars", "CodeCacheSet", function(obj) inputvars(obj$provstore))
setMethod("inputvars", "CachedData", function(obj) inputvars(obj$provstore))

setGeneric("inputvarhashes", function(obj) standardGeneric("inputvarhashes"))
setMethod("inputvarhashes", "ProvStoreDF", function(obj) obj$inputvarhash)
setMethod("inputvarhashes", "CachingEngine", function(obj) inputvarhashes(obj$provstore))
setMethod("inputvarhashes", "CodeCacheSet", function(obj) inputvarhashes(obj$provstore))
setMethod("inputvarhashes", "CachedData", function(obj) inputvarhashes(obj$provstore))


## setMethod('[', "ProvStoreDF",
##           function(x, ...) {
##     df = `[.data.frame`(x, ...)
##     new("ProvStoreDF", df)
## })

## setMethod('[<-', "ProvStoreDF",
##           function(x, ..., value) {
##     df = `[<-.data.frame`(x, ..., value = value)
##     new("ProvStoreDF", df)
## })
