\name{cachingEngine}
\alias{cachingEngine}
\title{\code{CachingEngine} Constructor}
\description{
  The \code{cachingEngine} function is provided as a constructor for
  this class with reasonable default values for the fields.}
\usage{
cachingEngine(base_dir = "./r_caches", tmp_base_dir = tempdir(), 
    write_allowed = TRUE, write_on_cache = FALSE, gdevs = list(png),
eval_fun = parseWithVis, return_handler = withVisHandler, populate =
TRUE)
}
\arguments{
  \item{base_dir}{The (permanent) base directory associated with the caching engine}
  \item{tmp_base_dir}{The (temp) directory associated with the caching
    engine}
  \item{write_allowed}{Whether the created caching engine should allow
    caches to be written to disk. Defaults to \code{TRUE}
  }
  \item{write_on_cache}{Whether all caches should be immediately written
    to disk upon creation. Defaults to \code{FALSE}
  }
  \item{gdevs}{A list of graphics device functions which should be used
    to save static images of any graphics associated with a cache}
  \item{eval_fun}{The function which should be used when evaluating code
    using the created caching engine.}
  \item{return_handler}{The function which should be used to process the
    return value of \code{eval_fun}. This function will be called on the
    value returned by \code{eval_fun} both when code is evaluated and
    when results from an existing cache are loaded.}
  \item{populate}{Whether the \code{CachingEngine} object created by the
    constructor should immediately populate itself from disk with any
    caches found in \code{base_dir}. Defaults to \code{TRUE}}
}
\author{
  Gabriel Becker
}
\seealso{
  \code{\linkS4class{CachingEngine}} \code{\link{evalWithCache}}
}
