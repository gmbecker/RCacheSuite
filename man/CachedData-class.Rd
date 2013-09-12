\name{CachedData-class}
\Rdversion{1.1}
\docType{class}
\alias{CachedData-class}

\title{Class \code{"CachedData"}}
\description{
  An R object class representing the cached evaluation results of a single
  code/state combination. Individual sets of cached results stored
  within a \code{CodeLevelCache} are represented as objects of this
  class.

  This class is primarily for internal use. End users are intended to
  use the \code{CachingEngine} or \code{evalWithCache}-level interface.
  }
\author{
  Gabriel Becker
}
\seealso{
\code{\linkS4class{CodeCacheSet}} \code{linkS4class{CachingEngine}}
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{code_hash}:}{The hash representing the code associated
      with this cache. }
    \item{\code{inputs_hash}:}{The hash representing the state
      (values of input variables) of this cache. }
    \item{\code{disk_location}:}{Location on disk that this cache object
    with read from and write to.}
    \item{\code{tmp_disk_location}:}{Temporary disk location this cache
      object will read from and write to }
    \item{\code{.data}:}{An environment which stores the caches results
      when the cache is loaded into memory }
    \item{\code{last_used}:}{A representation of the last time the
      results in the cache were used during a cached evaluation. }
    \item{\code{plot}:}{Any plotting results associated with the cached results}
    \item{\code{gdevs}:}{A list of graphics devices to use to save
      static images of the contents of \code{plot} when the cache is
      written to disk}
    \item{\code{file_stale}:}{Whether the contents of the cache have
      changed since the last time it was written to disk. }
  }
}
\section{Methods}{
  These methods are exported and documented but not intended for direct
  use. They are typically called automatically via methods on the
  \code{CachingEngine} and \code{CodeLevelCache} classes.
  \describe{
    \item{\code{from_disk(location, hsh)}:}{ Write the contents of this
      cache to disk. }
    \item{\code{unload_mem()}:}{ Unload the R objects representing the
      results stored in the cache from memory. This does not destroy the
    \code{CachingEngine} object itself, it simply empties the
    \code{.data} evironment.}
    \item{\code{retrieve_data(env)}:}{ Populate the \code{env}
      environment with the results stored in the cache, as if the
      original code had been evaluated in that environment.}
    \item{\code{to_disk(tmp, clear_mem, force, location)}:}{ Write the
      contents of this cache to disk. \code{tmp} indicates that the
      temporary directory sould be used, \code{clear_mem} indicates that
    the cache should be unloaded from memory after writing, and
    \code{location} indicates the location on disk to write.}
    \item{\code{mem_size()}:}{ Returns the size of the objects
      associated with this cache that are currently being stored in
      memory. \code{0} indicates that the cache is searchable but no
      results are currently in memory.}
  }
}
