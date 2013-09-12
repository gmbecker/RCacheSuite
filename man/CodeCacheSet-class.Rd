\name{CodeCacheSet-class}
\Rdversion{1.1}
\docType{class}
\alias{CodeCacheSet-class}
\title{Class \code{"CodeCacheSet"}}
\description{
  A set of individual caches associated with the same code evaluated
  within different states.

  This class is primarily for internal use. End users are intended to
  use the \code{CachingEngine} or \code{evalWithCache}-level interface.
}
\author{
  Gabriel Becker
}
\seealso{
\code{\linkS4class{CachedData}} \code{linkS4class{CachingEngine}}
}
\examples{
showClass("CodeCacheSet")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{hash}:}{The hash representing the code associated with
  this set of caches.}
    \item{\code{code}:}{A copy of the code associated with this
  set of caches.}
    \item{\code{cache_data}:}{A list of the \code{CachedData} objects
  contained in this set of caches}
    \item{\code{tmp_cache_dir}:}{The temporary directory associated with
  this set of caches}
     \item{\code{cache_dir}:}{The permanent directory associated with
  this set of caches.}
    \item{\code{inputs}:}{A character vector containing the names of
  input variables associated with \code{code}. The values of these
  variables at the time of cached evaluation make up the state used to
  differentiate between caches.}
    \item{\code{outputs}:}{A character vector containing the names of
  output variables associated with \code{code}. These variables are
  included in the results tracked by caches in this set.}
    \item{\code{gdevs}:}{A list of graphics devices to use to save
      static images of the results of any detected plotting during the
  evaluation of \code{code} when the caches is written to disk }
    \item{\code{write_on_cache}:}{Whether or not caches in this set can
  be written to disk. }
  }
}
\section{Methods}{
  \describe{
    \item{\code{load_in_mem(hashes)}:}{ Load the results stored in the
  caches associated with \code{hashes} into memory. }
    \item{\code{unload_mem(write_to_disk)}:}{ Unload the contents of all
  caches in this set from memory. If \code{write_to_disk} is
  \code{TRUE}, write the contents of the caches to disk before unloading.}
    \item{\code{populate(refresh_existing, load_data)}:}{ Populate this
  set with caches found in \code{cache_dir}. if \code{refresh_existing}
  is \code{FALSE}, only populate caches which do not match those already
  in the set. if \code{load_data}, load the results in the caches into
  memory as they are created.}
    \item{\code{add_data(dat)}:}{ Add a cache to the set. }
    \item{\code{find_data(hash, check_disk, extra_dirs)}:}{ Find a cache
  associated with \code{code} and a a state-level hash of
  \code{hash}. If \code{check_disk} is \code{TRUE}, look both in
  \code{cache_data} and on disk, otherwise look only in \code{cache_data}.}
    \item{\code{to_disk(dir, clear_mem)}:}{ Write all caches in the set
  to disk in the \code{dir} directory. If \code{clear_mem} is
  \code{TRUE}, unload the results of each cache from memory after
  writing them disk. }
    \item{\code{mem_size()}:}{  Returns the total size in memory of all
  caches in this set, as identified by each cache's \code{mem_size} method.
     }
  }
}
