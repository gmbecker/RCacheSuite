\name{CodeCacheSet-class}
\Rdversion{1.1}
\docType{class}
\alias{CodeCacheSet-class}

\title{Class \code{"CodeCacheSet"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("CodeCacheSet")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{.hash}:}{Object of class \code{character} ~~ }
    \item{\code{hash}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{.code}:}{Object of class \code{character} ~~ }
    \item{\code{code}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{.cache_data}:}{Object of class \code{list} ~~ }
    \item{\code{cache_data}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{.tmp_cache_dir}:}{Object of class \code{character} ~~ }
    \item{\code{tmp_cache_dir}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{.cache_dir}:}{Object of class \code{character} ~~ }
    \item{\code{cache_dir}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{.inputs}:}{Object of class \code{character} ~~ }
    \item{\code{inputs}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{.outputs}:}{Object of class \code{character} ~~ }
    \item{\code{outputs}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{gdevs}:}{Object of class \code{list} ~~ }
    \item{\code{write_on_cache}:}{Object of class \code{logical} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{load_in_mem(hashes)}:}{ ~~ }
    \item{\code{unload_mem(write_to_disk)}:}{ ~~ }
    \item{\code{populate(refresh_existing, load_data)}:}{ ~~ }
    \item{\code{add_data(dat)}:}{ ~~ }
    \item{\code{initialize(..., populate)}:}{ ~~ }
    \item{\code{find_data(hash, check_disk, extra_dirs)}:}{ ~~ }
    \item{\code{to_disk(dir, clear_mem)}:}{ ~~ }
    \item{\code{mem_size()}:}{ ~~ }
  }
}
