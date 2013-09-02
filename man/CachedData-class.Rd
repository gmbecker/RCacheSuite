\name{CachedData-class}
\Rdversion{1.1}
\docType{class}
\alias{CachedData-class}

\title{Class \code{"CachedData"}}
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
showClass("CachedData")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{.code_hash}:}{Object of class \code{character} ~~ }
    \item{\code{code_hash}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{.inputs_hash}:}{Object of class \code{character} ~~ }
    \item{\code{inputs_hash}:}{Object of class \code{activeBindingFunction} ~~ }
    \item{\code{disk_location}:}{Object of class \code{character} ~~ }
    \item{\code{tmp_disk_location}:}{Object of class \code{character} ~~ }
    \item{\code{.data}:}{Object of class \code{environment} ~~ }
    \item{\code{last_used}:}{Object of class \code{ANY} ~~ }
    \item{\code{plot}:}{Object of class \code{ANY} ~~ }
    \item{\code{gdevs}:}{Object of class \code{list} ~~ }
    \item{\code{file_stale}:}{Object of class \code{logical} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{from_disk(location, hsh)}:}{ ~~ }
    \item{\code{unload_mem()}:}{ ~~ }
    \item{\code{retrieve_data(env)}:}{ ~~ }
    \item{\code{to_disk(tmp, clear_mem, force, location)}:}{ ~~ }
    \item{\code{mem_size()}:}{ ~~ }
  }
}
