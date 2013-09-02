\name{parseEval}
\alias{parseEval}
\alias{parseWithVis}
\alias{withVisHandler}
\title{
  Evaluation functions and return value handlers.
}
\description{
  RCacheSuite allows developers to specify custom evaluation functions,
  as well as handlers to be called on the values returned after evaluation.
}
\usage{
parseEval(code, env, ...)
parseWithVis(code, env, ...)

withVisHandler(val, graphics, env, evaled = TRUE, ...)
}
\arguments{
  \item{code}{
    The code to be evaluated. Currently this is passed as a character string.
}
  \item{env}{
    The environment in which the code should be evaluated and the final
    results should be assigned
}
  \item{\dots}{
    Additional arguments expected by specific evaluator or handler functions
  }
  \item{val}{
    The value returned by the evaluation function. No universal
    assumptions are made about the structure or class of this object
    other than that the handler function will now how to process it.
  }
  \item{graphics}{
    A list of \code{recordedplot} objects representing the plots
    generated when evaluating the code. All handlers must accept this
    argument but it can be ignored in cases when \code{val} contains
    plot information.
    }
  \item{evaled}{
    A logical indicated whether val came from evaluating \code{code}
    (\code{TRUE}) or from loading the value returned by the evaluation
    function from a cache (\code{FALSE}). This is passed to the handler
    automatically by \code{evalWithCache}.}
}
\details{
  \code{parseEval} simply parses and evaluates the code, returning the
  value returned by the call to \code{eval}.

  \code{parseWithVis} evaluates the code using \code{withVis} and
  returns the resulting list.

  \code{withVisHandler} accepts the list returned by \code{parseWithVis}
  as well as printing the final result if the \code{val$visible} is
  \code{TRUE}. It also draws any plots stored in the cache in the case
  that \code{evaled} is \code{FALSE} (if it were \code{TRUE} those plots
  would have already been generated during the evaluation process).
}
\value{
  No restrictions are placed on the values returned by these functions,
  though it is conceptually assumed that the return value of an evaluation
  function is some representation of the results of that evaluation.
}
\author{
  Gabriel Becker
}
\seealso{
  \code{\link{evalWithCache}} \code{\linkS4class{CachingEngine}}
}
\keyword{ programming }
\keyword{ evaluation }
