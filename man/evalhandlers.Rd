% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache.R, R/evaluate_support.R
\name{parseEval}
\alias{parseEval}
\alias{parseWithVis}
\alias{withVisHandler}
\alias{withVisRaw}
\alias{returnRaw}
\alias{noGraphicsRaw}
\alias{evaluate_ef}
\alias{evaluate_rh}
\title{Evaluation functions and return value handlers}
\usage{
parseEval(code, env, ...)

parseWithVis(code, env, ...)

withVisHandler(val, graphics, env, evaled = FALSE, last = FALSE, ...)

withVisRaw(val, graphics, env, evaled = FALSE, last = FALSE, ...)

returnRaw(val, graphics, env, evaled = FALSE, ...)

noGraphicsRaw(val, graphics, env, evaled = FALSE, ...)

evaluate_ef(code, env, evaled = FALSE, last = FALSE, stop_on_error = 1,
  ...)

evaluate_rh(val, graphics, env, evaled = FALSE, ...)
}
\arguments{
\item{code}{The code to evaluate}

\item{env}{The evaluation environment/environment the return handler should
populate}

\item{...}{Generally unused.}

\item{val}{ANY. The object returned from the evaluation function}

\item{graphics}{PlotList. The graphics captured during evaluation}

\item{evaled}{Logical. Indicates whether evaluation occured (it is passed
\code{FALSE} when values are loaded from cache).}

\item{last}{Logical. Is the value being processed the output of the final
expression in the code being handled.}

\item{stop_on_error}{integer. Passed to evaluate.}
}
\value{
Evaluation functions should return the result of evaluating
    the code, along with information sufficient to recreate any
    non-plotting side-effects the user wishes to record and
    recreate on loading from cache. Return value handlers should
    recreate those side effects, and can then call \code{returnRaw}
    or \code{withVisRaw} to recreate plots and return the value
    from a raw return \code{val} or \code{WithVisValue} val object,
    respectively. \code{WithVisValue} objects are created
    automatically by the default evaluate function.
}
\description{
RCacheSuite allows users to specify custom evaluation functions,
which can be used to capture information about side effects, and return value
handlers, which can then use that information to recreate the side effects.

In practice, the behavior of the default evaluation function
(\code{parseWithVis}) and return handler (\code{withVizHandler}) should meet
users' needs the majority of the time.
}
\details{
Evaluation functions must accept \code{code}, \code{env}, and \code{...}.

Return value handlers must accept \code{val}, \code{graphics},
\code{env}, and \code{...}. They will also be passed \code{evaled}
and \code{last} when called by the caching machinery, and can
handle those specifically or catch them in \code{...}.

\code{parseEval} simply parses and evaluates the code, returning the
 value returned by the call to \code{eval}.

 \code{parseWithVis} evaluates the code using \code{withVis} and
 returns a \code{WithVisValue} object with the resulting return
 value and visibility information.

 \code{withVisHandler} accepts the \code{WithVisValue} object
 returned by \code{parseWithVis} as well as printing the final
 result if the \code{val$visible} is \code{TRUE}. It also draws any
 plots stored in the cache in the case that \code{evaled} is
 \code{FALSE} (if it were \code{TRUE} those plots would have
 already been generated during the evaluation). Appropriate for use
 with \code{parseWithVis}.

 \code{returnRaw} recreates any plots recorded in the cache (always,
    not differentiating between evaluation and cache loading) and
    returns \code{val} unmodified. Appropriate for use with \code{parseEval}

 \code{noGraphicsRaw} returns \code{val} unmodified without
 recreating any plots.
}
\author{
Gabriel Becker
}
