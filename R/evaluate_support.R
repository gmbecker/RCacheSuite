#' @import evaluate



## probably overkill but this is the most robust way I can think of to
## be absolutely sure that the value handler is *always and only* hit when
## for the return value, because evaluate doesn't class or specially
## mark it and it can in principle be any class. It also returns
## unclassed character vectors for captured text 


setClass("ev_output", list(value = "ANY"))
setClass("ev_source", contains = "ev_output")
setClass("ev_message", contains = "ev_output")
setClass("ev_text", contains = "ev_output")
setClass("ev_graphics", contains = "ev_output")
setClass("ev_warning", contains = "ev_output")
setClass("ev_error", contains = "ev_output")
setClass("ev_value", contains = "ev_output",
         list(visible = "logical"))



make_evhandler = function(source = NULL,
                          text = NULL,
                          graphics = NULL,
                          message = NULL,
                          warning = NULL,
                          error = NULL,
                          value = NULL
                          ) {
    x = list()
    count = 1
    .evfunmaker = function(clname, extrafun = NULL, clear = FALSE) {
        function(val, visible) {
            if(!is.null(extrafun))
                extrafun(val)
            if(clear) {
                x <<- list()
                count <<- 1
            }
            if(clname == "ev_value")
                x[[count]] <<- new(clname, value = val,
                                   visible = visible)
            else
                 x[[count]] <<- new(clname, value = val)
            count <<- count + 1
        }
    }
    
    new_output_handler(
        source = .evfunmaker("ev_source", source, clear = TRUE),
        text = .evfunmaker("ev_text", text),
        graphics = .evfunmaker("ev_graphics", graphics),
        message = .evfunmaker("ev_message", message),
        warning = .evfunmaker("ev_warning", warning),
        error = .evfunmaker("ev_error", error),
        value = function(val, visible) {.evfunmaker("ev_value", value)(val, visible); x}
    )

    
}

#' @title Make an 'evaluate'-based evaluation function
#' @description Create an evaluation function appropiate for use with
#'     RCacheSuite that uses Wickham's evaluate function as the
#'     underlying mechanism
#' @param source An additional source handler to call before the one
#'     we use internally, or NULL (default)
#' @param text An additional text handler to call before the one we
#'     use internally, or NULL (default)
#' @param graphics An additional graphics handler to call before the
#'     one we use internally, or NULL (default)
#' @param message An additional message handler to call before the one
#'     we use internally, or NULL (default)
#' @param warning An additional warning handler to call before the one
#'     we use internally, or NULL (default)
#' @param error An additional error handler to call before the one we
#'     use internally, or NULL (default)
#' @param value An additional value handler to call before the one we
#'     use internally, or NULL (default)
#' @return A function which can be used as an evaluator with the
#'     RCacheSuite machinery
#' @export
make_evaluate_ef = function(source = NULL,
                          text = NULL,
                          graphics = NULL,
                          message = NULL,
                          warning = NULL,
                          error = NULL,
                          value = NULL
                          ) {

    evhandler = make_evhandler(source, text, graphics, message, warning, error, value)
    function(code, env, evaled = FALSE, last = FALSE, stop_on_error = 1, ...) {
       res =  evaluate(code, env, stop_on_error = stop_on_error,
                       output_handler = evhandler, ...)
       ## the last element is the value, which with this
       ## handler is the full list of captured elements
       res[[length(res)]]
    }
}
#' @export
#' @rdname evalhandlers
evaluate_ef = make_evaluate_ef()

setGeneric("handle_ev_out", function(outobj) standardGeneric("handle_ev_out"))
## handling source 'output' is a no-op for now. Could 'echo' it to
## console if we wanted to...
setMethod("handle_ev_out", "ev_source",
          function(outobj) NULL)
## for some reason the source output handler isn't working here.
setOldClass("source")
setMethod("handle_ev_out", "source",
          function(outobj) NULL)

setMethod("handle_ev_out", "ev_message",
          function(outobj) message(outobj@value))
setMethod("handle_ev_out", "ev_warning",
          function(outobj) warning(outobj@value))
setMethod("handle_ev_out", "ev_error",
          function(outobj) warning(outobj@value))
setMethod("handle_ev_out", "ev_text",
          function(outobj) cat(outobj@value))
setMethod("handle_ev_out", "ev_graphics",
          function(outobj) redrawPlot(outobj@value))
setMethod("handle_ev_out", "ev_value",
          function(outobj) outobj@value)

#' @export
#' @rdname evalhandlers
evaluate_rh = function(val, graphics, env, evaled = FALSE, ...) {
    ## we ignore the graphics argument because that is handled
    ## differently by evaluate and we just piggyback that.
    ##
    ## no need to worry about recreating assignments here
    ## evalWithCache takes care of that regardless of
    ## handler
    ##
    ## the value will always be the last one
    ## that's what we want to return, but recreate all
    ## the captured side-effects firsti
    lapply(head(val, -1), handle_ev_out)
    handle_ev_out(tail(val, 1)[[1]])
}
