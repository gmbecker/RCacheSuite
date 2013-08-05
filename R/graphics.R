#This is a modified version of the redraw.recordedPlot function from the pander package by Gergely Daróczi
#That package is licsensed as AGPL3
#The version in that package accepts only files which it seems to assume contain only a recorded plot in them. Furthemore if I am reading it correctly, the object stored in the rda file must be named plot ...

#We need more flexibility than that, and also would like to be able to redraw trellis/grid/ggplot/etc objects with the same function.


redrawPlot = function(recPlot)
{
    #this allows us to deal with trellis/grid/ggplot objects as well ...
    if(!is(recPlot, "recordedplot"))
        {
            res = try(print(recPlot))
            if(is(res, "error"))
                stop(res)
        } else {
            #start code from pander package
            if (getRversion() < "3.0.0") {
                for (i in 1:length(recPlot[[1]])) if ("NativeSymbolInfo" %in% 
                                                      class(recPlot[[1]][[i]][[2]][[1]])) 
                    recPlot[[1]][[i]][[2]][[1]] <- getNativeSymbolInfo(recPlot[[1]][[i]][[2]][[1]]$name)
            }
            else {
                for (i in 1:length(recPlot[[1]])) {
                    symbol <- recPlot[[1]][[i]][[2]][[1]]
                    if ("NativeSymbolInfo" %in% class(symbol)) {
                        if (!is.null(symbol$package)) 
                            name <- symbol$package[["name"]]
                        else name <- symbol$dll[["name"]]
                        pkgDLL <- getLoadedDLLs()[[name]]
                        nativeSymbol <- getNativeSymbolInfo(name = symbol$name, 
                                                            PACKAGE = pkgDLL, withRegistrationInfo = TRUE)
                        recPlot[[1]][[i]][[2]][[1]] <- nativeSymbol
                    }
                }
            }
            suppressWarnings(grDevices::replayPlot(recPlot))
            #end code from pander package
        }
}
