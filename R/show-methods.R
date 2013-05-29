setMethod("show", "Cache",
          function(object)
          {
       #       browser()
              sizes = sapply(object$cache_data, function(x) x$mem_size())
                     
              #if the input is list(), sapply always returns list()!
              if(!length(sizes))
                  sizes = 0
              
              cat(sprintf("An object of class 'Cache'\n\ttype: %s\n\tdisk directories: %s\n\t%d pieces of cached data currently in memory\n\t%d pieces of cached data total\n\tTotal memory in use: %d bytes.\n\n", object$type, object$cache_dirs, sum(sizes > 0), length(object$cache_data), sum(sizes)))
          })
