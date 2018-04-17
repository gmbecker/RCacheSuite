setMethod("show", "CachingEngine",
          function(object)
          {
       #       browser()
              sizes = unlist(sapply(object$children, function(lst) sapply(lst$children, function(x) x$mem_size())))
                     
              #if the input is list(), sapply always returns list()!
              if(!length(sizes))
                  sizes = 0
              lens = sapply(object$children, function(lst) length(lst$children))
              if(!length(lens))
                  lens = 0
              totcaches = sum(lens)
              formstring = paste(c("An object of class 'CachingEngine'",
                                  "\tbase directory: %s",
                                  "\t%d code-level cache sets",
                                  "\t%d output level caches in memory",
                                  "\t%d total output level caches",
                                  "\tTotal memory in use: %d bytes.\n"),
                                collapse="\n")
              cat(sprintf(formstring, 
                          object$base_dir,
                          length(object$children),
                          sum(sizes > 0),
                          totcaches,
                          sum(sizes)))
          })
