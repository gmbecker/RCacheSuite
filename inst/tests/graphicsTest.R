library(RCacheSuite)
x = 5

mycache = cacheClass$new( base_dir= "./")

evalWithCache("plot(1:x)", cache = mycache, verbose = TRUE)

evalWithCache("plot(1:x)", cache = mycache, verbose = TRUE)

x = 6

evalWithCache("plot(1:x)", cache = mycache, verbose = TRUE)

evalWithCache("plot(2:x)", cache = mycache, verbose = TRUE)

mycache

mycache$to_disk()

calls = thing[[1]]

for(i in 1:length(thing[[1]])) {
  if( "NativeSymbolInfo" %in% class(thing[[1]][[i]][[2]][[1]]) ){
    thing[[1]][[i]][[2]][[1]] <- getNativeSymbolInfo(thing[[1]][[i]][[2]][[1]]$name);
  }
} 


        
