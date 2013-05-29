
library(RCacheSuite)
x = 5
#evalWithCache("y=x+5", verbose=TRUE)

#evalWithCache("y=x+5", verbose=TRUE)

#evalWithCache("y = \nx+    5", verbose=TRUE)

x=6
evalWithCache("y=x+5", verbose=TRUE)

mycache = cacheClass$new(type="both", cache_dirs= "./")

evalWithCache("y=x+5", cache = mycache, verbose=TRUE)

evalWithCache("y = x+6", cache = mycache, verbose=TRUE)

evalWithCache("y     =    x + 6", cache = mycache, verbose=TRUE)
x = 7
evalWithCache("y = x + 6", verbose=TRUE, cache=mycache)

