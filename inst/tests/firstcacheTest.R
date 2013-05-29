
library(RCacheSuite)
x = 5

mycache = cacheClass$new(type="both", cache_dirs= "./")

print(mycache)
evalWithCache("y=x+5", cache = mycache, verbose=TRUE)

#same as above
evalWithCache("y =          x+5", cache = mycache, verbose=TRUE)

print(mycache)
x = 6
#different than above because of new value of x
evalWithCache("y=x+5", cache = mycache, verbose=TRUE)

#different tvhan all previous due to new code
evalWithCache("y = x+6", cache = mycache, verbose=TRUE)
#same as above
evalWithCache("y     =    x + 6", cache = mycache, verbose=TRUE)
x = 7
#different due to new value of x
evalWithCache("y = x + 6", verbose=TRUE, cache=mycache)

print(mycache)
