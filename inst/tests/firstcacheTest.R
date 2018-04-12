
library(RCacheSuite)
x = 5

mycache = cachingEngine(base_dir = file.path(tempdir(), "r_caches"))


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
#write the in-memory cached data to disk
mycache$to_disk()
x = 7
#different due to new value of x
evalWithCache("y = x + 6", verbose=TRUE, cache=mycache)

print(mycache)

#this last one didn't get written to disk, so when you quit R, start it again, and rerun the script, there won't be a cache for it, but there will for the rest of them.
