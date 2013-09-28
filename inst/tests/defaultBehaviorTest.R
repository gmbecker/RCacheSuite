library(RCacheSuite)

x = 5
res = evalWithCache("y = x+6; y", verbose=TRUE)

res2 = evalWithCache("plot(1:x)", verbose=TRUE)
