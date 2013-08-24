library(RCacheSuite)

myc = cacheClass(base_dir = "./")

code = readLines("testRcode1.R")

load(url("http://eeyore.ucdavis.edu/stat135/data/digitsSub.rda"))

evalWithCache(code, cache=myc) #works
