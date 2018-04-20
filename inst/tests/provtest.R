library(RCacheSuite)


code = readLines(system.file("tests/provscript.R", package = "RCacheSuite"))

mycache = cachingEngine(base_dir = file.path(tempdir(), "r_caches"))


print(mycache)

invisible(lapply(code, evalWithCache, cache = mycache))

prov = mycache$provstore
provgr = fullprovgraph(mycache)
RCacheSuite:::provextranodes(prov)
