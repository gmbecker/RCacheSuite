
library(cacheR)
x = 5
evalWithCache("y=x+5", verbose=TRUE)

evalWithCache("y=x+5", verbose=TRUE)

evalWithCache("y = \nx+    5", verbose=TRUE)

x=6
evalWithCache("y=x+5", verbose=TRUE)
