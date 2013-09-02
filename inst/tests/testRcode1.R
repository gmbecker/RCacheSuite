eigs = eigen(var(train.sub[,-1]))
par(mfrow=c(1,2))
props = eigs$values/sum(eigs$values)
cumprops = cumsum(props)
plot(props, xlab = "Component",
ylab = "Proporition of Variance Preserved", type="l")
plot(cumprops, xlab = "Number of Components",
ylab = "Cumulative Proportion of Variance", type="l")
