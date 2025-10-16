# 02. Statistical Learning

library(ISLR2)
str(Auto)

set.seed(-1)
N <- nrow(Auto)
ind.all <- 1:N
N.tr <- round(0.7*N)
ind.tr <- sample(x=ind.all, size=N.tr, replace=FALSE)
ind.te <- ind.all[-ind.tr]
D.tr <- Auto[ind.tr,]
D.te <- Auto[ind.te,]

head(ind.tr)

head(D.tr)

head(ind.te)

head(D.te)

Three.Means <- function(x)
{
  c(sample.mean = mean(x, na.rm = TRUE), 
    geometric.mean = exp(mean(log(x), na.rm = TRUE)),
    harmonic.mean = 1/mean(1/x, na.rm = TRUE))
}

Three.Means(c(1, 9))

Three.Means(c(1:9, 100))


