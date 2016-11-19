#Copyright (c) 2016 Riccardo Francescato
a <- c(15,31,47,54,60)
b <- c(12,15,21,28,30)
cat(paste("Mean of a= ",mean(a)))
cat(paste("Mean of b= ",mean(b)))
cat(paste("Variance of a= ",var(a)))
cat(paste("Variance of b= ",var(b)))
cat(paste("Covariance= ",cov(a, b)))
cat(paste("Corr. coef.= ",cor(a, b)))