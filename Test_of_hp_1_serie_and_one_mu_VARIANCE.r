#Copyright (c) 2016 Riccardo Francescato
a <- c(108,124,124,106,115,138,163,159,134,139)
#confidency level
alpha = .01 
#type of test: 0 left tail 1 double tail 2 right tail
type = 1
#calculations....
n0 = length(a)		# number of reps for sample 0
xbar0 = mean(a)		# sample 0 mean 
Sigma0 = 2			# given  variance 
s0 = sd(a)			# sample 0 standard deviation 
X0 = ((n0-1)*s0^2)/Sigma0
if(type == 0){
	x.alpha = qchisq(1−alpha, df=n0−1) 
	cat("Results of lower tail\n")
	cat("Sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat(paste("\nx0: ",X0))
	cat(paste("\nxalpha: ",x.alpha))
	if(X0<x.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")
}else if(type == 1){
	x.top.alpha = qchisq(alpha/2, df=n0−1) 
	x.bottom.alpha = qchisq(1-alpha/2, df=n0−1) 
	cat("Results of 2-tail tail\n")
	cat("Sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat(paste("\nx0: ",X0))
	cat(paste("\nxalpha +: ",x.top.alpha))
	cat(paste("\nxalpha -: ",x.bottom.alpha))
	if(X0<x.bottom.alpha || X0>x.top.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")

}else if(type == 2){
	x.alpha = qchisq(alpha, df=n0−1) 
	cat("Results of upper tail\n")
	cat("Sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat(paste("\nx0: ",X0))
	cat(paste("\nxalpha: ",x.alpha))
	if(X0>x.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")
}
