#Copyright (c) 2016 Riccardo Francescato
a <- c(108,124,124,106,115,138,163,159,134,139)
#confidency level
alpha = .01 
#type of test: 0 left tail 1 double tail 2 right tail
type = 2
#calculations....
n0 = length(a)		# number of reps for sample 0
xbar0 = mean(a)		# sample 0 mean 
mu = 120			# given  mean 
s0 = sd(a)			# sample 0 standard deviation 
t = (xbar0−mu)/(s0/sqrt(n0))
if(type == 0){
	t.alpha = qt(1−alpha, df=n0−1) 
	pval = pt(t, df=n0−1) 
	cat("Results of lower tail\n")
	cat("Sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat(paste("\nt0: ",t))
	cat(paste("\ntalpha: ",−t.alpha))
	cat(paste("\np-value: ",pval))
	if(t<−t.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")
}else if(type == 1){
	t.half.alpha = qt(1−alpha/2, df=n0−1) 
	pval = 2 * pt(t, df=n0−1)
	cat("Results of 2-tail tail\n")
	cat("Sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat(paste("\nt0: ",t))
	cat(paste("\ntalpha +: ",t.half.alpha))
	cat(paste("\ntalpha -: ",-t.half.alpha))
	cat(paste("\np-value: ",pval))
	if(t<-t.half.alpha || t>t.half.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")

}else if(type == 2){
	t.alpha = qt(1−alpha, df=n0−1) 
	pval = pt(t, df=n0−1, lower.tail=FALSE) 
	cat("Results of upper tail\n")
	cat("Sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat(paste("\nt0: ",t))
	cat(paste("\ntalpha: ",t.alpha))
	cat(paste("\np-value: ",pval))
	if(t>t.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")
}
