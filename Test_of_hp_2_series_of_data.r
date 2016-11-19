#Copyright (c) 2016 Riccardo Francescato
a <- c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
b <- c(16.62,16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)
#confidency level
alpha = .05 
#type of test: 0 left tail 1 double tail 2 right tail
type = 1
#calculations....
n0 = length(a)		# number of reps for sample 0
n1 = length(b)		# number of reps for sample 1
xbar0 = mean(a)		# sample 0 mean 
xbar1 = mean(b)		# sample 0 mean 
s0 = sd(a)			# sample 0 standard deviation 
s1 = sd(b)			# sample 1 standard deviation 
sp2 = ((n0-1)*s0^2 + (n1-1)*s1^2)/(n0+n1-2)
t = (xbar0−xbar1)/(sqrt(sp2)*sqrt((1/n0) + (1/n1))) 
if(type == 0){
	t.alpha = qt(1−alpha, df=n0−1) 
	pval = pt(t, df=n0−1) 
	cat("Results of lower tail\n")
	cat("First sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat("\nSecond sample\n")
	cat(paste("# of elements: ", n1, " Mean: ", xbar1, " St Dev: ",s1))
	cat(paste("\nEstimate of common variance Sp: ",sqrt(sp2)))
	cat(paste("\nt0: ",t))
	cat(paste("\ntalpha: ",−t.alpha))
	cat(paste("\np-value: ",pval))
	if(t<−t.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")
}else if(type == 1){
	t.half.alpha = qt(1−alpha/2, df=n0−1) 
	pval = 2 * pt(t, df=n0−1)
	cat("Results of 2-tail tail\n")
	cat("First sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat("\nSecond sample\n")
	cat(paste("# of elements: ", n1, " Mean: ", xbar1, " St Dev: ",s1))
	cat(paste("\nEstimate of common variance Sp: ",sqrt(sp2)))
	cat(paste("\nt0: ",t))
	cat(paste("\ntalpha +: ",t.half.alpha))
	cat(paste("\ntalpha -: ",-t.half.alpha))
	cat(paste("\np-value: ",pval))
	if(t<-t.half.alpha || t>t.half.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")

}else if(type == 2){
	t.alpha = qt(1−alpha, df=n0−1) 
	pval = pt(t, df=n0−1, lower.tail=FALSE) 
	cat("Results of upper tail\n")
	cat("First sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat("\nSecond sample\n")
	cat(paste("# of elements: ", n1, " Mean: ", xbar1, " St Dev: ",s1))
	cat(paste("\nEstimate of common variance Sp: ",sqrt(sp2)))
	cat(paste("\nt0: ",t))
	cat(paste("\ntalpha: ",t.alpha))
	cat(paste("\np-value: ",pval))
	if(t>t.alpha) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")
}
