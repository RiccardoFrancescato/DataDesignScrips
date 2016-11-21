#Copyright (c) 2016 Riccardo Francescato
a <- c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
b <- c(16.62,16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)
#confidency level
alpha = .05 
#type of test: 0 left tail 1 double tail 2 right tail
type = 2
#calculations....
n0 = length(a)		# number of reps for sample 0
n1 = length(b)		# number of reps for sample 1
xbar0 = mean(a)		# sample 0 mean 
xbar1 = mean(b)		# sample 1 mean 
s0 = sd(a)			# sample 0 standard deviation 
s1 = sd(b)			# sample 1 standard deviation 
f = (s0^2)/(s1^2)
if(type == 0){
	fc = qf(alpha,df1=n0-1,df2=n1-1,lower.tail=F)
	cat("Results of lower tail\n")
	cat("First sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat("\nSecond sample\n")
	cat(paste("# of elements: ", n1, " Mean: ", xbar1, " St Dev: ",s1))
	cat(paste("\nf: ",f))
	cat(paste("\nfalpha: ",fc))
	if(f>fc) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")
}else if(type == 1){
	fct = qf(alpha/2,df1=n0-1,df2=n1-1,lower.tail=F)
	fcb = qf(1-alpha/2,df1=n0-1,df2=n1-1,lower.tail=F)
	cat("Results of 2-tail tail\n")
	cat("First sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat("\nSecond sample\n")
	cat(paste("# of elements: ", n1, " Mean: ", xbar1, " St Dev: ",s1))
	cat(paste("\nf0: ",f))
	cat(paste("\nfalpha +: ",fct))
	cat(paste("\nfalpha -: ",fcb))
	if(f<fcb || f>fct) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")

}else if(type == 2){
	fc = qf(alpha,df1=n0-1,df2=n1-1,lower.tail=F)
	cat("Results of lower tail\n")
	cat("First sample\n")
	cat(paste("# of elements: ", n0, " Mean: ", xbar0, " St Dev: ",s0))
	cat("\nSecond sample\n")
	cat(paste("# of elements: ", n1, " Mean: ", xbar1, " St Dev: ",s1))
	cat(paste("\nf: ",f))
	cat(paste("\nfalpha: ",fc))
	if(f>fc) cat("\nREJECT H0\n") else cat("\nACCEPT H0\n")
}
