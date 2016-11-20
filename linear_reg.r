X = c(10,20,50,100,150,200)
Y = c(9.4,9.2,9.0,8.5,8.1,7.4)
alpha = .05
SigmaX =  sum( (X - mean(X) )^2 )
SigmaY = sum( (Y - mean(Y) )^2 )
CovXY = cov(X,Y)
CorXY = cor(X,Y)
Beta1 = sum((Y - mean(Y))*(X - mean(X) ))/SigmaX
Beta0 = mean(Y)-mean(X)*Beta1
Sigma2 = (SigmaY-(SigmaX*Beta1^2))/(length(X)-2)
Ystim = Y-(Beta0+Beta1*X)
Errors = Y-Ystim
SSR = sum((Ystim-mean(Y))^2)
SSE = sum(Errors^2)
SST = sum((Y-mean(Y))^2)
MSR = SSR/1
MSE = SSE/(length(X)-2)
MST = SST/(length(X)-1)
F0 = MSR/MSE
Fa = qf(alpha,df1=1,df2=(length(X)-2),lower.tail=F)

if(F0 > Fa) print(paste0(" reject H0 ", F0)) else print(paste0(" Accept H0 ", F0))
cat(paste0(" Regression  SS: ",SSR," Df: ", 1 ," MS: ",MSR, " F0: ",F0,
		"\n Error  SS: ",SSE," Df: ", (length(X)-2) ," MS: ",MSE,
		"\n Total  SS: ",SST," Df: ", (length(X)-1)))
