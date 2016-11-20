#data from example page 144
a = 4
b = 6
N = a*b 
alpha = .05
RawData= c(90.3,89.2, 98.2, 93.9, 87.4, 97.9,92.5, 89.5, 90.6, 94.7, 87.0, 95.8,85.5, 90.8, 89.6, 86.2, 88.0, 93.4,82.5, 89.5, 85.6, 87.4, 78.9, 90.7)
data = matrix(RawData,# the data elements 
	nrow=a,              # number of rows 
	ncol=b,              # number of columns 
	byrow = TRUE)        # fill matrix by rows

SStot = sum(data^2) - sum(data)^2/N
SStreat = 0
for(i in 1:a){
	SStreat = SStreat + sum(data[i,])^2
}
SStreat = SStreat *(1/b) - sum(data)^2/N
SSblock = 0
for(i in 1:b){
	SSblock = SSblock + sum(data[,i])^2
}
SSblock = SSblock *(1/a) - sum(data)^2/N
SSerr = SStot - SStreat - SSblock
MStreat = SStreat/(a-1)
MSblock = SSblock/(b-1)
MSerr = SSerr/((a-1)*(b-1))

F0 <- MStreat/MSerr
Fa <- qf(alpha,df1=a-1,df2=((a-1)*(b-1)),lower.tail=F)

if(F0 > Fa) print(paste0(" reject H0 ", F0)) else print(paste0(" Accept H0 ", F0))
cat(paste0(" Treatments  SS: ",SStreat," Df: ", a-1 ," MS: ",MStreat, " F0: ",F0,
		"\n Blocks  SS: ",SSblock," Df: ", b-1 ," MS: ",MSblock,
		"\n Error  SS: ",SSerr," Df: ", (a-1)*(b-1) ," MS: ",MSerr,
		"\n Total  SS: ",SStot," Df: ", (a*b)-1))



delivery.df = data.frame(
  Treatment = c(rep("1", col), rep("2", col), rep("3", col), rep("4", col)),
  Block = c(rep(c("1", "2", "3", "4","5","6"), row)),
  Data = RawData
)
delivery.mod1 = lm(Data ~ Treatment+Block, data = delivery.df)
anova(delivery.mod1)