#Copyright (c) 2016 Riccardo Francescato
#insert a vector for eache experiment repetition
RawData0 = c(130,34,20,
			150,136,25,
			138,174,96)
RawData1 = c(155,40,70,
			188,122,70,
			110,120,104)
RawData2 = c(74,80,82,
			159,106,58,
			168,150,82)
RawData3 = c(180,75,58,
			126,115,45,
			160,139,60)

RawData = c(RawData0,RawData1,RawData2,RawData3)

a = 3 # numer of treatments A (columns)
b = 3 # numer of treatments B (rows)
n = 4 # numer of repetitions
N = a*b*n
alpha = .05
SRawData = RawData0+RawData1+RawData2+RawData3
SRawMatrix = matrix(SRawData,ncol=a,byrow = TRUE)
RawMatrix = matrix(RawData,ncol=a,byrow = TRUE)

# ANOVA
sum(SRawMatrix[1,])

SSt <- sum(RawData^2) - sum(RawData)^2/N
SSa = 0
for (i in 1:b){
	SSa=SSa+sum(SRawMatrix[i,])^2
}
SSa=(1/(b*n))*SSa-sum(RawData)^2/N

SSb = 0
for (i in 1:a){
	SSb=SSb+sum(SRawMatrix[,i])^2
}
SSb=(1/(a*n))*SSb-sum(RawData)^2/N

SSab = (1/n)*sum(SRawMatrix^2) - sum(RawData)^2/N - SSa - SSb
			
SSe = SSt - SSab - SSa - SSb

MSa	= SSa/(a-1)
MSb = SSb/(b-1)
MSab = SSab/((a-1)*(b-1))
MSe = SSe/(a*b*(n-1))
F0a = MSa/MSe
F0b = MSb/MSe
F0ab = MSab/MSe
Fa = qf(alpha,df1=a-1,df2=b-1,lower.tail=F)
cat(paste0(" Treatment A  SS: ",SSa," Df: ", a-1 ," MS: ",MSa, " F0a: ",F0a,
		"\n Treatment B  SS: ",SSb," Df: ", b-1 ," MS: ",MSb," F0b: ",F0b,
		"\n Interaction AB  SS: ",SSab," Df: ", (a-1)*(b-1) ," MS: ",MSab," F0ab: ",F0ab,
		"\n Error   SS: ",SSe," Df: ",a*b*(n-1) ," MS: ",MSe,
		"\n Total  SS: ",SStot," Df: ", (a*b*n)-1,
		"\n Fcrit: ", Fa))
if(F0 > Fa) print(paste0(" reject H0 ", F0)) else print(paste0(" Accept H0 ", F0))
