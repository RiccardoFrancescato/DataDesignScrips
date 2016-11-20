#Copyright (c) 2016 Riccardo Francescato
p = 5 # number of rows 
N = p^2
RawLatin=c("A","B","C","D","E",
  			"B","C","D","E","A",
  			"C","D","E","A","B",
  			"D","E","A","B","C",
  			"E","A","B","C","D")
RawData = c(-1,-5,-6,-1,-1,
  		   	-8,-1, 5, 2,11,
  		   	-7,13, 1, 2,-4,
  		   	 1, 6, 1,-2,-3,
  		   	-3, 5,-5, 4, 6)

data = matrix(RawData,nrow=a,ncol=b,byrow = TRUE)        
latin = matrix(RawLatin,nrow=a,ncol=b,byrow = TRUE)  

SStot = sum(data^2) - sum(data)^2/N

SSrows = 0
for(i in 1:p){
	SSrows = SSrows + sum(data[i,])^2
}
SSrows = SSrows *(1/p) - sum(data)^2/N

SScol = 0
for(i in 1:p){
	SScol = SScol + sum(data[,i])^2
}
SScol = SScol *(1/p) - sum(data)^2/N

TreatTot = vector(,p)
for(i in 1:p){
	TreatTot[i]=0
}

for(i in 1:N){
	if(RawLatin[i]=="A") TreatTot[1]=TreatTot[1]+RawData[i]
	if(RawLatin[i]=="B") TreatTot[2]=TreatTot[2]+RawData[i]
	if(RawLatin[i]=="C") TreatTot[3]=TreatTot[3]+RawData[i]
	if(RawLatin[i]=="D") TreatTot[4]=TreatTot[4]+RawData[i]
	if(RawLatin[i]=="E") TreatTot[5]=TreatTot[5]+RawData[i]
}

SStreat=0
for(i in 1:p){
	SStreat = SStreat + TreatTot[i]^2
}
SStreat = SStreat *(1/p) - sum(data)^2/N


SSerr = SStot - SSrows - SScol - SStreat

MStreat = SStreat/(p-1)
MSrows = SSrows/(p-1)
MScols = SScol/(p-1)
MSerr = SSerr/((p-2)*(p-1))

F0 <- MStreat/MSerr
Fa <- qf(alpha,df1=a-1,df2=((a-1)*(b-1)),lower.tail=F)

if(F0 > Fa) print(paste0(" reject H0 ", F0)) else print(paste0(" Accept H0 ", F0))
cat(paste0(" Treatments  SS: ",SStreat," Df: ", p-1 ," MS: ",MStreat, " F0: ",F0,
		"\n Rows  SS: ",SSrows," Df: ", p-1 ," MS: ",MSrows,
		"\n Cols  SS: ",SScol," Df: ", p-1 ," MS: ",MScols,
		"\n Error  SS: ",SSerr," Df: ", (p-2)*(p-1) ," MS: ",MSerr,
		"\n Total  SS: ",SStot," Df: ", (p^2)-1))


delivery.df = data.frame(
  Opersators = c(rep("O1", p), rep("O2", p), rep("O3", p), rep("O4", p), rep("O5", p)),
  Batches = c(rep(c("B1", "B2", "B3", "B4","B5"), p)),
  Formulation = RawLatin,
  Data = RawData
)
delivery.df

delivery.mod1 = lm(Data ~ Formulation+Batches+Opersators, delivery.df)
anova(delivery.mod1)
