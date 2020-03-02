
#3.1 Compute both nonparametric and parametric 95% interval estimates for the median of
#the granodiorite data of exercise 2.3. Which is more appropriate for these data? Why?

grandorCh<- c(6.0, 5.0, 0.5, 0.5, 0.6, 10, 0.4, 1.2, 0.2, 0.7, 0.3, 0.2, 0.8, 0.2, 1.7, 6.0, 0.5, 3.0)
print(paste("Granodorite Chloride concentration in mg/L are:"))
grandorCh


#n=18 
#alfa= 0.05 for 95% confidence
#To ensure using the real median alfa/2=0.25
#p=0.5 is the median

#using the following link: https://www.youtube.com/watch?v=cxUWQCwxQgk and book https://perfeval.epfl.ch/ in page 347 

#Then, x'=4, p=0.969 and:

Ri<-5
Ru<-14

ogr<-sort(grandorCh)
ogr
median(ogr)
mean(ogr)

#using alfa/2=0.025

lowgr1<- ogr[5]+0.025*(ogr[6]-ogr[5])
uppgr1<- ogr[14]+ 0.025*(ogr[15]-ogr[14])
print(paste(lowgr1, "to", uppgr1, "mg/L"))



# I will make this a function
n<-18
y<-log(grandorCh)
y1<- mean(y)
y1
GM<- exp(y1)
GM
a<-0.05/2
#1-a=0.975
t<- qt(0.975, df=18-1)
t
s<-var(y)
s
sqp<-sqrt(s/n)
sqp



pargrai<- exp(y1-(t*sqp))

pargrau<- exp(y1+(t*sqp))

print(paste(pargrai, "< GM<", pargrau))



#cls in R 
CI<-predict(lm(grandorCh~1), inetrval="confidence", level=0.95)
CI[1]



hist(grandorCh)



#3.4 Construct the most appropriate 95 percent interval estimates for the mean and median
#annual streamflows for the Conecuh River at Brantley, Alabama (data in Appendix C2).
library(tidyverse)
Con_river <- read_csv("Conecuh_River_apxc2.csv", col_names =TRUE, cols(
  Year = col_double(),
  `Flow (cfs)` = col_double()
))
Con_river



hist(Con_river$`Flow (cfs)`)


#95 percent interval estimates for the mean
f<-(Con_river$`Flow (cfs)`)

R1cr<- mean(f + 1, na.rm=TRUE )
Rucr<- mean(f - 1, na.rm=TRUE)
R1cr
Rucr


lowcr<- mean(f, na.rm=TRUE)-2*(sd(f, na.rm=TRUE))  
uppcr<- mean(f, na.rm=TRUE)+ 2*(sd(f, na.rm=TRUE)) 

print(paste(lowcr, "to", uppcr, "mg/L"))

Pcr<- predict(lm(f~1), interval="confidence", level=0.95)
Pcr




#95 percent interval estimates for the median
#n=22 
#alfa= 0.05 for 95% confidence
#To ensure using the real median alfa/2=0.25
#p=0.5 is the median

#using the following link: https://www.youtube.com/watch?v=cxUWQCwxQgk and book https://perfeval.epfl.ch/ in page 347 

#Then, x'=5, p=0.965 and:

Ri<-6
Ru<-16

ordf<-sort(f)
ordf
median(ordf)
mean(ordf)

#using alfa/2=0.025

lowf1<- ordf[6]+0.025*(ordf[7]-ordf[6])
uppf1<- ordf[16]+ 0.025*(ordf[17]-ordf[16])

print(paste(lowf1, "to", uppf1, "mg/L"))
