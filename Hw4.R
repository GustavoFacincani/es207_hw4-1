
#3.1 Compute both nonparametric and parametric 95% interval estimates for the median of
#the granodiorite data of exercise 2.3. Which is more appropriate for these data? Why?

grandorCh<- c(6.0, 5.0, 0.5, 0.5, 0.6, 10, 0.4, 1.2, 0.2, 0.7, 0.3, 0.2, 0.8, 0.2, 1.7, 6.0, 0.5, 3.0)
x <- grandorCh[order(grandorCh)]
#plot the data to visualize it
plot(x)

#Finding the non-parametric intervals

#According to the table on page 367 of the book "Performance evaluation of computer and communication systems" (https://perfeval.epfl.ch/), the intervals for a dataset of 18 samples, the lower and upper bounds are the 5th and 14th numbers in the distribution (in ascending order). That gives us the Rl (Rl =  x' + 1) and Ru (Ru =  n ??? x' = x).

lowerg <- x[5]
upperg <- x[14]
print(paste(x[5], "to", x[14], "mg/L"))

#Finding the parametric intervals

#Finding the parametric intervals
n = 18
alpha = 0.05
t.value <- qt(1-alpha/2, n-1)
t.value2 <- qt(1-alpha/2, n-1)
ln.grano <- log(x)
var.grano <- var(ln.grano)
mean.ln.grano <- mean(ln.grano)

lower <- exp(mean.ln.grano-t.value*sqrt(var.grano/n))
lower

upper <- exp(mean.ln.grano+t.value2*sqrt(var.grano/n))
upper

print(paste(lower, "to", upper, "mg/L"))


#The non-parametric 95% interval estimates for the median is from 0.4 to 3 mg/L, while the parametric ranges from 0.5 to 1.8 mg/L. As the dataset is small and non-normal it is better to use non-parametric, because the non-parametric provides a wider range for the confidence interval than the parametric.


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
