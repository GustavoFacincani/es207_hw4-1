---
title: "Naivy_es207_HW4_"
output:
  html_document:
    df_print: paged
date: "02/17/2020"
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

                      HOMEWORK 4 
                      
>Helsel and Hirsch CH3

3.1 Compute both nonparametric and parametric 95% interval estimates for the median of
the granodiorite data of exercise 2.3. Which is more appropriate for these data? Why?

```{r}
grandorCh<- c(6.0, 5.0, 0.5, 0.5, 0.6, 10, 0.4, 1.2, 0.2, 0.7, 0.3, 0.2, 0.8, 0.2, 1.7, 6.0, 0.5, 3.0)
print(paste("Granodorite Chloride concentration in mg/L are:"))
grandorCh

```


NON PARAMETRIC 95% FOR MEDIAN 

```{r}
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
```


PARAMETRIC 95% FOR THE MEDIAN

```{r}
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
```

```{r}
pargrai<- exp(y1-(t*sqp))

pargrau<- exp(y1+(t*sqp))

print(paste(pargrai, "< GM<", pargrau))
```

#Confidence interval 

```{r}
#cls in R 
CI<-predict(lm(grandorCh~1), inetrval="confidence", level=0.95)
CI[1]
```

```{r}
hist(grandorCh)
```

    Non-parametric will be a better aproach, as we saw in class, when you have a small data set and its no-normal distributed it is better to use it because it has a widther range feeling confident of having the values close to the median.
    



3.4 Construct the most appropriate 95 percent interval estimates for the mean and median
annual streamflows for the Conecuh River at Brantley, Alabama (data in Appendix C2).

```{r}
library(tidyverse)
Con_river <- read_csv("Conecuh_River_apxc2.csv", col_names =TRUE, cols(
  Year = col_double(),
  `Flow (cfs)` = col_double()
))
Con_river
```

```{r}
hist(Con_river$`Flow (cfs)`)
```

    The data looks king of symetric, but it is not non-normal distributed and it is a small data set. Therefore, I am using non-parametric


95 percent interval estimates for the mean

```{r}
f<-(Con_river$`Flow (cfs)`)

R1cr<- mean(f + 1, na.rm=TRUE )
Rucr<- mean(f - 1, na.rm=TRUE)
R1cr
Rucr
```

```{r}
lowcr<- mean(f, na.rm=TRUE)-2*(sd(f, na.rm=TRUE))  
uppcr<- mean(f, na.rm=TRUE)+ 2*(sd(f, na.rm=TRUE)) 

print(paste(lowcr, "to", uppcr, "mg/L"))
 
Pcr<- predict(lm(f~1), interval="confidence", level=0.95)
Pcr
```

#The difference between 682.65-556.6 is 126.13

    *For the mean the interval is 682.65+/- 126.13 from 556.6->808.89 alfa= 0.05 (95%)
    

95 percent interval estimates for the median

```{r}
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
```



>Quian CH4

4. The Everglades wetland ecosystems are phosphorus limited. After the Everglades Agriculture Areas (EAA) were established (enabled by a series of federal government constructed water diversion systems for draining part of the Everglades wetland), phosphorus-rich agriculture runoff reached the Everglades wetland and resulted in dramatic changes in parts of the Everglades wetlands. To better protect the Everglades, many studies were conducted in the late 1980s and the 1990s to learnabout the effects of phosphorus enrichment in the Everglades. One study focused on estimating the background level of phosphorus concentration. To identify which site is not affected by the agriculture runoff, researchers measured phosphatase activity (APA) in sites known to be affected (TP > 30 ug/L) and sites that are unaffected by agriculture runoff. Phosphatase is an enzyme produced by organisms in low P environment.
Because producing this enzyme costs energy, organisms do not produce them when bio-available phosphorus are present. As a result, high APA is an indicator of P limitation. The data file apa.s contains both APA and TP concentrations. It can be imported into R using function source.

```{r}
library(tidyverse)
library(lattice)
library(tidyr)
source("apa.s")
```

(a) Compare the distributions of APA from sites with TP > 30 ug/L and APA from sites with TP < 30 ug/L using graphical tools we learned in Chapter 3.

```{r}
apa.dat %>%
   filter(tp > 30)->tp30

apa.dat %>%
  filter(tp < 30)->tpl30

hist(tp30$apa, xlab="TP > 30ug/L", main=paste("Tp> APA DATA") )
hist(tpl30$apa, xlab="TP < 30ug/L", main=paste("Tp< APA DATA"))
```

```{r}
tp30 %>%
  pull(apa) -> apatp30

tpl30 %>%
  pull(apa) -> apatpl30

qapa <- qqplot(x = apatp30, y = apatpl30, plot.it = FALSE)


ggplot(as.data.frame(qapa), aes(x = x, y = y)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  coord_fixed(ratio = 1, 
              xlim = range(c(qapa$x, qapa$y)), 
              ylim = range(c(qapa$x, qapa$y))) +
  xlab('TP > 30') +
  ylab('TP < 30') +
  ggtitle('APA TP DATA')

```



(b) What is the nature of difference between the two populations of APA?

    From the plots we can see that the distributions are not normal, they have a positive skewness with more variability the data with Tp<30ug/L.They are multiplicative as the start from the cero, but do not follow the straight line, the separate from it. 

#the diference between the two populations are likely multiplicative. As a result, the diference in log TP concentrations is likely additive. 

#The 95% refers to the probability that a confidence interval includes the true value of the mean


(c) Use an appropriate test to determine whether the difference is statistically significant and describe the result in non-technical terms.


```{r}
#The normal approximation (also known as the continuity correction)

wilcox.test(apatp30,apatpl30, mu=log(10))

```

    I can conclude that the p-value is: 6.615e-08, which is less than the signficance level alpha pf 0.05. Then, TP<30 and TP<30 median s significantly different with that p-value
    
#http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r





```{r}
#one test we could try is the t-test if the data was parametric

t.test(x=apatp30, y=apatpl30, alternative = "greater", var.equal = T)

```