rm(list = ls(all=TRUE))

pnorm(29000,29321,2120/sqrt(100))

pnorm(215,220,15/sqrt(40))

1.64*0.449

sqrt(880.4)

0.14583 + 1.96*sqrt(0.14583*0.854/1200)

qnorm(0.05,lower.tail = F)
marginError <- qnorm(0.05,lower.tail = F)*4.49/10
49 - marginError
49 + marginError

 z <- qnorm(0.025, lower.tail = F)
n <- 
  
  p <- 175/1200
q <- 1-p

lowerLimit <- p - 1.96*sqrt(p*q/1200)
UpperLimit <- p + 1.96*sqrt(p*q/1200)


n <- 35
mean <- 2.364
var <- 0.81
90% confidence internval
tvalue <- qt(0.05,34,lower.tail = F) # we have 34 degrees of freedom
2.364 - tvalue * 0.9/sqrt(35)
2.364 + tvalue * 0.9/sqrt(35)


marginError <- qnorm(0.025,lower.tail = F)
lowerLimit <- 1014 - marginError*25/10
upperLimit <- 1014 + marginError*25/10
lowerLimit
upperLimit



# 95%
n <- 36
z <- qnorm(0.025,lower.tail = F)
ME <- z * 0.3/sqrt(n)
lowerLimit <- 2.6 - ME
upperLimit <- 2.6 + ME
c(lowerLimit,upperLimit)

# 99%
z <- qnorm(0.005,lower.tail = F)
ME <- z * 0.3/sqrt(n)
lowerLimit <- 2.6 - ME
upperLimit <- 2.6 + ME
c(lowerLimit,upperLimit)

qnorm(0.05, lower.tail = F)

pnorm(1.164,9900,120/sqrt(30))

alpha <- 0.05
limitOnCriticalZone <- qnorm(0.05,lower.tail = F) # 1.644854
options(scipen = 2)
actual <- pnorm(9900,10000,120/sqrt(30)) # output 0.000002505166
actual
#As actual < limitOnCriticalZone, we can reject Manufacturer's claim


mean <- 10000
n = 30
smean = 9900
sd = 120
zval <- (9900-10000)/(120/sqrt(30))
zval # -4.56
limitOnCriticalZone <- qnorm(0.05,lower.tail = F) # 1.644854
#-4.56 < 1.64 reject the claim

n = 100
p = 0.84
q = 0.16
mean = 100 * 0.84 # 84
sd = sqrt(100*0.84*0.16) # 3.666061

1 - pnorm(80,84,3.67) # 0.86

qnorm(0.95)

cal <- (84-80)/ sqrt(100*0.84*0.16)


qnorm(0.025)
