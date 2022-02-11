library(boot)
library(AER)
library(ggplot2)
library(stats4)
library(kernelboot)
library(simpleboot)


data("CPS1985")#determinants of wage data


#Comparison of characteristics 1)married and 2)gender on the wage of the employee
View(CPS1985)
d=CPS1985
#Comparison of wage by gender
boxplot(d$wage~d$gender, las=1, ylab="Wage ($)", 
        xlab="gender",main="Wage by Gender")
#In the case of females we have an extreme outlier which we will be dropping as it 
#might effect our study
d=d[!(d$wage > 40),]
boxplot(d$wage~d$gender, las=1, ylab="Wage ($)", 
        xlab="gender",main="Wage by Gender")
#vector for male and female wages
male = d[d$gender=='male',"wage"]
female = d[d$gender=='female',"wage"]

#data frame male and female wages
m <- d[d$gender=="male",]
f <- d[d$gender=="female",]

#################Create a function with comparison of means############################

diff.means.boot <- function(d, i) {
  
  m1 <- mean(d[i, ][d$gender=="male", "wage"])
  m2 <- mean(d[i, ][d$gender=="female", "wage"])
  
  return(m1-m2)
}

##nonparametric boot##
nonparam.diff <- two.boot(male, female, mean, R = 1000)
hist(nonparam.diff$t)


##parametric boot##

#plotting the distribution of wage for male and female to see the right fit
ggplot(m, aes(x=wage))+geom_histogram(aes(y=..density..),binwidth=1,color="red", fill="grey")+geom_density(data = data.frame(wage =rchisq(50000, df=9)), col = "black")+ labs(x = "Wage ($) for Male", y = "Density")
ggplot(f,aes(x=wage)) +geom_histogram(aes(y=..density..),binwidth=1,color="red", fill="grey") +geom_density(data = data.frame(wage =rchisq(50000, df=8)), col = "black")+ labs(x = "Wage ($) for Female", y = "Density")


#function for log likelihood
LL_f <- function(df) {
  R = dchisq(female, df)
  -sum(log(R))
}

LL_m <- function(df) {
  R = dchisq(male, df)
  -sum(log(R))
}


mle(LL_f, start = list(df = 1))
mle(LL_m, start = list(df = 1))

# because of estimation put df_m=10 and df_f=8
# plot a histogram to show the fitness accurately!!!!!(imp)

df_m <- 10
df_f <- 8

mle = c(df_f, df_m)
#Set the function that simulates data by drawing from chi-squared distribution 
#with degrees of freedom taken from MLE.
gen_function <- function(x,mle) { f <- rchisq(length(x$wage[x$gender=="female"]),mle[1])
  m <- rchisq(length(x$wage[x$gender=="male"]), mle[2])
  data = data.frame(c(f, m), c(rep("female", length(f)), rep("male",  length(m))))
  colnames(data) <-c("wage", "gender")
  return(data)
  }

param.diff <- boot(d, sim = "parametric", ran.gen = gen_function, mle = mle, statistic = diff.means.boot, R=1000)
param.diff
plot(param.diff)
param.diff$t


hist(d$wage, freq=FALSE, col="peachpuff")
lines(density(rchisq(500, df=7.5)), lwd = 2, col = "chocolate3")


##smoothed boot##

smoothed.diff=kernelboot(
  d,
  diff.means.boot,
  R = 1000,
  bw = "default",
  kernel = "gaussian",
  shrinked = TRUE
)
summary(smoothed.diff)
smoothed.diff$orig.stat
hist(smoothed.diff$boot.samples)
################Create a function for variance of difference of means##################
var.diffmeans.boot <- function(d, i) {
  
  v1 <- var(d[i, ][d$gender=="male", "wage"])
  v2 <- var(d[i, ][d$gender=="female", "wage"])
  
  n1 <- length(d[i, ][d$gender=="male", "wage"])
  n2 <- length(d[i, ][d$gender=="female", "wage"])
  return((v1/n1)+(v2/n2))
}

#nonparametric variance
nonparam.var <- boot(d, var.diffmeans.boot, R=1000, stype = "i" ) 
nonparam.var
plot(nonparam.var)
#parametric variance

param.var <- boot(d, sim = "parametric", ran.gen = gen_function, mle = mle, statistic = var.diffmeans.boot, R=1000)
param.var
plot(param.var)

#smoothed boot

smoothed.var=kernelboot(
  d,
  var.diffmeans.boot,
  R = 1000,
  bw = "default",
  kernel = "gaussian",
  shrinked = TRUE
)
summary(smoothed.var)
smoothed.var$orig.stat
hist(smoothed.var$boot.samples)

################Create a function for CI of difference of means##################

##nonparametric##
quantile(nonparam.diff$t, prob=0.025)
quantile(nonparam.diff$t, prob=0.975)

##parametric##
quantile(param.diff$t, prob=0.025)
quantile(param.diff$t, prob=0.975)

##smoothed##
quantile(smoothed.diff$boot.samples, prob=0.025)
quantile(smoothed.diff$boot.samples, prob=0.975)


