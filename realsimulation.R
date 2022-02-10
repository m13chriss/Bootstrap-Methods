library(boot)
library(AER)
library(ggplot2)
library(stats4)
library(kernelboot)


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

male = d[d$gender=='male',]
female = d[d$gender=='female',]
#plotting the distribution of wage for male and female
ggplot(male, aes(x=wage))+geom_histogram(aes(y=..density..),binwidth=1,color="red", fill="grey")+geom_density(data = data.frame(wage =rchisq(50000, df=9)), col = "black")+ labs(x = "Wage ($) for Male", y = "Density")
ggplot(female,aes(x=wage)) +geom_histogram(aes(y=..density..),binwidth=1,color="red", fill="grey") +geom_density(data = data.frame(wage =rchisq(50000, df=8)), col = "black")+ labs(x = "Wage ($) for Female", y = "Density")


####
ggplot() + 
  geom_line(data = d, aes(x = A, y = B), col = "red") +
  geom_histogram(data = d, aes(x = A), alpha = .5) +
  facet_wrap(~ ss,as.table=T)

#mean for male and female wages
m <- d$wage[d$gender=="male"]
f <- d$wage[d$gender=="female"]

geom_density(aes(y=0.5*..count..), colour="black", adjust=4) 


ggplot(d[d$gender=='male',], aes(wage)) +geom_density(aes(y=0.5*..count..), colour="black", adjust=4)+xlim(0,30)
ggplot(d[d$gender=='female',], aes(wage)) +geom_density() +xlim(0,30)
#################Create a function with comparison of means############################

diff.means.boot <- function(d, i) {
  
  m1 <- mean(d[i, ][d$gender=="male", "wage"])
  m2 <- mean(d[i, ][d$gender=="female", "wage"])
  
  return(m1-m2)
}

#nonparametric boot
nonparam <- boot(d, diff.means.boot, R=1000, stype = "i" ) 
nonparam
plot(nonparam)

#parametric boot
LL_f <- function(df) {
  R = dchisq(f, df)
  -sum(log(R))
}

LL_m <- function(df) {
  R = dchisq(m, df)
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

param <- boot(d, sim = "parametric", ran.gen = gen_function, mle = mle, statistic = diff.means.boot, R=1000)
param
plot(param)


hist(d$wage, freq=FALSE, col="peachpuff")
lines(density(rchisq(500, df=7.5)), lwd = 2, col = "chocolate3")


#smoothed boot

k=kernelboot(
  d,
  diff.means.boot,
  R = 1000,
  bw = "default",
  kernel = "gaussian",
  shrinked = TRUE
)
summary(k)
k$orig.stat
hist(k$boot.samples)


################Create a function for variance of difference of means##################
var.diffmeans.boot <- function(d, i) {
  
  v1 <- var(d[i, ][d$gender=="male", "wage"])
  v2 <- var(d[i, ][d$gender=="female", "wage"])
  
  n1 <- length(d[i, ][d$gender=="male", "wage"])
  n2 <- length(d[i, ][d$gender=="female", "wage"])
  return((v1/n1)+(v2/n2))
}

#nonparametric variance
nonparam <- boot(d, var.diffmeans.boot, R=1000, stype = "i" ) 
nonparam
plot(nonparam)

#parametric variance

param <- boot(d, sim = "parametric", ran.gen = gen_function, mle = mle, statistic = var.diffmeans.boot, R=1000)
param
plot(param)

#smoothed boot

k=kernelboot(
  d,
  var.diffmeans.boot,
  R = 1000,
  bw = "default",
  kernel = "gaussian",
  shrinked = TRUE
)
summary(k)
k$orig.stat
hist(k$boot.samples)

################Create a function for CI of difference of means##################
# var.diffmeans.boot <- function(d, i) {
#   
#   m1 <- mean(d[i, ][d$gender=="male", "wage"])
#   m2 <- mean(d[i, ][d$gender=="female", "wage"])
#   
#   v1 <- var(d[i, ][d$gender=="male", "wage"])
#   v2 <- var(d[i, ][d$gender=="female", "wage"])
#   
#   n1 <- length(d[i, ][d$gender=="male", "wage"])
#   n2 <- length(d[i, ][d$gender=="female", "wage"])
#   
#   CIn=(m1-m2)-1.96*((v1/n1)+(v2/n2))
#   CIm=(m1-m2)+1.96*((v1/n1)+(v2/n2))
#   
#   
#   return()
#}


# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")
