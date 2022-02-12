install.packages("reshape")
install.packages("gridExtra")
install.packages("grid")
library(boot)
library(AER)
library(ggplot2)
library(stats4)
library(kernelboot)
library(simpleboot)
library(tidyverse)
library(hrbrthemes)
library(reshape)
library(gridExtra)

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

#plotting the distribution of wage for male and female to see the right fit for parametric
ggplot(m, aes(x=wage))+geom_histogram(aes(y=..density..),binwidth=1,color="red", fill="grey")+geom_density(data = data.frame(wage =rchisq(50000, df=9)), col = "black")+ labs(x = "Wage ($) for Male", y = "Density")
ggplot(f,aes(x=wage)) +geom_histogram(aes(y=..density..),binwidth=1,color="red", fill="grey") +geom_density(data = data.frame(wage =rchisq(50000, df=8)), col = "black")+ labs(x = "Wage ($) for Female", y = "Density")
#plotting the distribution of wage for male and female to see the right fit for smoothed 

sb_m <- ggplot(m, aes(x=wage))+geom_histogram(aes(y=..density..),binwidth=1,color="red", fill="white")+stat_density(adjust = 0.5, alpha=0.3,fill="blue")+ labs(x = "Wage ($) for Male", y = "Kernel Density")+
  theme_ipsum() +
  theme(axis.text=element_text(size=19),
               axis.title=element_text(size=19,face="bold"),
    plot.title = element_text(size=18)
  )

sb_f <- ggplot(f,aes(x=wage)) +geom_histogram(aes(y=..density..),binwidth=1,color="red", fill="white") +stat_density(adjust = 0.5, alpha=0.3,fill="blue")+ labs(x = "Wage ($) for Female", y = "Kernel Density",fill="")+
  theme_ipsum() +
  theme(axis.text=element_text(size=19),
        axis.title=element_text(size=19,face="bold"),
        plot.title = element_text(size=18)
  )

grid.arrange(sb_m, sb_f, ncol=2,top = "KDF and EDF of wages for men and women")

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
# plot(param.diff)
# param.diff$t
# hist(d$wage, freq=FALSE, col="peachpuff")
# lines(density(rchisq(500, df=7.5)), lwd = 2, col = "chocolate3")


##smoothed bootstrap##

#smoothed.diff=kernelboot(
#  d,
#   diff.means.boot,
#   R = 1000,
#   bw = "default",
#   kernel = "gaussian",
#   shrinked = TRUE
# )
# summary(smoothed.diff)
# smoothed.diff$orig.stat
# hist(smoothed.diff$boot.samples)

#smoothed boot 2.0
z_f <- density(female, bw=0.15, kernel="gaussian")
z_m <- density(male, bw=0.15, kernel="gaussian")

width_f <- z_f$bw
width_m <- z_m$bw 

param<-c(width_m, width_f)


rdens <- function(d, mle) {
  male <- d[d$gender =='male', 'wage']
  female <-d[d$gender =='female', 'wage']
  
  # Kernel width
  n_f <- length(female)
  n_m<-length(male)
  # Kernel sampler
  #m<-c()
  m<-sample(male, n_m, replace=TRUE) + rnorm(n_m, sd=mle[1])
  f<-sample(female, n_f, replace=TRUE) + rnorm(n_f, sd=mle[2])
  data = data.frame(c(f, m), c(rep("female", length(f)), rep("male",  length(m))))
  colnames(data) <-c("wage", "gender")
  return(data)
}

smoothed.diff <- boot(d, sim = "parametric", ran.gen = rdens, mle=param, statistic = diff.means.boot, R=1000)
hist(smoothed.diff)

#plotting kernel density
#z_f <- density(female, bw=0.15, kernel="gaussian")
#z_m <- density(male, bw=0.15, kernel="gaussian")
#width_f <- z_f$bw
#width_m <- z_m$bw 
#n_f <- length(female)
#n_m<-length(male)
#m<-sample(male, n_m, replace=TRUE) + rnorm(n_f, sd=mle[1])
#f<-sample(female, n_f, replace=TRUE) + rnorm(n_m, sd=mle[2])
#data = data.frame(c(f, m), c(rep("female", length(f)), rep("male",  length(m))))
#colnames(data) <-c("wage", "gender")
#data[data$wage<0,"wage"]<-0
#hist(data$wage)
#plot(z_f)
#plot all three for comparison and one together

#create a scaling function

scaleFUN <- function(x) sprintf("%.1f", x)

plot1 <- ggplot() + aes(nonparam.diff$t)+ geom_histogram(aes(y=..density..), binwidth=0.1, fill="lightpink1", color="#e9ecef", alpha=0.9)+
  geom_vline(aes(xintercept = mean(nonparam.diff$t)),col='blue',size=0.4,linetype="dashed")+
  geom_vline(aes(xintercept = nonparam.diff$t0),col='red',size=0.4,linetype="dashed")+
  scale_x_continuous(labels=scaleFUN)+labs(x = "Difference in mean wage",y="Density")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


plot2 <- ggplot() + aes(param.diff$t)+ geom_histogram(aes(y=..density..), binwidth=0.1, fill="darkseagreen3", color="#e9ecef", alpha=0.9)+
  geom_vline(aes(xintercept = mean(param.diff$t),col='Bootstrap_mean'),size=0.4,linetype="dashed")+
  geom_vline(aes(xintercept = param.diff$t0,col='Original_mean'),size=0.4,linetype="dashed")+
  scale_x_continuous(labels=scaleFUN)+labs(x = "Difference in mean wage",y="Density")+
  scale_color_manual(name = "", values = c(Bootstrap_mean = "blue", Original_mean = "red"))+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

plot3 <- ggplot() + aes(smoothed.diff$t)+ geom_histogram(aes(y=..density..), binwidth=0.1, fill="lightblue", color="#e9ecef", alpha=0.9)+
  geom_vline(aes(xintercept = mean(smoothed.diff$t)),col='blue',size=0.4,linetype="dashed")+
  geom_vline(aes(xintercept = smoothed.diff$t0),col="red",size=0.4,linetype="dashed") +
  scale_x_continuous(labels=scaleFUN)+labs(x = "Difference in mean wage",y="Density")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

#Combined density graphs
df <- data.frame(Nonparametric=nonparam.diff$t,
                 Parametric=param.diff$t,
                 Smoothed=smoothed.diff$t)


#convert from wide format to long format
data <- melt(df)
head(data)


#create overlaying density plots
plot4 <- ggplot(data, aes(x=value, fill=variable)) +
  geom_density(alpha=.25)+ scale_x_continuous(labels=scaleFUN)+labs(x = "Difference in mean wage",y="Density",fill = "")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2,top = "Difference in mean wages between men and women")

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

# smoothed.var=kernelboot(
#   d,
#   var.diffmeans.boot,
#   R = 1000,
#   bw = "default",
#   kernel = "gaussian",
#   shrinked = TRUE
# )
# summary(smoothed.var)
# smoothed.var$orig.stat

#Smoothed bootstrap var 2.0

smoothed.var <- boot(d, sim = "parametric", ran.gen = rdens, mle=param, statistic = var.diffmeans.boot, R=1000)
hist(smoothed.diff)

###fitting the kernel density function


#create a scaling function
scaleFUN <- function(x) sprintf("%.3f", x)

plot1 <- ggplot() + aes(nonparam.var$t)+ geom_histogram(aes(y=..density..), binwidth=0.0025, fill="lightpink1", color="#e9ecef", alpha=0.9)+
  geom_vline(aes(xintercept = mean(nonparam.var$t)),col='blue',size=0.4,linetype="dashed")+
  geom_vline(aes(xintercept = nonparam.var$t0),col='red',size=0.4,linetype="dashed")+
  scale_x_continuous(labels=scaleFUN)+labs(x = "Variance of difference in mean wage",y="Density")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


plot2 <- ggplot() + aes(param.var$t)+ geom_histogram(aes(y=..density..), binwidth=0.0025, fill="darkseagreen3", color="#e9ecef", alpha=0.9)+
  geom_vline(aes(xintercept = mean(param.var$t),col='Bootstrap_mean'),size=0.4,linetype="dashed")+
  geom_vline(aes(xintercept = param.var$t0,col='Original_mean'),size=0.4,linetype="dashed")+
  scale_x_continuous(labels=scaleFUN)+labs(x = "Variance of difference in mean wage",y="Density")+
  scale_color_manual(name = "", values = c(Bootstrap_mean = "blue", Original_mean = "red"))+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

plot3 <- ggplot() + aes(smoothed.var$t)+ geom_histogram(aes(y=..density..), binwidth=0.0025, fill="lightblue", color="#e9ecef", alpha=0.9)+
  geom_vline(aes(xintercept = mean(smoothed.var$t)),col='blue',size=0.4,linetype="dashed")+
  geom_vline(aes(xintercept = smoothed.var$t0),col="red",size=0.4,linetype="dashed") +
  scale_x_continuous(labels=scaleFUN)+labs(x = "Variance of difference in mean wage",y="Density")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

#Combined density graphs
df <- data.frame(Nonparametric=nonparam.var$t,
                 Parametric=param.var$t,
                 Smoothed=smoothed.var$t)


#convert from wide format to long format
data <- melt(df)
head(data)


#create overlaying density plots
plot4 <- ggplot(data, aes(x=value, fill=variable)) +
  geom_density(alpha=.25)+ scale_x_continuous(labels=scaleFUN)+labs(x = "Variance of difference in mean wage",y="Density",fill = "")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2,top = "Variance of difference in mean wages between men and women")


################Create a function for CI of difference of means##################


df <- data.frame(Nonparametric=nonparam.diff$t,
                 Parametric=param.diff$t,
                 Smoothed=smoothed.diff$t)


#convert from wide format to long format
data <- melt(df)
head(data)

df <- data.frame(do.call("rbind",
        tapply(data$value,       # Specify numeric column
               data$variable,            # Specify group variable
               quantile,probs=c(0.025,0.975))))

mean = list(mean(nonparam.diff$t), mean(param.diff$t), mean(smoothed.diff$t))

df$mean <- unlist(mean)

df$true_mean <- c(nonparam.diff$t0,param.diff$t0,smoothed.diff$t0)

df$row_names <- row.names(df)

df

#Plotting the CI along with the means
ggplot(df, aes(row_names, mean)) +        # ggplot2 plot with confidence intervals
  geom_point(data=df, mapping=aes(x=row_names, y=true_mean,colour="Original Mean")) +
  geom_point(data=df, mapping=aes(x=row_names, y=mean,colour="Bootstrap Mean")) +
  geom_errorbar(aes(ymin =  X2.5., ymax = X97.5.))+labs(title="Comparison of 95% Confidence Intervals",y = "Confidence Interval (95%)",x="Bootstrap method",fill = "")+theme_ipsum() 
  
  
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

# ##nonparametric##
# quantile(nonparam.diff$t, prob=0.025)
# 
# quantile(nonparam.diff$t, prob=0.975)
# 
# ##parametric##
# quantile(param.diff$t, prob=0.025)
# quantile(param.diff$t, prob=0.975)
# 
# ##smoothed##
# quantile(smoothed.diff$boot.samples, prob=0.025)
# quantile(smoothed.diff$boot.samples, prob=0.975)


