# Answer to the question no. a 
library(lifecontingencies)
library(survival)
#data=read.csv(file = "telecom_churn.csv",header = TRUE, sep=",")
data <- read.csv(file.choose())
dim(data)
head(data)
names(data)

# Answer to the question no. b 

# set up Surv() object
dat<-data[,c("Account.length","Churn")]
#taking the churn value as 0(false) and 1 (true)
#dat$churn<-as.numeric(data$Churn)-1

dat$Churn<-ifelse(dat$Churn =="TRUE",1,0)

dat$Churn
 
# Answer to the question no. c 
#fit the model
survdat<-Surv(time=dat$Account.length,event=dat$Churn)
fit<-survfit(survdat~1,se=TRUE)
plot(fit,main="Survival Function",xlab="time",ylab="Survival Probability")
#median time to churn
fit

#Answer to the question no. d 
#fit cox proportional hazards model
fitt <- coxph(Surv(dat$Account.length,dat$Churn)~State+Account.length+Int.l.Plan+VMail.Plan+Day.Mins,data=data)
summary(fitt)
