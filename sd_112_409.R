###========================
### problem-01

library(lifecontingencies)
showClass("lifetable")
showClass("actuarialtable")
x_sample<-seq(from=0, to=18, by=1)
lx_sample<-c(900,850,800,750,700,650,600,550,500,450,400,350,300,250,200,150,100,50,0)

# Answer to the question no. a
lifetable<-new("lifetable",x=x_sample,lx=lx_sample, name="lifetable")
print(lifetable)

# Answer to the question no. b
exampleAct<-new("actuarialtable", x=x_sample, lx=lx_sample, interest = 0.03, name ="example actuarialtable")
print(exampleAct)

# Answer to the question no. c
DemoEX1<-pxt(lifetable, 5,1)
c(DemoEX1)

# Answer to the question no. d
DemoEX2<-qxt(lifetable, 5, 1)
c(DemoEX2)

# Answer to the question no. e
l5=650; l6=600; l7=550
ans=1-(l6-l7)/l5
ans

# Answer to the question no. f
DemoEX3<-exn(lifetable, 5, 1, "complete")
c(DemoEX3)

# Answer to the question no. g
DemoEX4<-mxt(lifetable, 5, 1)
c(DemoEX4)

# Answer to the question no. h
DemoEX5<-dxt(lifetable, 5, 6)
c(DemoEX5)

# Answer to the question no. i
DemoEX10<-dxt(lifetable, 10, 15)
c(DemoEX10)

# Answer to the question no. j
Axn(actuarialtable=exampleAct, x=10, n=15)

# Answer to the question no. k
Axn(actuarialtable=exampleAct, x=10, n=12,i=0.06)

# Answer to the question no. l(i)
M_10=261.16551
D_10=297.63757
#the value of whole-life assurance
WA=1000*(M_10/D_10)
WA

# Answer to the question no. l(ii)
D_16=96.27929
D_10=132.22356
#the value of pure endowment assurance
PE=500*(D_16/D_10)
PE

# Answer to the question no. l(iii)
M_10=261.16551
M_16=59.62055
D_10=132.22356
#the value of temporary assurance
TA=1000*((M_10-M_16)/D_10)
TA

# Answer to the question no. l(iv)
M_10=261.16551
M_16=59.62055
D_10=132.22356
D_16=96.27929
#the value of endowment assurance
EA=1000*((M_10-M_16+D_16)/D_10)
EA




###==================================
### problem-02
# Answer to the question no. a
library(lifecontingencies)
i=0.07
monthlyInt=(1+i)^(1/12)-1
Capital=150000
#Monthly installment
R=1/12*Capital/annuity(i=i, n=10,k=12, type = "immediate")
R
# Monthly installment is 1725.05
#loan summary
balance=numeric(10*12+1)
capitals=numeric(10*12+1)
interests=numeric(10*12+1)
balance[1]=Capital
interests[1]=0
capitals[1]=0
for(i in (2:121)) {
  balance[i]=balance[i-1]*(1+monthlyInt)-R
  interests[i]=balance[i-1]*monthlyInt
  capitals[i]=R-interests[i]
}
loanSummary=data.frame(rate=c(0, rep(R,10*12)),balance, interests, capitals)
loanSummary

# Answer to the question no. b
RA <- 150000/accumulatedValue(i=0.07, n=10)
RA
RB <- 120000/accumulatedValue(i=0.06, n=12)
RB
RC <- 200000/accumulatedValue(i=0.08, n=15)
RC
RD <- 100000/accumulatedValue(i=0.075, n=10)
RD
RE <- 160000/accumulatedValue(i=0.08, n=20)
RE

100*annuity(i=0.5, n=6)
100*accumulatedValue(i=0.05, n=6)

iannuity_A<-annuity(i=0.07, n=10, k=1, type="immediate")
dannuity_A<-annuity(i=0.07, n=10, k=12, type="due")
c(iannuity_A,dannuity_A)

iannuity_B<-annuity(i=0.06, n=12, k=1, type="immediate")
dannuity_B<-annuity(i=0.06, n=12, k=12, type="due")
c(iannuity_B,dannuity_B)

iannuity_C<-annuity(i=0.08, n=15, k=1, type="immediate")
dannuity_C<-annuity(i=0.08, n=15, k=12, type="due")
c(iannuity_C,dannuity_C)

iannuity_D<-annuity(i=0.075, n=10, k=1, type="immediate")
dannuity_D<-annuity(i=0.075, n=10, k=12, type="due")
c(iannuity_D,dannuity_D)

iannuity_E<-annuity(i=0.08, n=20, k=1, type="immediate")
dannuity_E<-annuity(i=0.08, n=20, k=12, type="due")
c(iannuity_E,dannuity_E)

#Answer to question no. c
#payment made at the end of one year from now
i=0.04 
V=1/(1+i)
# the present value
PV=500*((1-V^5)/i)
PV
#payment made at the beginning of the year
i=0.04
V=1/(1+i)
PV=500*(1+((1-V^4)/i))
PV



###===========================
###problem-03
#3(a)
#Fund at the beginning of the year
A=12500000
#Fund at the end of the year
PI=1000000
GI=1250000
IT=125000
I=GI-IT
C=250000
E=125000
B=A+PI+I-C-E
#net interest rate
i=((2*I)/(A+B-I))
i
#gross interest
I1=1250000
i1=((2*I1)/(A+B-I))
i1

#3(b)
# (i) Policy Value
i=0.10 
n=10
V=1/(1+i)
K=10000
AC=(V^n)*K
A_n=((1-V^n)/i)
A_n1=(1+i)*A_n
P=AC/A_n1
P
# Policy value
n=5
S_n=((1+i)^n-1)/i
S_n1=(1+i)*S_n
PValue=P*S_n1
PValue
#(ii) Paid-up policy value
Paidvalue=PValue/V^(10-n)
Paidvalue




###==============================
### problem-04
#3(a)
#Fund at the beginning of the year
A=12500000
#Fund at the end of the year
PI=1000000
GI=1250000
IT=125000
I=GI-IT
C=250000
E=125000
B=A+PI+I-C-E
#net interest rate
i=((2*I)/(A+B-I))
i
#gross interest
I1=1250000
i1=((2*I1)/(A+B-I))
i1

#3(b)
# (i) Policy Value
i=0.10 
n=10
V=1/(1+i)
K=10000
AC=(V^n)*K
A_n=((1-V^n)/i)
A_n1=(1+i)*A_n
P=AC/A_n1
P
# Policy value
n=5
S_n=((1+i)^n-1)/i
S_n1=(1+i)*S_n
PValue=P*S_n1
PValue
#(ii) Paid-up policy value
Paidvalue=PValue/V^(10-n)
Paidvalue


###======================
### problem-05
# answer to question a
library(lifecontingencies)
valdezDf<- data.frame(x=c(0:10), lx=c(100000,92770,90204,89436,88867,88298,87504,86792,85938,84651,82802),
                      heart=c(1741,549,54,7,16,0,0,24,30,18,0), 
                      accident=c(61,82,43,59,64,119,137,122,300,646,928),
                      other=c(5428,1935,671,503,490,675,595,708,958,1185,1185))
valdezMdt<-new("mdt",name="ValdezExample",table=valdezDf)
print(valdezMdt)

#Answer to question b
dxt(valdezMdt, x=5, decrement= "heart")
dxt(valdezMdt, x=5, decrement= "accident")
dxt(valdezMdt, x=5, decrement= "other")

#Answer to question c
dxt(valdezMdt, x=5,t=2, decrement= "heart")
dxt(valdezMdt, x=5,t=2, decrement= "accident")
dxt(valdezMdt, x=5,t=2, decrement= "other")

#Answer to question d
dxt(valdezMdt, x=5,t=4, decrement= "heart")
dxt(valdezMdt, x=5,t=4, decrement= "accident")
dxt(valdezMdt, x=5,t=4, decrement= "other")

#Answer to question e
pxt(valdezMdt, x=1,t=2)

#Answer to question f
qxt(valdezMdt, x=1,t=2)



####============================
#### problem-06
# Joint Life Calculation Example
ages <- c(65, 62)
qx1 <- 0.005 * exp(0.05 * (0:40))
qx2 <- 0.004 * exp(0.05 * (0:40))
n <- 40
i <- 0.05
v <- 1 / (1 + i)

px1 <- pmin(1 - qx1[1:n], 1)
px2 <- pmin(1 - qx2[1:n], 1)
px1
px2
# Joint survival (both alive)
tpxy <- numeric(n + 1)
tpxy[1] <- 1
for (k in 1:n) tpxy[k + 1] <- tpxy[k] * px1[k] * px2[k]
tpxy

# Last survivor (at least one alive)
tpx <- numeric(n + 1)
tpx[1] <- 1
tpy <- numeric(n + 1)
tpy[1] <- 1
for (k in 1:n) {
  tpx[k + 1] <- tpx[k] * px1[k]
  tpy[k + 1] <- tpy[k] * px2[k]
}
tp_last <- tpx + tpy - tpxy
tp_last
# Construct table
cat(sprintf("%-6s  %-12s  %-12s  %-12s  %-14s\n",
            "Year", "tpx", "tpy", "tpxy", "tp(last surv)"))
for (t in c(0, 1, 5, 10, 15, 20, 25, 30, 40)) {
  if (t > n) next
  cat(sprintf("%-6d  %-12.6f  %-12.6f  %-12.6f  %-14.6f\n",
              t, tpx[t + 1], tpy[t + 1], tpxy[t + 1], tp_last[t + 1]))
}

# Joint life annuity-due
axy <- sum(tpxy[1:(n + 1)] * v^(0:n))
cat(sprintf("\nJoint life annuity-due: %.5f\n", axy))

# Last survivor annuity-due
a_last <- sum(tp_last[1:(n + 1)] * v^(0:n))
cat(sprintf("Last survivor annuity-due: %.5f\n", a_last))

