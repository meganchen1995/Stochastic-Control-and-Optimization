###Optimization Final Review###

#########################
#1.Linear Programming
library('lpSolve')

#objective coefficients
c<-c(80,129)

#LHS of constraints
#
# define a 4x2 matrixn of zeros
A<-matrix(0,4,2)

# assembly constraint
A[1,]<-c(5,6)
# testing constraint
A[2,]<-c(1,2)
#Max constraints
A[3,]<-c(1,0)
A[4,]<-c(0,1)

#RHS of constraints
b<-c(10000,3000,600,1200)

#All constraints have a <=
dir<-rep("<=",4)

#solve the LP and assign the returned strcture to variable s
s=lp("max",c,A,dir,b,compute.sens = 1)
s$solution
s$objval
data.frame(s$duals, s$duals.from, s$duals.to)

#########################

#2.Integer Programming
library('lpSolve')
# X arranged as [y1.. y5, x1...x5]
obj = c(-25,-35,-28,-20,-40,10,18,11,9,10)

# 14 constraints and 10 Varianbles in A
A=matrix(0,14,10)

#Cost constraint
A[1,1:10]=c(25,35,28,20,40,5,7,6,4,8)

#y1+y5<=1
A[2,1]=1;A[2,5]=1

#y1-y2-y3<=0
A[3,1]=1;A[3,2]=-1;A[3,3]=-1;

#-y3-y4-y5<=-2
A[4,3]=-1;A[4,4]=-1;A[4,5]=-1;

#yi - xi <=0
for(i in 5:9){
  A[i,i-4]=1; A[i,i+1]=-1;
}

#xi - yi*Max_i <=0
A[10,6]=1;A[10,1]=-5;
A[11,7]=1;A[11,2]=-4;
A[12,8]=1;A[12,3]=-5;
A[13,9]=1;A[13,4]=-7;
A[14,10]=1;A[14,5]=-3;

dir = c(rep("<=",14))
b = c(125,1,0,-2,rep(0,10))

S0 = lp("max",obj,A,dir,b,binary.vec=1:5,int.vec=6:10)

# Can you change the budget between 100 and 150 to see how it impacts your NPV?
sVec=rep(0,51)

for(budget in 100:150){
  b = c(budget,1,0,-2,rep(0,10))
  S = lp("max",obj,A,dir,b,binary.vec=1:5,int.vec=6:10)
  sVec[budget-99]=S$objval
}

plot(100:150,sVec)

#########################

#3.Non-Linear Programming

#a.un-constrained
#The 100 is simply a guess and can be any number.
#
# Soution: x=93.33 and fval=-9343.95
#
# you can plot this function using
#
# price=seq(80,120,.1);
# plot(price,-func(price));
#

func <- function(price){ 
  demand=(3777178*price^(-2.154))
  profit=demand*(price-50)
  return(-profit)
}
S=optim(100,func,method="CG")

#b.constrained
library(quadprog)
d = rep(0,5)
D = diag(5)
D[1,1] = 2
D[2,2] = 8
D[3,3] = 12
D[4,4] = 24
D[5,5] = 6
A = matrix(c(1,1,0,0,0,-1,0,1,1,0,0,-1,-1,0,1),5,3)
b = c(710,0,0)
solution = solve.QP(D,d,A,b)
print(solution$solution)

##eff-frontier
m=c(0.1073,0.0737,0.0627)
s=c(0.1667,0.1055,0.0340)

rho=matrix(c(1,0.2199,0.0366,0.2199,1,-0.0545,0.0366,-0.0545,1),3,3,byrow=TRUE)

covMat=diag(s) %*% rho %*% diag(s)

RVals=seq(0.065,0.105,0.005)
StdDevs=rep(0,length(RVals))

for (i in 1:length(RVals)){
  
  Dmat=2*covMat
  dvec=rep(0,3)
  Amat=matrix(c(1,1,1,-1,-1,-1,m),3)
  bvec=c(1,-1,RVals[i])
  
  #no shorting
  Amat=cbind(Amat,diag(3))
  bvec=c(bvec,0,0,0)
  
  S=solve.QP(Dmat,dvec,Amat,bvec)
  StdDevs[i]=sqrt(S$value)
  
}
plot(StdDevs,RVals,"l")

#########################

#4.Network
#Trans-shipment
library(lpSolveAPI)


#Var Arrangement (ab stands for a->b: eg: mn stands from Memphis to New York)
#ind:           1     2     3     4     5     6     7     8     9     10    11    12     13   14 
ColNames <- c("Xmn","Xmc","Xml","Xmb","Xdn","Xdc","Xdl","Xdb","Xnc","Xnl","Xnb","Xcn","Xcl","Xcb")

tShipOpt<-make.lp(0,14)

#set objective coefficients
set.objfn(tShipOpt, c(8,13,25,28,15,12,26,25,6,16,17,6,14,16))

#set objective direction
lp.control(tShipOpt,sense='min')

add.constraint(tShipOpt,rep(1,4), "<=",150,c(1:4))
add.constraint(tShipOpt,rep(1,4), "<=",200,c(5:8))

add.constraint(tShipOpt,c(1,1,1,-1,-1,-1), "=",0,c(1,5,12,9,10,11))
add.constraint(tShipOpt,c(1,1,1,-1,-1,-1), "=",0,c(2,6,9,12,13,14))

add.constraint(tShipOpt,rep(1,4), ">=",130,c(3,7,10,13))
add.constraint(tShipOpt,rep(1,4), ">=",130,c(4,8,11,14))

RowNames <- c("m","d","n","c","l","b")
dimnames(tShipOpt) <- list(RowNames, ColNames)

#write to text file
write.lp(tShipOpt,'tShipOpt.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(tShipOpt)

#this return the proposed solution
get.objective(tShipOpt)
x=get.variables(tShipOpt)

#lhs of constraints
get.constraints(tShipOpt)

#sensitivity:
d=get.sensitivity.rhs(tShipOpt)
c=get.sensitivity.obj(tShipOpt)


#Assignment
library(lpSolveAPI)

cost=matrix(c(14,5,8,7,2,12,6,5,7,8,3,9,2,4,6,10),4,4,byrow=TRUE)

assignOpt<-make.lp(0,16)

#set objective coefficients
set.objfn(assignOpt, as.vector(t(cost)))

#set objective direction
lp.control(assignOpt,sense='min')

add.constraint(assignOpt,rep(1,4), "=",1,c(1:4))
add.constraint(assignOpt,rep(1,4), "=",1,c(5:8))
add.constraint(assignOpt,rep(1,4), "=",1,c(9:12))
add.constraint(assignOpt,rep(1,4), "=",1,c(13:16))


add.constraint(assignOpt,rep(1,4), "=",1,c(1,5,9,13))
add.constraint(assignOpt,rep(1,4), "=",1,c(1,5,9,13)+1)
add.constraint(assignOpt,rep(1,4), "=",1,c(1,5,9,13)+2)
add.constraint(assignOpt,rep(1,4), "=",1,c(1,5,9,13)+3)


ColNames <- c("x11","x12","x13","x14","x21","x22","x23","x24","x31","x32","x33","x34","x41","x42","x43","x44")
RowNames <- c("machine1","machine2","machine3","machine4","job1","job2","job3","job4")
dimnames(assignOpt) <- list(RowNames, ColNames)


set.type(lanProj, c(1:16), "binary")

#write to text file
write.lp(assignOpt,'assignOpt.lp',type='lp')

#solve the model, if this return 0 an optimal solution is found
solve(assignOpt)

#this return the proposed solution
get.objective(assignOpt)
x=get.variables(assignOpt)

#lhs of constraints
get.constraints(assignOpt)

#sensitivity:
d=get.sensitivity.rhs(assignOpt)
c=get.sensitivity.obj(assignOpt)

#

#CPM
library(lpSolveAPI)

#define the cost matrix
l=matrix(0,16,16)
l[1,2]=10
l[2,3]=l[2,4]=6
l[3,5]=l[3,7]=6
l[4,6]=l[4,7]=12
l[5,6]=4
l[6,8]=l[6,9]=3
l[7,10]=6
l[8,12]=12
l[9,11]=3
l[10,13]=14
l[11,12]=4
l[12,14]=l[12,13]=3
l[13,15]=8
l[14,15]=12
l[15,16]=4

lanProj<-make.lp(0,16*16)

#set objective coefficients
set.objfn(lanProj, as.vector(t(l)))

#set objective direction
lp.control(lanProj,sense='max')

nodes=c(1:16)

rhs=c(1,rep(0,14),-1)

for (n in 1:16){
  
  # for each node, we are assebling "Outputs - Inputs = 0" (or +1 for start node and -1 for end node)
  #    on the LHS it is easier to sum over all nodes with only the connected nodes having non-zero coefficients
  #    so we do an is.finite(l_ij/l_ij)... this will return a 1 if i and j are connected and a 0 if not.
  
  coef=c(l[n,1:16]/l[n,1:16],-l[1:16,n]/l[1:16,n])
  ind=c((n-1)*16+c(1:16),(c(1:16)-1)*16+n)
  nz=is.finite(coef)
  add.constraint(lanProj,coef[nz], "=",rhs[n],ind[nz])               
}

#name the rows and columns and write the lp to a text file
ColNames = c()
RowNames = c()
for(i in 1:16){
  for(j in 1:16){
    ColNames = cbind(ColNames,paste("x",i,",",j, sep=""))
  }
  RowNames=cbind(RowNames,paste("node",i))
}
dimnames(lanProj) <- list(RowNames, ColNames)
set.type(lanProj, c(1:256), "binary")
write.lp(lanProj,'lanProj.lp',type='lp')


#solve the model, if this return 0 an optimal solution is found
status=solve(lanProj)

#this return the proposed solution
z=get.objective(lanProj)
x=get.variables(lanProj)


#########################
#5.Simulation

#a.queue
N=5000

#tau = sampled from 1 to 15
tau = sample(15,N-1,replace=TRUE)
#S = sampled from (5,10) with prob 0.7 and 0.3 respectively
S= sample(c(5,10),N,prob=c(0.7,0.3),replace=TRUE)  

A = c(0,cumsum(tau));
T = rep(NA,N)
D = rep(NA,N)
W = rep(NA,N)

T[1] = 0
D[1] = S[1]
W[1] = 0

for (i in 2:N){
  T[i] = max(D[i-1],A[i])
  D[i] = T[i] + S[i]
  W[i] = T[i] - A[i]
}

AvgWaitTimes = mean(W)
PercentAnnoyed = mean(W>20)*100

#b.Montyhall
#no. of simulations
N=1000

#Door label list - you can even use: doors = c('A','B','C')
doors = c(1:3)

stay = rep(NA,N)
swit = rep(NA,N)

for (i in 1:N){
  
  #Generate Random Variables
  car = doors[ceiling(runif(1)*length(doors))]
  pick = doors[ceiling(runif(1)*length(doors))]
  
  #Run the Simulation - that is, play the game
  hostChoices  = setdiff(doors,union(pick,car))
  host = hostChoices[ceiling(runif(1)*length(hostChoices))]
  
  switchedDoor = setdiff(doors,union(pick,host))
  
  #remember output - that is, win or not for each strategy
  stay[i] = (pick==car) 
  swit[i] = (switchedDoor==car)
  
}

#summarize output
print(paste("Prob. of winning if we do not switch doors:", mean(stay)))
print(paste("Prob. of winning if we switch doors       :", mean(swit)))

#########################

#6.Dynamic Programming

price = 1
M=100
delta = 0.9
T=15

sValues = seq(0,M)
tValues = seq(0,T)

sN=length(sValues)
tN=length(tValues)

V=matrix(NA,sN,tN)
U=matrix(NA,sN,tN)
rownames(V) = sValues # add rownames and colnames to the V matrix
colnames(V) = tValues
U = V # add rownames and colnames to U matrix

for (t in rev(tValues)){
  for (s in sValues){
    
    if(t==tValues[tN]){
      V[paste(s),paste(t)]=0
      U[paste(s),paste(t)]=0
    }
    else{
      X = seq(0,s)
      valueChoices= price*X - (X^2)/(1+s) + delta*V[paste(s-X),paste(t+1)]
      
      V[paste(s),paste(t)]=max(valueChoices)
      U[paste(s),paste(t)]=X[which.max(valueChoices)]
    }
  }
}

s=100
for (t in (1:15)){
  print(paste("Year:", t , " Init Ore:",s, " Mine:", U[paste(s),paste(t)]))
  s=s-(U[paste(s),paste(t)])
}



