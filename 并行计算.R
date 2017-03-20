library(foreach)
library(parallel)
library(iterators)
library(doParallel)

#构建实验数据集
n=10000
p=50
X=matrix(rnorm(n*p),nrow=n)
beta=matrix(runif(p,10,20),ncol=1)
y=X%*%beta+rnorm(n)

#最小二乘估计参数
myfun=function(X,y,lambda=0.001){
  p=ncol(X)
  n=nrow(X)
  lam=diag(p)*lambda
  beta_hat=solve(t(X)%*%X+lam)%*%t(X)%*%y
  y_hat=X%*%beta_hat
  y_bar=mean(y)
  R_squre=sum((y_hat-y_bar)^2)/sum((y-y_bar)^2)
  return(list(coef=beta_hat,R_squre=R_squre))
}
ols=myfun(X,y)
ols$R_squre
data.frame(beta,beta_hat=ols$coef)

myfun=function(X,y,lambda=0.001){
  p=ncol(X)
  n=nrow(X)
  lam=diag(p)*lambda
  beta_hat=solve(t(X)%*%X+lam)%*%t(X)%*%y
  y_hat=X%*%beta_hat
  y_bar=mean(y)
  R_squre=sum((y_hat-y_bar)^2)/sum((y-y_bar)^2)
  return()
}

#如果使用for循环做100次最小二乘
t1=system.time(for(i in 1:100) myfun(X,y))
t1=as.numeric(t1)[1:3]
t1

#使用foreach进行100次最小二乘
t2=system.time(foreach(x=1:100,.combine='c') %do% myfun(X,y))
t2=as.numeric(t2)[1:3]
t2

#用doParallel使用两个核作为foreach计算的后端进行100次最小二乘
cl=makeCluster(2)
registerDoParallel(cl)
t3=system.time(foreach(x=1:100,.combine='c') %dopar% myfun(X,y))
stopCluster(cl)
t3=as.numeric(t3)[1:3]
t3

#如果使用apply进行100次最小二乘
t4=system.time(sapply(1:100,function(x)myfun(X,y)))
t4=as.numeric(t4)[1:3]
t4

#使用parallel调用两个核再使用apply函数类进行100次最小二乘
cl=makeCluster(2)
clusterExport(cl, list("myfun","X","y"), envir = environment())
t5=system.time(parSapply(cl,1:100,function(x)myfun(X,y)))
stopCluster(cl)
t5=as.numeric(t5)[1:3]
t5

library(ggplot2)
library(sysfonts)
library(showtext)
library(reshape2)
showtext.auto(enable = T)

df=data.frame(rbind(t1,t2,t3,t4,t5))
names(df)=c("用户","系统","流逝")
df$方法=c("for","foreach","foreach \n doParallel","apply","apply \n Parallel")
df2=melt(df)
df2$variable=factor(df2$variable,levels = c("系统","用户","流逝"))
df2$方法=factor(df$方法,levels=c("for","foreach","foreach \n doParallel","apply","apply \n Parallel"))

ph1=ggplot(data=df2,aes(x=方法,y=value,fill=variable))+
  geom_bar(stat="identity",position = "dodge",width = 0.8,size=0.8)+
  scale_fill_brewer(palette = "Greys")+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=18),
        legend.title = element_blank(),
        legend.text = element_text(size=18))
png("时间比较.png",width = 720,height = 450)
print(ph1)
dev.off()
  