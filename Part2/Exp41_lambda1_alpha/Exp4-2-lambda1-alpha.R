#Dynamical Interplay between Awareness and Epidemic Spreading in Multiplex Networks文章复现
suppressPackageStartupMessages(suppressWarnings({
  library(igraph)
  library(progress)
  library(magrittr)
  library(tidyverse)
  library(RColorBrewer)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(tidyr)
  library(poweRlaw)
}))

minmaxscale<-function(a){
  center <- sweep(a, 2, apply(a, 2, min),'-') 
  R <- apply(a, 2, max) - apply(a,2,min)   
  k<-(sweep(center, 2, R, "/")) 
  return(0.999*k+0.001)
}

setwd("c:/users/sim509/desktop/zuo1/zuo")
source("MMCA_InforLiteracy_custom_function-2.R")

t1<-Sys.time()

#----------------------------#
#---1.Fig4数据收集与可视化---#
#----------------------------#

#1.1 基础参数设置，构建网络
N<-1000      #选个2的倍数

#四个自定义参数
set.seed(0808)
theta<-rpldis(N,xmin=1,alpha = 2.0) %>% 
  data.frame() %>% 
  minmaxscale() %>% 
  unlist() %>% 
  as.numeric()

#确定4/5位数的解释比例(未标准化前)
# set.seed(0808)
# theta<-rpldis(N,xmin=1,alpha = 2.0)
explaination<-sum(theta[theta>=quantile(theta,probs = seq(0, 1, 0.2))[5]])/sum(theta)

#alpha<-0.8   #意识转化为行为的概率参数
#beta_U<-
#lamda1<-0.15  #媒体信息转化为意识的转换效率参数
lamda2<-0.15  #邻居再传播信息转化为意识的概率参数


#beta_U      #无意识条件下被感染的概率
mu<-0.4      #康复的概率
delta<-0.6   #遗忘意识的概率
gamma<-0.2   #有意识条件下，降低被感染风险的调节参数


#打乱顺序
#1-->high；2-->low 


set.seed(111)
pl<-barabasi.game(N, power = 2.5,m=5, directed = FALSE)   
add_graph<-erdos.renyi.game(N,p=0.0004)     
B<-get.adjacency(pl, type="both",sparse=FALSE)
C_temp<-get.adjacency(add_graph, sparse=FALSE)
A<-B+C_temp 
diag(A)<-0
diag(B)<-0
A[A==2]<-1

#查看本地核数
n_core<-detectCores(logical = F)

#1.2 进行多线程运算
system.time({
  cl<- makeCluster(n_core)      
  registerDoParallel(cl)       #进行进程注册
  clusterEvalQ(cl, {
    ter<-50
    N<-1000
    #ratio<-0.5 #高信息素养节点比例值
    beta_U<-0.2
    rho_A<-1:ter
    rho_I<-1:ter
    # rho_Ah<-1:ter
    # rho_Ih<-1:ter
    # rho_Al<-1:ter
    # rho_Il<-1:ter
    temp2<-data.frame()
  }) 
  fig4_MMCA_data <- foreach(
    l1=1:50,          #输入等待请求的参数
    .combine=rbind   #返回结果的整合
    #.packages = c("igraph") 
    #多个进程共享的系统环境
  ) %dopar% get_rhoIA_by_lambda1_alpha(l1)
  stopCluster(cl)
})



write.csv(fig4_MMCA_data,"Exp41_lambda1_alpha/MMCA2_Exp4_lambda1_alpha.csv",row.names = F)


t2<-Sys.time()
t1
t2
t2-t1 







