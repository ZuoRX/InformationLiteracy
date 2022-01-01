suppressPackageStartupMessages(suppressWarnings({
  library(igraph)
  library(progress)
  library(tidyverse)
  library(ggthemes)
  library(RColorBrewer)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(data.table)
}))

# 主机pu
setwd("D:/zuo/AAA-Revision/Part3")

# #服务器
# setwd("/home/zuo_r/zuo/AAA-Revision/Part3")
# #主机
# setwd("C:/Users/sim509/Desktop/zuo1/AAA-Reivsion/Part3")
source("MC_InforLiteracy_custom_function.R")


t1<-Sys.time()

#-----------------四个自定义参数-----------------#
theta<-0.8   #信息素养参数
alpha<-0.8   #意识转化为行为的概率参数
#lamda1<-0.15  #媒体信息转化为意识的转换效率参数
lamda2<-0.15  #邻居再传播信息转化为意识的概率参数
#------------------------------------------------#


#------------基础参数设置，构建网络--------------#
N<-1000
beta_U<-0.2      #无意识条件下被感染的概率
mu<-0.4      #康复的概率
delta<-0.6   #遗忘意识的概率
gamma<-0.2   #有意识条件下，降低被感染风险的调节参数
#------------------------------------------------#


set.seed(111)
pl<-barabasi.game(N, power = 2.5,m=5, directed = FALSE)   
#物理层邻接矩阵
B<-get.adjacency(pl, type="both",sparse=FALSE)

pl1<-barabasi.game(N, power = 2.5,m=5, directed = FALSE)   
A_temp<-get.adjacency(pl1, type="both",sparse=FALSE)

add_graph<-erdos.renyi.game(N,p=0.0004)  
C_temp<-get.adjacency(add_graph, sparse=FALSE)

#虚拟层邻接矩阵
A<-A_temp+C_temp 

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
    stp<-10 
    N<-1000
    termi<-50
    ratio<-0.5 #高信息素养节点比例值
    set.seed(111)
    infor<-sample(c(rep(1,ceiling(N*ratio)),rep(2,ceiling(N*(1-ratio)))),N,replace = F)
    rho_A<-1:termi
    rho_I<-1:termi
    rho_Ah<-1:termi
    rho_Ih<-1:termi
    rho_Al<-1:termi
    rho_Il<-1:termi
    temp2<-data.frame()
  }) 
  MC_data <- foreach(
    Repeat=1:20,          #输入等待请求的参数
    .combine=rbind,  #返回结果的整合
    .packages = c("magrittr","purrr")
  ) %dopar% get_Exp41_MC_lamda1_alpha(Repeat)
  stopCluster(cl)
})

MC_exp41_data<-MC_data %>% 
  data.table() %>% 
  purrr::set_names("Repeat","alpha","lamda1","rho_A","rho_I","rho_Ah","rho_Ih","rho_Al","rho_Il") %>% 
  .[,.(rho_A=mean(rho_A),rho_I=mean(rho_I),
       rho_Ah=mean(rho_Ah),rho_Ih=mean(rho_Ih),
       rho_Al=mean(rho_Al),rho_Il=mean(rho_Il)),by=c("alpha","lamda1")]
   
write.csv(MC_data,"Exp41_lambda1_alpha/MC_Exp41.csv",row.names = F)
write.csv(MC_exp41_data,"Exp41_lambda1_alpha/MC_Exp41_lamda1_alpha.csv",row.names = F)

t2<-Sys.time()
t1;t2;t2-t1
