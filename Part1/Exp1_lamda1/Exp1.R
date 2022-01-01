#Dynamical Interplay between Awareness and Epidemic Spreading in Multiplex Networks文章复现
suppressPackageStartupMessages(suppressWarnings({
  library(igraph)
  library(progress)
  library(magrittr)
  library(tidyverse)
  library(ggthemes)
  library(RColorBrewer)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(tidyr)
  library(ggplot2)
}))


setwd("/home/zuo_r/zuo")
source("MMCA_InforLiteracy_custom_function.R")
t1<-Sys.time()


#----------------------------#
#---1.Fig3数据收集与可视化---#
#----------------------------#

#1.1 基础参数设置，构建网络
N<-1000      #选个2的倍数

#四个自定义参数
theta<-0.8   #信息素养参数
alpha<-0.8   #意识转化为行为的概率参数
#调节lamda1:0.1\0.2\0.3\0.4
lamda1<-0.4  #媒体信息转化为意识的转换效率参数
lamda2<-0.15  #邻居再传播信息转化为意识的概率参数


#beta_U      #无意识条件下被感染的概率
mu<-0.4      #康复的概率
delta<-0.6   #遗忘意识的概率
gamma<-0.2   #有意识条件下，降低被感染风险的调节参数


#打乱顺序
#1-->high；2-->low 


set.seed(111)
pl<-barabasi.game(N, power = 2.5,m=5, directed = FALSE)   
B<-get.adjacency(pl, type="both",sparse=FALSE)

pl1<-barabasi.game(N, power = 2.5,m=5, directed = FALSE)   
A_temp<-get.adjacency(pl1, type="both",sparse=FALSE)

add_graph<-erdos.renyi.game(N,p=0.0004)     
C_temp<-get.adjacency(add_graph, sparse=FALSE)

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
    ter<-100
    N<-1000 
    ratio<-0.5 #高信息素养节点比例值
    set.seed(111)
    n<-sample(c(rep(1,N*ratio),rep(2,N*(1-ratio))),N,replace = F)
    rho_A<-1:ter
    rho_I<-1:ter
    rho_Ah<-1:ter
    rho_Ih<-1:ter
    rho_Al<-1:ter
    rho_Il<-1:ter
  }) 
  fig3_MMCA_data <- foreach(
    l=1:100,          #输入等待请求的参数
    .combine=rbind   #返回结果的整合
    #.packages = c("igraph") 
    #多个进程共享的系统环境
  ) %dopar% get_rhoA_rhoI_data(l)
  stopCluster(cl)
})




#1.3 可视化 visualization
#ter<-100


MMCA_fig3<-fig3_MMCA_data[,-1] %>% 
  purrr::set_names("beta_U","rho_A","rho_I","rho_Ah","rho_Ih","rho_Al","rho_Il")
  
write.csv(MMCA_fig3,"Exp1_lamda1/MMCA_lamda1_04.csv",row.names = F)


# df<-MMCA_fig3[,c(1,2,4,6)] %>% 
#   gather("item",value,-1)
# 
# ggplot(df,aes(x=beta_U,y=value,color=item))+
#   geom_point()+
#   theme_few()+
#   #ylim(0,1)+
#   xlab(expression(beta^U))+
#   ylab("Aware Ratio")+
#   scale_colour_manual(values = c("#1F78B4", "#E31A1C","#33A02C"),
#           labels = c(expression(rho^A),expression(rho^Ah),expression(rho^Al)))+
#   theme(legend.title=element_blank(),
#         axis.line = element_line(colour = "black")) 
# #brewer.pal(9,"Paired") %>% scales::show_col()
# 
# ggsave("Exp1_lamda1/betaU_fig3_MMCA.png",width=15, height=15,units="cm",dpi = 600)
# 
# df1<-MMCA_fig3[,c(1,3,5,7)]%>% 
#   gather("item",value,-1)
# 
# ggplot(df1,aes(x=beta_U,y=value,color=item))+
#   geom_point()+
#   theme_few()+
#   #ylim(0,1)+
#   xlab(expression(beta^U))+
#   ylab("Infected Ratio")+
#   scale_colour_manual(values = c("darkblue", "red","green"),
#                       labels = c(expression(rho^I),expression(rho^Ih),expression(rho^Il)))+
#   theme(legend.title=element_blank(),
#         axis.line = element_line(colour = "black")) 
# 
# 
# ggsave("Exp1_lamda1/rhoI_betaU_fig3_MMCA.png",width=15, height=15,units="cm",dpi = 600)
# 

t2<-Sys.time()
t2-t1 


