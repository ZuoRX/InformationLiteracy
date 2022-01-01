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


setwd("c:/users/sim509/desktop/zuo1/zuo")
source("MMCA_InforLiteracy_custom_function.R")

t1<-Sys.time()

#----------------------------#
#---1.Fig4数据收集与可视化---#
#----------------------------#

#1.1 基础参数设置，构建网络
N<-1000      #选个2的倍数

#四个自定义参数
theta<-0.8   #信息素养参数
#alpha<-0.8   #意识转化为行为的概率参数
#beta_U<-
lamda1<-0.15  #媒体信息转化为意识的转换效率参数
#lamda2<-0.15  #邻居再传播信息转化为意识的概率参数


#beta_U      #无意识条件下被感染的概率
mu<-0.4      #康复的概率
delta<-0.6   #遗忘意识的概率
gamma<-0.2   #有意识条件下，降低被感染风险的调节参数


#打乱顺序
#1-->high；2-->low 


set.seed(111)
pl<-barabasi.game(N, power = 2.5,m=5, directed = FALSE)   
B<-get.adjacency(pl, type="both",sparse=FALSE)

add_graph<-erdos.renyi.game(N,p=0.0004)     
C_temp<-get.adjacency(add_graph, sparse=FALSE)

pl1<-barabasi.game(N, power = 2.5,m=5, directed = FALSE)   
A_temp<-get.adjacency(pl1, type="both",sparse=FALSE)

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
    ter<-50
    N<-1000
    ratio<-0.5 #高信息素养节点比例值
    beta_U<-0.2
    rho_A<-1:ter
    rho_I<-1:ter
    rho_Ah<-1:ter
    rho_Ih<-1:ter
    rho_Al<-1:ter
    rho_Il<-1:ter
    temp2<-data.frame()
  }) 
  fig4_MMCA_data <- foreach(
    l1=1:50,          #输入等待请求的参数
    .combine=rbind   #返回结果的整合
    #.packages = c("igraph") 
    #多个进程共享的系统环境
  ) %dopar% get_rhoIA_by_lambda2_alpha(l1)
  stopCluster(cl)
})




write.csv(fig4_MMCA_data,"Exp42_lambda2_alpha/Exp4_lambda2_alpha.csv",row.names = F)



#fig4_MMCA_data<-read.csv("Exp42_lambda2_alpha/Exp4_lambda2_alpha.csv")

# #1. rho_A
# df<-fig4_MMCA_data[,c(2,3,4)] %>% 
#   purrr::set_names("alpha","lambda2","rho_A")
# 
# ggplot(df,aes(x=lambda2,y=alpha,fill=rho_A))+
#   geom_raster() +
#   scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_A),end=max(df$rho_A)))+
#   theme_few()+
#   ylab(expression(alpha))+
#   xlab(expression(lambda[2]))+
#   guides(fill=guide_colorbar(title= expression(rho^A),title.hjust =  .2))
# 
# ggsave("exp42rhoA.png",width=15, height=13,units="cm",dpi = 600)
# 
# #3. rho_Ah
# df<-fig4_MMCA_data[,c(2,3,6)] %>% 
#   purrr::set_names("alpha","lambda2","rho_Ah")
# 
# ggplot(df,aes(y=alpha,x=lambda2,fill=rho_Ah))+
#   geom_raster() +
#   scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Ah),end=max(df$rho_Ah)))+
#   theme_few()+
#   ylab(expression(alpha))+
#   xlab(expression(lambda[2]))+
#   guides(fill=guide_colorbar(title= expression(rho^Ah),title.hjust =  .2))
# 
# ggsave("exp42rhoAh.png",width=15, height=13,units="cm",dpi = 600)
# 
# #5. rho_Al
# df<-fig4_MMCA_data[,c(2,3,8)] %>% 
#   purrr::set_names("alpha","lambda2","rho_Al")
# 
# ggplot(df,aes(y=alpha,x=lambda2,fill=rho_Al))+
#   geom_raster() +
#   scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Al),end=max(df$rho_Al)))+
#   theme_few()+
#   ylab(expression(alpha))+
#   xlab(expression(lambda[2]))+
#   guides(fill=guide_colorbar(title= expression(rho^Al),title.hjust =  .2))
# 
# ggsave("exp42rhoAl.png",width=15, height=13,units="cm",dpi = 600)
# 
# 
# #--------------------------------------------------------------------------------------------#
# temp<-fig4_MMCA_data[,c(2,3,5,7,9)]
# apply(temp, 2, min)
# apply(temp, 2, max)
# 
# #2. rho_I
# df<-fig4_MMCA_data[,c(2,3,5)] %>% 
#   purrr::set_names("alpha","lambda2","rho_I")
# 
# ggplot(df,aes(y=alpha,x=lambda2,fill=rho_I))+
#   geom_raster() +
#   scale_fill_gradientn(colours =rainbow(7,start=(min(df$rho_I)-0.2064)/(0.5749584-0.2064126),end=1))+
#   theme_few()+
#   ylab(expression(alpha))+
#   xlab(expression(lambda[2]))+
#   guides(fill=guide_colorbar(title= expression(rho^I),title.hjust =  .2))
# 
# ggsave("exp42rhoI.png",width=15, height=13,units="cm",dpi = 600)
# 
# 
# #4. rho_Ih
# df<-fig4_MMCA_data[,c(2,3,7)] %>% 
#   purrr::set_names("alpha","lambda2","rho_Ih")
# 
# ggplot(df,aes(y=alpha,x=lambda2,fill=rho_Ih))+
#   geom_raster() +
#   scale_fill_gradientn(colours =rainbow(7,start=(min(df$rho_Ih)-0.2064)/(0.5749584-0.2064126),end=(max(df$rho_Ih)-0.2064)/(0.5749584-0.2064126)))+
#   theme_few()+
#   ylab(expression(alpha))+
#   xlab(expression(lambda[2]))+
#   guides(fill=guide_colorbar(title= expression(rho^Ih),title.hjust =  .2))
# 
# ggsave("exp42rhoIh.png",width=15, height=13,units="cm",dpi = 600)
# 
# 
# 
# #6. rho_Il
# df<-fig4_MMCA_data[,c(2,3,9)] %>% 
#   purrr::set_names("alpha","lambda2","rho_Il")
# 
# ggplot(df,aes(y=alpha,x=lambda2,fill=rho_Il))+
#   geom_raster() +
#   scale_fill_gradientn(colours =rainbow(7,start=(min(df$rho_Il)-0.2064)/(0.5749584-0.2064126),end=(max(df$rho_Il)-0.2064)/(0.5749584-0.2064126)))+
#   theme_few()+
#   # scale_x_continuous(limits = c(0,0.5),
#   #                    breaks = c(0,0.1,0.2,0.3,0.4,0.5))+
#   # scale_y_continuous(limits = c(0.5,1),
#   #                    breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
#   ylab(expression(alpha))+
#   xlab(expression(lambda[2]))+
#   guides(fill=guide_colorbar(title= expression(rho^Il),title.hjust =  .2))
# 
# ggsave("exp42rhoIl.png",width=15, height=13,units="cm",dpi = 600)


t2<-Sys.time()
t2-t1 



















