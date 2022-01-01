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


a<-Sys.time()


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

lamda01<-read.csv("MMCA_lamda2_01.csv") %>% 
  mutate(parameter=rep("lamda2=0.1",100))

lamda02<-read.csv("MMCA_lamda2_02.csv") %>% 
  mutate(parameter=rep("lamda2=0.2",100))

lamda03<-read.csv("MMCA_lamda2_03.csv") %>% 
  mutate(parameter=rep("lamda2=0.3",100))

lamda04<-read.csv("MMCA_lamda2_04.csv") %>% 
  mutate(parameter=rep("lamda2=0.4",100))

data<-rbind(lamda01,lamda02,lamda03,lamda04)

remove(lamda01,lamda02,lamda03,lamda04)


#1. rho_Ah
df<-data[,c(1,4,8)]

ggplot(df,aes(x=beta_U,y=rho_Ah,color=parameter))+
  geom_point(key_glyph=draw_key_point)+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^Ah))+
  scale_color_discrete(labels=c(expression(paste(lambda[2],"=0.1")),
                                expression(paste(lambda[2],"=0.2")),
                                expression(paste(lambda[2],"=0.3")),
                                expression(paste(lambda[2],"=0.4"))))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3)) 

ggsave("exp2rhoAh.png",width=12, height=8,units="cm",dpi = 600)


#2. rho_Al
df<-data[,c(1,6,8)]

ggplot(df,aes(x=beta_U,y=rho_Al,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^Al))+
  scale_color_discrete(labels=c(expression(paste(lambda[2],"=0.1")),
                                expression(paste(lambda[2],"=0.2")),
                                expression(paste(lambda[2],"=0.3")),
                                expression(paste(lambda[2],"=0.4"))))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3)) 

ggsave("exp2rhoAl.png",width=12, height=8,units="cm",dpi = 600)



#3. rho_Ih
df<-data[,c(1,5,8)]

ggplot(df,aes(x=beta_U,y=rho_Ih,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^Ih))+
  scale_color_discrete(labels=c(expression(paste(lambda[2],"=0.1")),
                                expression(paste(lambda[2],"=0.2")),
                                expression(paste(lambda[2],"=0.3")),
                                expression(paste(lambda[2],"=0.4"))))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3)) 

ggsave("exp2rhoIh.png",width=12, height=8,units="cm",dpi = 600)



#4. rho_Il
df<-data[,c(1,7,8)]

ggplot(df,aes(x=beta_U,y=rho_Il,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^Il))+
  scale_color_discrete(labels=c(expression(paste(lambda[2],"=0.1")),
                                expression(paste(lambda[2],"=0.2")),
                                expression(paste(lambda[2],"=0.3")),
                                expression(paste(lambda[2],"=0.4"))))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3)) 

ggsave("exp2rhoIl.png",width=12, height=8,units="cm",dpi = 600)



#5. rho_A
df<-data[,c(1,2,8)]

ggplot(df,aes(x=beta_U,y=rho_A,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^A))+
  scale_color_discrete(labels=c(expression(paste(lambda[2],"=0.1")),
                                expression(paste(lambda[2],"=0.2")),
                                expression(paste(lambda[2],"=0.3")),
                                expression(paste(lambda[2],"=0.4"))))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3)) 

ggsave("exp2rhoA.png",width=12, height=8,units="cm",dpi = 600)


#6. rho_I
df<-data[,c(1,3,8)]

ggplot(df,aes(x=beta_U,y=rho_I,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^I))+
  scale_color_discrete(labels=c(expression(paste(lambda[2],"=0.1")),
                                expression(paste(lambda[2],"=0.2")),
                                expression(paste(lambda[2],"=0.3")),
                                expression(paste(lambda[2],"=0.4"))))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3)) 

ggsave("exp2rhoI.png",width=12, height=8,units="cm",dpi = 600)

b<-Sys.time()
a;b;b-a






