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


setwd("/home/zuo/zuo/Exp3_alpha")

alpha06<-read.csv("alpha06/MMCA_fig3.csv") %>% 
  mutate(parameter=rep("alpha=0.6",100))

alpha07<-read.csv("alpha07/MMCA_fig3.csv") %>% 
  mutate(parameter=rep("alpha=0.7",100))

alpha08<-read.csv("alpha08/MMCA_fig3.csv") %>% 
  mutate(parameter=rep("alpha=0.8",100))

alpha09<-read.csv("alpha09/MMCA_fig3.csv") %>% 
  mutate(parameter=rep("alpha=0.9",100))

data<-rbind(alpha06,alpha07,alpha08,alpha09)

remove(alpha06,alpha07,alpha09,alpha08)

#1. rho_Ah
df<-data[,c(1,4,8)]

ggplot(df,aes(x=beta_U,y=rho_Ah,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^Ah))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3),
        axis.line = element_line(colour = "black")) 

ggsave("rhoAh.png",width=12, height=8,units="cm",dpi = 600)


#2. rho_Al
df<-data[,c(1,6,8)]

ggplot(df,aes(x=beta_U,y=rho_Al,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^Al))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3),
        axis.line = element_line(colour = "black")) 

ggsave("rhoAl.png",width=12, height=8,units="cm",dpi = 600)



#3. rho_Ih
df<-data[,c(1,5,8)]

ggplot(df,aes(x=beta_U,y=rho_Ih,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^Ih))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3),
        axis.line = element_line(colour = "black")) 

ggsave("rhoIh.png",width=12, height=8,units="cm",dpi = 600)



#4. rho_Il
df<-data[,c(1,7,8)]

ggplot(df,aes(x=beta_U,y=rho_Il,color=parameter))+
  geom_point()+
  theme_few()+
  #ylim(0,1)+
  xlab(expression(beta^U))+
  ylab(expression(rho^Il))+
  theme(legend.title=element_blank(),
        legend.position = c(0.8,0.3),
        axis.line = element_line(colour = "black")) 

ggsave("rhoIl.png",width=12, height=8,units="cm",dpi = 600)














