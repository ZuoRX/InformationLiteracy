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

t1<-Sys.time()


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

Exp4_data<-read.csv("MC2_Exp41_lamda1_alpha.csv")%>% 
  mutate(order=1:nrow(.)) %>% 
  select(order,everything())

#1. rho_A
df<-Exp4_data[,c(2,3,4)] %>%
  purrr::set_names("alpha","lambda1","rho_A")

ggplot(df,aes(x=lambda1,y=alpha,fill=rho_A))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_A),end=max(df$rho_A)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(lambda[1]))+
  guides(fill=guide_colorbar(title= expression(rho^A),title.hjust =  .2))

ggsave("p2_exp41rhoA.png",width=15, height=13,units="cm",dpi = 600)



#2. rho_I
df<-Exp4_data[,c(2,3,5)] %>%
  purrr::set_names("alpha","lambda1","rho_I")

ggplot(df,aes(y=alpha,x=lambda1,fill=rho_I))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(lambda[1]))+
  guides(fill=guide_colorbar(title= expression(rho^I),title.hjust =  .2))

ggsave("p2_exp41rhoI.png",width=15, height=13,units="cm",dpi = 600)

t2<-Sys.time()
t2
t2-t1 



















