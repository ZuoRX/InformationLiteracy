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


getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

t1<-Sys.time()


Exp5_data<-read.csv("MC_Exp5_ratio_theta.csv")%>% 
  mutate(order=1:nrow(.)) %>% 
  select(order,everything())


#1. rho_A
df<-Exp5_data[,c(2,3,5)] %>%
  purrr::set_names("ratio","theta","rho_A")

ggplot(df,aes(x=ratio,y=theta,fill=rho_A))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7))+
  theme_few()+
  scale_x_continuous(limits = c(0,1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  scale_y_continuous(limits = c(0.5,1),
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  xlab("ratio")+
  ylab(expression(theta))+
  guides(fill=guide_colorbar(title= expression(rho^A),title.hjust =  .2))

ggsave("exp5rhoA.png",width=17, height=8,units="cm",dpi = 300)



#2. rho_I
df<-Exp5_data[,c(2,3,6)] %>%
  purrr::set_names("ratio","theta","rho_I")

ggplot(df,aes(x=ratio,y=theta,fill=rho_I))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7))+
  theme_few()+
  scale_x_continuous(limits = c(0,1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  scale_y_continuous(limits = c(0.5,1),
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  xlab("ratio")+
  ylab(expression(theta))+
  guides(fill=guide_colorbar(title= expression(rho^I),title.hjust =  .2))

ggsave("exp5rhoI.png",width=17, height=8,units="cm",dpi = 300)



#3. rho_Ah
df<-Exp5_data[,c(2,3,7)] %>%
  purrr::set_names("ratio","theta","rho_Ah")

ggplot(df,aes(x=ratio,y=theta,fill=rho_Ah))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7))+
  theme_few()+
  scale_x_continuous(limits = c(0,1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  scale_y_continuous(limits = c(0.5,1),
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  xlab("ratio")+
  ylab(expression(theta))+
  guides(fill=guide_colorbar(title= expression(rho^Ah),title.hjust =  .2))

ggsave("exp5rhoAh.png",width=17, height=8,units="cm",dpi = 300)



#4. rho_Ih
df<-Exp5_data[,c(2,3,8)] %>%
  purrr::set_names("ratio","theta","rho_Ih")

ggplot(df,aes(x=ratio,y=theta,fill=rho_Ih))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7))+
  theme_few()+
  scale_x_continuous(limits = c(0,1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  scale_y_continuous(limits = c(0.5,1),
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  xlab("ratio")+
  ylab(expression(theta))+
  guides(fill=guide_colorbar(title= expression(rho^Ih),title.hjust =  .2))

ggsave("exp5rhoIh.png",width=17, height=8,units="cm",dpi = 300)



#5. rho_Al
df<-Exp5_data[,c(2,3,9)] %>%
  purrr::set_names("ratio","theta","rho_Al")

ggplot(df,aes(x=ratio,y=theta,fill=rho_Al))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7))+
  theme_few()+
  scale_x_continuous(limits = c(0,1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  scale_y_continuous(limits = c(0.5,1),
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  xlab("ratio")+
  ylab(expression(theta))+
  guides(fill=guide_colorbar(title= expression(rho^Al),title.hjust =  .2))

ggsave("exp5rhoAl.png",width=17, height=8,units="cm",dpi = 300)


#6. rho_Il
df<-Exp5_data[,c(2,3,9)] %>%
  purrr::set_names("ratio","theta","rho_Il")

ggplot(df,aes(x=ratio,y=theta,fill=rho_Il))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7))+
  theme_few()+
  scale_x_continuous(limits = c(0,1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  scale_y_continuous(limits = c(0.5,1),
                     breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  xlab("ratio")+
  ylab(expression(theta))+
  guides(fill=guide_colorbar(title= expression(rho^Il),title.hjust =  .2))

ggsave("exp5rhoIl.png",width=17, height=8,units="cm",dpi = 300)


t2<-Sys.time()
t2
t1;t2;t2-t1 














