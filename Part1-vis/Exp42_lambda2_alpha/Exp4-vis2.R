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


getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

t1<-Sys.time()

Exp42_data<-read.csv("Exp4_lambda2_alpha.csv")

#1. rho_A
df<-Exp42_data[,c(2,3,4)] %>%
  purrr::set_names("alpha","lambda2","rho_A")

ggplot(df,aes(x=lambda2,y=alpha,fill=rho_A))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_A),end=max(df$rho_A)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(lambda[2]))+
  guides(fill=guide_colorbar(title= expression(rho^A),title.hjust =  .2))

ggsave("exp42rhoA.png",width=15, height=13,units="cm",dpi = 600)

#3. rho_Ah
df<-Exp42_data[,c(2,3,6)] %>%
  purrr::set_names("alpha","lambda2","rho_Ah")

ggplot(df,aes(y=alpha,x=lambda2,fill=rho_Ah))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Ah),end=max(df$rho_Ah)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(lambda[2]))+
  guides(fill=guide_colorbar(title= expression(rho^Ah),title.hjust =  .2))

ggsave("exp42rhoAh.png",width=15, height=13,units="cm",dpi = 600)

#5. rho_Al
df<-Exp42_data[,c(2,3,8)] %>%
  purrr::set_names("alpha","lambda2","rho_Al")

ggplot(df,aes(y=alpha,x=lambda2,fill=rho_Al))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Al),end=max(df$rho_Al)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(lambda[2]))+
  guides(fill=guide_colorbar(title= expression(rho^Al),title.hjust =  .2))

ggsave("exp42rhoAl.png",width=15, height=13,units="cm",dpi = 600)


#--------------------------------------------------------------------------------------------#
temp<-Exp42_data[,c(2,3,5,7,9)]
apply(temp, 2, min)
apply(temp, 2, max)

#2. rho_I
df<-Exp42_data[,c(2,3,5)] %>%
  purrr::set_names("alpha","lambda2","rho_I")

ggplot(df,aes(y=alpha,x=lambda2,fill=rho_I))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_I),end=max(df$rho_I)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(lambda[2]))+
  guides(fill=guide_colorbar(title= expression(rho^I),title.hjust =  .2))

ggsave("exp42rhoI.png",width=15, height=13,units="cm",dpi = 600)


#4. rho_Ih
df<-Exp42_data[,c(2,3,7)] %>%
  purrr::set_names("alpha","lambda2","rho_Ih")

ggplot(df,aes(y=alpha,x=lambda2,fill=rho_Ih))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Ih),end=max(df$rho_Ih)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(lambda[2]))+
  guides(fill=guide_colorbar(title= expression(rho^Ih),title.hjust =  .2))

ggsave("exp42rhoIh.png",width=15, height=13,units="cm",dpi = 600)



#6. rho_Il
df<-Exp42_data[,c(2,3,9)] %>%
  purrr::set_names("alpha","lambda2","rho_Il")

ggplot(df,aes(y=alpha,x=lambda2,fill=rho_Il))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=(min(df$rho_Il)-0.2064)/(0.5749584-0.2064126),end=(max(df$rho_Il)-0.2064)/(0.5749584-0.2064126)))+
  theme_few()+
  # scale_x_continuous(limits = c(0,0.5),
  #                    breaks = c(0,0.1,0.2,0.3,0.4,0.5))+
  # scale_y_continuous(limits = c(0.5,1),
  #                    breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  ylab(expression(alpha))+
  xlab(expression(lambda[2]))+
  guides(fill=guide_colorbar(title= expression(rho^Il),title.hjust =  .2))

ggsave("exp42rhoIl.png",width=15, height=13,units="cm",dpi = 600)


t2<-Sys.time()
t1;t2;t2-t1 



















