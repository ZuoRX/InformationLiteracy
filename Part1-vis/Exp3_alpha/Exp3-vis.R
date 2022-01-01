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

Exp3_data<-read.csv("Exp3_beta_alpha.csv")

#1. rho_A
df<-Exp3_data[,c(2,3,4)] %>%
  purrr::set_names("alpha","beta_U","rho_A")

ggplot(df,aes(x=beta_U,y=alpha,fill=rho_A))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(n=7,start=min(df$rho_A),end=max(df$rho_A)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(beta^U))+
  guides(fill=guide_colorbar(title= expression(rho^A),title.hjust =  .2))

ggsave("exp3rhoA.png",width=15, height=13,units="cm",dpi = 300)



#2. rho_I
df<-Exp3_data[,c(2,3,5)] %>%
  purrr::set_names("alpha","beta_U","rho_I")

ggplot(df,aes(y=alpha,x=beta_U,fill=rho_I))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_I),end=max(df$rho_I)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(beta^U))+
  guides(fill=guide_colorbar(title= expression(rho^I),title.hjust =  .2))

ggsave("exp3rhoI.png",width=15, height=13,units="cm",dpi = 600)



#3. rho_Ah
df<-Exp3_data[,c(2,3,6)] %>%
  purrr::set_names("alpha","beta_U","rho_Ah")

ggplot(df,aes(y=alpha,x=beta_U,fill=rho_Ah))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Ah),end=max(df$rho_Ah)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(beta^U))+
  guides(fill=guide_colorbar(title= expression(rho^Ah),title.hjust =  .2))

ggsave("exp3rhoAh.png",width=15, height=13,units="cm",dpi = 600)



#4. rho_Ih
df<-Exp3_data[,c(2,3,7)] %>%
  purrr::set_names("alpha","beta_U","rho_Ih")

ggplot(df,aes(y=alpha,x=beta_U,fill=rho_Ih))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Ih),end=max(df$rho_Ih)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(beta^U))+
  guides(fill=guide_colorbar(title= expression(rho^Ih),title.hjust =  .2))

ggsave("exp3rhoIh.png",width=15, height=13,units="cm",dpi = 600)



#5. rho_Al
df<-Exp3_data[,c(2,3,8)] %>%
  purrr::set_names("alpha","beta_U","rho_Al")

ggplot(df,aes(y=alpha,x=beta_U,fill=rho_Al))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Al),end=max(df$rho_Al)))+
  theme_few()+
  ylab(expression(alpha))+
  xlab(expression(beta^U))+
  guides(fill=guide_colorbar(title= expression(rho^Al),title.hjust =  .2))

ggsave("exp3rhoAl.png",width=15, height=13,units="cm",dpi = 600)



#6. rho_Il
df<-Exp3_data[,c(2,3,9)] %>%
  purrr::set_names("alpha","beta_U","rho_Il")

ggplot(df,aes(y=alpha,x=beta_U,fill=rho_Il))+
  geom_raster() +
  scale_fill_gradientn(colours =rainbow(7,start=min(df$rho_Il),end=max(df$rho_Il)))+
  theme_few()+
  # scale_x_continuous(limits = c(0,0.5),
  #                    breaks = c(0,0.1,0.2,0.3,0.4,0.5))+
  # scale_y_continuous(limits = c(0.5,1),
  #                    breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  ylab(expression(alpha))+
  xlab(expression(beta^U))+
  guides(fill=guide_colorbar(title= expression(rho^Il),title.hjust =  .2))

ggsave("exp3rhoIl.png",width=15, height=13,units="cm",dpi = 600)


t2<-Sys.time()
t1;t2;t2-t1 



















