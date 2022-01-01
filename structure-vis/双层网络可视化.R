suppressPackageStartupMessages(suppressWarnings({
  library(tidyverse)
  library(data.table)
  library(tidyr)
  library(ggplot2)
  library(ggthemes)
  library(RColorBrewer)
  library(igraph)
  library(graphlayouts)
  library(ggraph)
  library(threejs)
  library(oaqc)
}))

pryr::mem_used()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


a<-Sys.time()

N<-1000
set.seed(111)
#物理线下网络
g1<-barabasi.game(N, power = 2.5,m=5, directed = FALSE) 
B<-get.adjacency(g1, type="both",sparse=FALSE)

#虚拟线上网络
add_graph<-erdos.renyi.game(N,p=0.0004)  
C_temp<-get.adjacency(add_graph, sparse=FALSE)

pl1<-barabasi.game(N, power = 2.5,m=5, directed = FALSE)   
A_temp<-get.adjacency(pl1, type="both",sparse=FALSE)
A<-A_temp+C_temp 

diag(A)<-0
diag(B)<-0
A[A==2]<-1
g2<-graph.adjacency(A,mode="undirected")
remove(A,add_graph,B,C_temp)


#compress to 2-5
node_size<-c(degree(g1),degree(g2))/3 
node_size<-ifelse(node_size>=100,node_size/20,node_size)
#node_size

#合并两个图
node_top<-paste("t",1:N,sep="") #top 100节点
node_bottom<-paste("b",1:N,sep="") #bottom 100节点名

#添加进图属性
V(g1)$name<-node_top
V(g2)$name<-node_bottom


#节点属性  AI=0.2  US=AS=0.4
nodes <- data.frame(name=c(node_top,node_bottom),
                    lvl=c(rep(1,N),rep(2,N)),
                    literacy=c(rep("hA",0.3*N),rep("hU",0.2*N),
                               rep("lA",0.3*N),rep("lU",0.2*N),
                               rep("hI",0.1*N),rep("hS",0.4*N),
                               rep("lI",0.1*N),rep("lS",0.4*N)
                               )
                    #literacy=rep(c("h","l"),N)
                    )
literacy=c(rep("hA",0.3*N),rep("hU",0.2*N),
                     rep("lA",0.3*N),rep("lU",0.2*N),
                     rep("hI",0.1*N),rep("hS",0.4*N),
                     rep("lI",0.1*N),rep("lS",0.4*N)) %>% 
  factor(levels=c("hA","hU","lA","lU",
                    "hI","hS","lI","lS"))

#边属性
edges <- rbind(igraph::as_data_frame(g1), igraph::as_data_frame(g2)) %>% 
  rbind(data.frame(from=node_top,to=node_bottom))

#新图
new_g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)



#-----------------------------#
xy <- layout_as_multilevel(new_g,type = "fix1",   #固定上下布局
                           FUN1 = layout_as_backbone,   
                           #FUN2 = layout_with_stress,
                           alpha = 25, beta = 35)

ggraph(new_g, "manual", x = xy[, 1], y = xy[, 2]) +
  geom_edge_link0(
    aes(filter = (node1.lvl == 1 & node2.lvl == 1)),
    edge_colour = "#FFBCBC",#firebrick3
    alpha = 0.7,
    edge_width = 0.25
  ) +
  geom_edge_link0(
    aes(filter = (node1.lvl != node2.lvl)),
    alpha = 0.3,
    edge_width = 0.1,
    edge_colour = "black"
  ) +
  geom_edge_link0(
    aes(filter = (node1.lvl == 2 &
                    node2.lvl == 2)),
    edge_colour = "#B5EAEA",#goldenrod3
    edge_width = 0.25,
    alpha = 0.5
  ) +
  geom_node_point(aes(shape = factor(literacy,levels=c("hA","hU","lA","lU",
                                                          "hI","hS","lI","lS"))), 
                  fill = c(rep("#284E78",0.3*N),rep("#ECD662",0.2*N),
                           rep("#284E78",0.3*N),rep("#ECD662",0.2*N),
                           rep("#FF4646",0.1*N),rep("#61B15A",0.4*N),
                           rep("#FF4646",0.1*N),rep("#61B15A",0.4*N)), 
                  # color = c(rep("#284E78",0.3*N),rep("#ECD662",0.2*N),
                  #           rep("#284E78",0.3*N),rep("#ECD662",0.2*N),
                  #           rep("#FF4646",0.1*N),rep("#61B15A",0.4*N),
                  #           rep("#FF4646",0.1*N),rep("#61B15A",0.4*N)), 
                  size = node_size) +
  scale_shape_manual(values = c(21,21,22,22,21,21,22,22)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.title = element_blank())


ggsave("structure1.png",width=24, height=18,units="cm",dpi = 300)


b<-Sys.time()
a
b
b-a







































