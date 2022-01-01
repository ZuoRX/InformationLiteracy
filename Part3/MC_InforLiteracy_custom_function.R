#1. Exp1 function
get_MC_iteration_data<-function(Repeat){
  
  for(l in 1:termi){
    
    #物理层初始概率 被感染的比例
    initial_wl<-0.2
    #信息层初始概率 有意识的比例
    initial_xx<-0.6 
    
    #设置物理层初始状态
    #MC generates random deviates.about the uniform distribution
    #产生随机数，并进行判断<---蒙特卡洛
    x<-ifelse(runif(N,min=0,max=1)>initial_wl,0,1)
    
    #设置信息层初始状态
    m<-ifelse(runif(N,min=0,max=1)>initial_xx,0,1)
    
    beta_U<-l/termi
    
    #stp:状态更新次数 state update times
    for(t in 1:stp){
      
      n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
      y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
      
      for(i in 1:N){
        
        #第一步，先更新物理层（被感染，自动获取意识）
        #1.感染者康复
        if(x[i]==1){                    #recovered #infected
          ifelse(runif(1,min=0,max=1)<mu, y[i]<-0, y[i]<-1)
          
        }else{
        #2.易感者感染
          
          #邻居感染者节点数量
          Njx1<-B[,i] %>% 
            data.frame() %>% 
            set_names("nei1") %>% 
            cbind(x=x) %>% 
            subset(nei1==1 & x==1) %>% 
            nrow()
          
          #2.1 无意识易感者
          if(m[i]==0){
            ifelse(runif(1,min=0,max=1) < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
          }else{
          #2.2 有意识易感者
            beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
            ifelse(runif(1,min=0,max=1) < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
          }
        }
        
        
    
        #第二步，更新信息层
        #1. 感染者自动获取意识
        if(x[i]==1){
          n[i]<-1
        
        }else if(m[i]==1){
        #2. 有意识者遗忘
          ifelse(runif(1,min=0,max=1)<delta,n[i]<-0,n[i]<-1)
          
        }else{
        #3. 无意识者获取 
          
          #邻居有意识者节点数量
          Njm1<-A[,i] %>% 
            data.frame() %>% 
            set_names("nei1") %>% 
            cbind(m=m) %>% 
            subset(nei1==1 & m==1) %>% 
            nrow()
          
          #3.1 从大众媒体
          #3.2 "或"从有意识的邻居
          #a.高信息素养
          if(infor[i]==1){
            ifelse(runif(1,min=0,max=1)<lamda1*theta | 
                     runif(1,min=0,max=1)<1-(1-lamda2*theta)^Njm1,n[i]<-1,n[i]<-0)
          #b.低信息素养
          }else{
            ifelse(runif(1,min=0,max=1)<lamda1*(1-theta) |
                     runif(1,min=0,max=1)<1-(1-lamda2*(1-theta))^Njm1, n[i]<-1,n[i]<-0)
          }
        }
        
        x[i]<-y[i]    #将y更新给x
        m[i]<-n[i]    #将n更新给m      
        
      }    
      
      rho_A[l]<-sum(m)/N #收集有意识节点的比例
      rho_I[l]<-sum(x)/N #收集感染者比例
      
      #high
      rho_Ah[l]<-sum(m[infor==1])/length(infor==1)
      rho_Ih[l]<-sum(x[infor==1])/length(infor==1)
      
      #low
      rho_Al[l]<-sum(m[infor==2])/length(infor==2)
      rho_Il[l]<-sum(x[infor==2])/length(infor==2)
    }  
    
    rho_AI<-data.frame(rho_A[l],rho_I[l],rho_Ah[l],rho_Ih[l],rho_Al[l],rho_Il[l])
    
    temp1<-data.frame(Repeat=Repeat,beta_U=beta_U,rho_AI)
    
    temp2<-rbind(temp2,temp1)
  }
  return(temp2)
}



#3. Exp3. beta-alpha
get_Exp3_MC_beta_alpha<-function(Repeat){
  
  for(l1 in 1:termi){
    alpha<-(2*l1)/(2*termi)
    
    for(l in 1:termi){
      
      #物理层初始概率 被感染的比例
      initial_wl<-0.2
      #信息层初始概率 有意识的比例
      initial_xx<-0.6 
      
      #设置物理层初始状态
      #MC generates random deviates.about the uniform distribution
      #产生随机数，并进行判断<---蒙特卡洛
      x<-ifelse(runif(N,min=0,max=1)>initial_wl,0,1)
      
      #设置信息层初始状态
      m<-ifelse(runif(N,min=0,max=1)>initial_xx,0,1)
      
      beta_U<-l/termi
      
      #stp:状态更新次数 state update times
      for(t in 1:stp){
        
        n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
        y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
        
        for(i in 1:N){
          
          #第一步，先更新物理层（被感染，自动获取意识）
          #1.感染者康复
          if(x[i]==1){                    #recovered #infected
            ifelse(runif(1,min=0,max=1)<mu, y[i]<-0, y[i]<-1)
            
          }else{
            #2.易感者感染
            
            #邻居感染者节点数量
            Njx1<-B[,i] %>% 
              data.frame() %>% 
              set_names("nei1") %>% 
              cbind(x=x) %>% 
              subset(nei1==1 & x==1) %>% 
              nrow()
            
            #2.1 无意识易感者
            if(m[i]==0){
              ifelse(runif(1,min=0,max=1) < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
            }else{
              #2.2 有意识易感者
              beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
              ifelse(runif(1,min=0,max=1) < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
            }
          }
          
          
          
          #第二步，更新信息层
          #1. 感染者自动获取意识
          if(x[i]==1){
            n[i]<-1
            
          }else if(x[i]==0 & m[i]==1){
            #2. 有意识者遗忘
            ifelse(runif(1,min=0,max=1)>delta,n[i]<-m[i],n[i]<-0)
            
          }else{
            #3. 无意识者获取 
            
            #邻居有意识者节点数量
            Njm1<-A[,i] %>% 
              data.frame() %>% 
              set_names("nei1") %>% 
              cbind(m=m) %>% 
              subset(nei1==1 & m==1) %>% 
              nrow()
            
            #3.1 从大众媒体
            #3.2 "或"从有意识的邻居
            #a.高信息素养
            if(infor[i]==1){
              ifelse(runif(1,min=0,max=1)<lamda1*theta | 
                       runif(1,min=0,max=1)<1-(1-lamda2*theta)^Njm1,n[i]<-1,n[i]<-0)
              #b.低信息素养
            }else{
              ifelse(runif(1,min=0,max=1)<lamda1*(1-theta) |
                       runif(1,min=0,max=1)<1-(1-lamda2*(1-theta))^Njm1, n[i]<-1,n[i]<-0)
            }
          }
          
          x[i]<-y[i]    #将y更新给x
          m[i]<-n[i]    #将n更新给m      
          
        }    
        
        rho_A[l]<-sum(m)/N #收集有意识节点的比例
        rho_I[l]<-sum(x)/N #收集感染者比例
        
        #high
        rho_Ah[l]<-sum(m[infor==1])/length(infor==1)
        rho_Ih[l]<-sum(x[infor==1])/length(infor==1)
        
        #low
        rho_Al[l]<-sum(m[infor==2])/length(infor==2)
        rho_Il[l]<-sum(x[infor==2])/length(infor==2)
        
      }  
      
      rho_AI<-data.frame(rho_A[l],rho_I[l],rho_Ah[l],rho_Ih[l],rho_Al[l],rho_Il[l])
      
      temp1<-data.frame(Repeat=Repeat,alpha=alpha,beta_U=beta_U,rho_AI)
      
      temp2<-rbind(temp2,temp1)
    }
  }
  return(temp2)
}


#41. Exp41. lamda1-alpha
get_Exp41_MC_lamda1_alpha<-function(Repeat){
  
  for(l1 in 1:termi){
    alpha<-(2*l1)/(2*termi)
    
    for(l in 1:termi){
      
      #物理层初始概率 被感染的比例
      initial_wl<-0.2
      #信息层初始概率 有意识的比例
      initial_xx<-0.6 
      
      #设置物理层初始状态
      #MC generates random deviates.about the uniform distribution
      #产生随机数，并进行判断<---蒙特卡洛
      x<-ifelse(runif(N,min=0,max=1)>initial_wl,0,1)
      
      #设置信息层初始状态
      m<-ifelse(runif(N,min=0,max=1)>initial_xx,0,1)
      
      lamda1<-l/termi
      
      #stp:状态更新次数 state update times
      for(t in 1:stp){
        
        n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
        y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
        
        for(i in 1:N){
          
          #第一步，先更新物理层（被感染，自动获取意识）
          #1.感染者康复
          if(x[i]==1){                    #recovered #infected
            ifelse(runif(1,min=0,max=1)<mu, y[i]<-0, y[i]<-1)
            
          }else{
            #2.易感者感染
            
            #邻居感染者节点数量
            Njx1<-B[,i] %>% 
              data.frame() %>% 
              set_names("nei1") %>% 
              cbind(x=x) %>% 
              subset(nei1==1 & x==1) %>% 
              nrow()
            
            #2.1 无意识易感者
            if(m[i]==0){
              ifelse(runif(1,min=0,max=1) < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
            }else{
              #2.2 有意识易感者
              beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
              ifelse(runif(1,min=0,max=1) < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
            }
          }
          
          
          
          #第二步，更新信息层
          #1. 感染者自动获取意识
          if(x[i]==1){
            n[i]<-1
            
          }else if(x[i]==0 & m[i]==1){
            #2. 有意识者遗忘
            ifelse(runif(1,min=0,max=1)>delta,n[i]<-m[i],n[i]<-0)
            
          }else{
            #3. 无意识者获取 
            
            #邻居有意识者节点数量
            Njm1<-A[,i] %>% 
              data.frame() %>% 
              set_names("nei1") %>% 
              cbind(m=m) %>% 
              subset(nei1==1 & m==1) %>% 
              nrow()
            
            #3.1 从大众媒体
            #3.2 "或"从有意识的邻居
            #a.高信息素养
            if(infor[i]==1){
              ifelse(runif(1,min=0,max=1)<lamda1*theta | 
                       runif(1,min=0,max=1)<1-(1-lamda2*theta)^Njm1,n[i]<-1,n[i]<-0)
              #b.低信息素养
            }else{
              ifelse(runif(1,min=0,max=1)<lamda1*(1-theta) |
                       runif(1,min=0,max=1)<1-(1-lamda2*(1-theta))^Njm1, n[i]<-1,n[i]<-0)
            }
          }
          
          x[i]<-y[i]    #将y更新给x
          m[i]<-n[i]    #将n更新给m      
          
        }    
        
        rho_A[l]<-sum(m)/N #收集有意识节点的比例
        rho_I[l]<-sum(x)/N #收集感染者比例
        
        #high
        rho_Ah[l]<-sum(m[infor==1])/length(infor==1)
        rho_Ih[l]<-sum(x[infor==1])/length(infor==1)
        
        #low
        rho_Al[l]<-sum(m[infor==2])/length(infor==2)
        rho_Il[l]<-sum(x[infor==2])/length(infor==2)
      }  
      
      rho_AI<-data.frame(rho_A[l],rho_I[l],rho_Ah[l],rho_Ih[l],rho_Al[l],rho_Il[l])
      
      temp1<-data.frame(Repeat=Repeat,alpha=alpha,lamda1=lamda1,rho_AI)
      
      temp2<-rbind(temp2,temp1)
      
    }
  }
  return(temp2)
}

#42. Exp41. lamda2-alpha
get_Exp42_MC_lamda2_alpha<-function(Repeat){
  
  for(l1 in 1:termi){
    alpha<-(2*l1)/(2*termi)
    
    for(l in 1:termi){
      
      #物理层初始概率 被感染的比例
      initial_wl<-0.2
      #信息层初始概率 有意识的比例
      initial_xx<-0.6 
      
      #设置物理层初始状态
      #MC generates random deviates.about the uniform distribution
      #产生随机数，并进行判断<---蒙特卡洛
      x<-ifelse(runif(N,min=0,max=1)>initial_wl,0,1)
      
      #设置信息层初始状态
      m<-ifelse(runif(N,min=0,max=1)>initial_xx,0,1)
      
      lamda2<-l/termi
      
      #stp:状态更新次数 state update times
      for(t in 1:stp){
        
        n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
        y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
        
        for(i in 1:N){
          
          #第一步，先更新物理层（被感染，自动获取意识）
          #1.感染者康复
          if(x[i]==1){                    #recovered #infected
            ifelse(runif(1,min=0,max=1)<mu, y[i]<-0, y[i]<-1)
            
          }else{
            #2.易感者感染
            
            #邻居感染者节点数量
            Njx1<-B[,i] %>% 
              data.frame() %>% 
              set_names("nei1") %>% 
              cbind(x=x) %>% 
              subset(nei1==1 & x==1) %>% 
              nrow()
            
            #2.1 无意识易感者
            if(m[i]==0){
              ifelse(runif(1,min=0,max=1) < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
            }else{
              #2.2 有意识易感者
              beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
              ifelse(runif(1,min=0,max=1) < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
            }
          }
          
          
          
          #第二步，更新信息层
          #1. 感染者自动获取意识
          if(x[i]==1){
            n[i]<-1
            
          }else if(x[i]==0 & m[i]==1){
            #2. 有意识者遗忘
            ifelse(runif(1,min=0,max=1)>delta,n[i]<-m[i],n[i]<-0)
            
          }else{
            #3. 无意识者获取 
            
            #邻居有意识者节点数量
            Njm1<-A[,i] %>% 
              data.frame() %>% 
              set_names("nei1") %>% 
              cbind(m=m) %>% 
              subset(nei1==1 & m==1) %>% 
              nrow()
            
            #3.1 从大众媒体
            #3.2 "或"从有意识的邻居
            #a.高信息素养
            if(infor[i]==1){
              ifelse(runif(1,min=0,max=1)<lamda1*theta | 
                       runif(1,min=0,max=1)<1-(1-lamda2*theta)^Njm1,n[i]<-1,n[i]<-0)
              #b.低信息素养
            }else{
              ifelse(runif(1,min=0,max=1)<lamda1*(1-theta) |
                       runif(1,min=0,max=1)<1-(1-lamda2*(1-theta))^Njm1, n[i]<-1,n[i]<-0)
            }
          }
          
          x[i]<-y[i]    #将y更新给x
          m[i]<-n[i]    #将n更新给m      
          
        }    
        
        rho_A[l]<-sum(m)/N #收集有意识节点的比例
        rho_I[l]<-sum(x)/N #收集感染者比例
        
        #high
        rho_Ah[l]<-sum(m[infor==1])/length(infor==1)
        rho_Ih[l]<-sum(x[infor==1])/length(infor==1)
        
        #low
        rho_Al[l]<-sum(m[infor==2])/length(infor==2)
        rho_Il[l]<-sum(x[infor==2])/length(infor==2)
      }  
      
      rho_AI<-data.frame(rho_A[l],rho_I[l],rho_Ah[l],rho_Ih[l],rho_Al[l],rho_Il[l])
      
      temp1<-data.frame(Repeat=Repeat,alpha=alpha,lamda2=lamda2,rho_AI)
      
      temp2<-rbind(temp2,temp1)
    }
  }
  return(temp2)
}

#5. Exp5. theta-ratio 注释版
get_Exp5_MC_theta_ratio<-function(Repeat){
  
  for(l1 in 1:termi){
    ratio<-(2*l1)/(2*termi) #高信息素养节点比例值 50*0.03
    set.seed(111)
    infor<-sample(c(rep(1,ceiling(N*ratio)),rep(2,ceiling(N*(1-ratio)))),N,replace = F)
    
    for(l in 1:termi){
      
      #物理层初始概率 被感染的比例
      initial_wl<-0.2
      #信息层初始概率 有意识的比例
      initial_xx<-0.6 
      
      #设置物理层初始状态
      #MC generates random deviates.about the uniform distribution
      #产生随机数，并进行判断<---蒙特卡洛
      x<-ifelse(runif(N,min=0,max=1)>initial_wl,0,1)
      
      #设置信息层初始状态
      m<-ifelse(runif(N,min=0,max=1)>initial_xx,0,1)
      
      #theta<-l/termi
      theta<-l/(2*termi)+0.49
      
      #stp:状态更新次数 state update times
      for(t in 1:stp){
        
        n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
        y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
        
        for(i in 1:N){
          
          #第一步，先更新物理层（被感染，自动获取意识）
          #1.感染者康复
          if(x[i]==1){                    #recovered #infected
            ifelse(runif(1,min=0,max=1)<mu, y[i]<-0, y[i]<-1)
            
          }else{
            #2.易感者感染
            
            #邻居感染者节点数量
            Njx1<-B[,i] %>% 
              data.frame() %>% 
              set_names("nei1") %>% 
              cbind(x=x) %>% 
              subset(nei1==1 & x==1) %>% 
              nrow()
            
            #2.1 无意识易感者
            if(m[i]==0){
              ifelse(runif(1,min=0,max=1) < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
            }else{
              #2.2 有意识易感者
              beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
              ifelse(runif(1,min=0,max=1) < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
            }
          }
          
          
          
          #第二步，更新信息层
          #1. 感染者自动获取意识
          if(x[i]==1){
            n[i]<-1
            
          }else if(x[i]==0 & m[i]==1){
            #2. 有意识者遗忘
            ifelse(runif(1,min=0,max=1)>delta,n[i]<-m[i],n[i]<-0)
            
          }else{
            #3. 无意识者获取 
            
            #邻居有意识者节点数量
            Njm1<-A[,i] %>% 
              data.frame() %>% 
              set_names("nei1") %>% 
              cbind(m=m) %>% 
              subset(nei1==1 & m==1) %>% 
              nrow()
            
            #3.1 从大众媒体
            #3.2 "或"从有意识的邻居
            #a.高信息素养
            if(infor[i]==1){
              ifelse(runif(1,min=0,max=1)<lamda1*theta | 
                       runif(1,min=0,max=1)<1-(1-lamda2*theta)^Njm1,n[i]<-1,n[i]<-0)
              #b.低信息素养
            }else{
              ifelse(runif(1,min=0,max=1)<lamda1*(1-theta) |
                       runif(1,min=0,max=1)<1-(1-lamda2*(1-theta))^Njm1, n[i]<-1,n[i]<-0)
            }
          }
          
          x[i]<-y[i]    #将y更新给x
          m[i]<-n[i]    #将n更新给m      
          
        }    
        
        rho_A[l]<-sum(m)/N #收集有意识节点的比例
        rho_I[l]<-sum(x)/N #收集感染者比例
        
        #high
        rho_Ah[l]<-sum(m[infor==1])/length(infor==1)
        rho_Ih[l]<-sum(x[infor==1])/length(infor==1)
        
        #low
        rho_Al[l]<-sum(m[infor==2])/length(infor==2)
        rho_Il[l]<-sum(x[infor==2])/length(infor==2)
      } 
      
      rho_AI<-data.frame(rho_A[l],rho_I[l],rho_Ah[l],rho_Ih[l],rho_Al[l],rho_Il[l])
      
      temp1<-data.frame(Repeat=Repeat,ratio=ratio,theta=theta,rho_AI)
      
      temp2<-rbind(temp2,temp1)
    }
  }
  return(temp2)
}

# #5. Exp5. theta-ratio 无注释版
# get_Exp5_MC_theta_ratio<-function(Repeat){
#   
#   for(l1 in 1:termi){
#     ratio<-(2*l1)/(2*termi) 
#     set.seed(111)
#     infor<-sample(c(rep(1,ceiling(N*ratio)),rep(2,ceiling(N*(1-ratio)))),N,replace = F)
#     
#     for(l in 1:termi){
#       
#       initial_wl<-0.2
#       initial_xx<-0.6 
# 
#       x<-ifelse(runif(N,min=0,max=1)>initial_wl,0,1)
#       
#       m<-ifelse(runif(N,min=0,max=1)>initial_xx,0,1)
#       
#       theta<-l/(2*termi)+0.49
#       
#       for(t in 1:stp){
#         
#         n<-rep(0,N) 
#         y<-rep(0,N) 
#         
#         for(i in 1:N){
#           
#           if(x[i]==1){                    
#             ifelse(runif(1,min=0,max=1)<mu, y[i]<-0, y[i]<-1)
#             
#           }else{
# 
#             Njx1<-B[,i] %>% 
#               data.frame() %>% 
#               set_names("nei1") %>% 
#               cbind(x=x) %>% 
#               subset(nei1==1 & x==1) %>% 
#               nrow()
#             
#             if(m[i]==0){
#               ifelse(runif(1,min=0,max=1) < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
#             }else{
#               beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
#               ifelse(runif(1,min=0,max=1) < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
#             }
#           }
#           
#           if(x[i]==1){
#             n[i]<-1
#             
#           }else if(x[i]==0 & m[i]==1){
#             ifelse(runif(1,min=0,max=1)>delta,n[i]<-m[i],n[i]<-0)
#             
#           }else{
#             Njm1<-A[,i] %>% 
#               data.frame() %>% 
#               set_names("nei1") %>% 
#               cbind(m=m) %>% 
#               subset(nei1==1 & m==1) %>% 
#               nrow()
#             
#             if(infor[i]==1){
#               ifelse(runif(1,min=0,max=1)<lamda1*theta | 
#                        runif(1,min=0,max=1)<1-(1-lamda2*theta)^Njm1,n[i]<-1,n[i]<-0)
#             }else{
#               ifelse(runif(1,min=0,max=1)<lamda1*(1-theta) |
#                        runif(1,min=0,max=1)<1-(1-lamda2*(1-theta))^Njm1, n[i]<-1,n[i]<-0)
#             }
#           }
#           
#           x[i]<-y[i] 
#           m[i]<-n[i]        
#           
#         }    
#         
#         rho_A[l]<-sum(m)/N 
#         rho_I[l]<-sum(x)/N
#         
#         rho_Ah[l]<-sum(m[infor==1])/length(infor==1)
#         rho_Ih[l]<-sum(x[infor==1])/length(infor==1)
#         
#         #low
#         rho_Al[l]<-sum(m[infor==2])/length(infor==2)
#         rho_Il[l]<-sum(x[infor==2])/length(infor==2)
#       } 
#       
#       rho_AI<-data.frame(rho_A[l],rho_I[l],rho_Ah[l],rho_Ih[l],rho_Al[l],rho_Il[l])
#       
#       temp1<-data.frame(Repeat=Repeat,ratio=ratio,theta=theta,rho_AI)
#       
#       temp2<-rbind(temp2,temp1)
#     }
#   }
#   return(temp2)
# }