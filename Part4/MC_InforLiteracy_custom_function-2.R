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
    random_num<-runif(N,min=0,max=1)
    x<-ifelse(random_num>initial_wl,0,1)
    
    #设置信息层初始状态
    random_num<-runif(N,min=0,max=1)
    m<-ifelse(random_num>initial_xx,0,1)
    
    beta_U<-l/termi
    
    #stp:状态更新次数 state update times
    for(t in 1:stp){
      
      n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
      y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
      
      for(i in 1:N){
        
        #第一步，先更新物理层（被感染，自动获取意识）
        #1.感染者康复
        if(x[i]==1){                    #recovered #infected
          random_n<-runif(1,min=0,max=1)
          ifelse(random_n<mu, y[i]<-0, y[i]<-1)
          
        }else{
        #2.易感者感染
          
          #邻居感染者节点数量
          Njx1<-nrow(subset(data.frame(cbind(nei1=as.vector(B[,i]),x=x)),nei1==1 & x==1))
          
          
          #2.1 无意识易感者
          if(m[i]==0){
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
          }else{
          #2.2 有意识易感者
            beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
          }
        }
        
  
    
        #第二步，更新信息层
        #1. 感染者自动获取意识
        if(x[i]==1){
          n[i]<-1
        
        }else if(m[i]==1){
        #2. 有意识者遗忘
          random_n<-runif(1,min=0,max=1)
          ifelse(random_n<delta,n[i]<-0,n[i]<-1)
          
        }else{
        #3. 无意识者获取 
          
          #邻居有意识者节点数量
          Njm1<-nrow(subset(data.frame(cbind(nei1=as.vector(A[,i]),m=m)),nei1==1 & x==1))
          
          #3.1 从大众媒体
          #3.2 "或"从有意识的邻居
          #信息素养
          random_n<-runif(1,min=0,max=1)
          random_n1<-runif(1,min=0,max=1)
          ifelse(random_n<lamda1*theta[i] | 
                   random_n1<1-(1-lamda2*theta[i])^Njm1,n[i]<-1,n[i]<-0)
        }
        
        x[i]<-y[i]    #将y更新给x
        m[i]<-n[i]    #将n更新给m      
        
      }    
      
      rho_A[l]<-sum(m)/N #收集有意识节点的比例
      rho_I[l]<-sum(x)/N #收集感染者比例
      
    }  
    
    rho_AI<-data.frame(rho_A[l],rho_I[l])
    
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
      random_num<-runif(N,min=0,max=1)
      x<-ifelse(random_num>initial_wl,0,1)
      
      #设置信息层初始状态
      random_num<-runif(N,min=0,max=1)
      m<-ifelse(random_num>initial_xx,0,1)
      
      beta_U<-l/termi
      
      #stp:状态更新次数 state update times
      for(t in 1:stp){
        
        n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
        y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
        
        for(i in 1:N){
          
          #第一步，先更新物理层（被感染，自动获取意识）
          #1.感染者康复
          if(x[i]==1){                    #recovered #infected
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n<mu, y[i]<-0, y[i]<-1)
            
          }else{
            #2.易感者感染
            
            #邻居感染者节点数量
            Njx1<-nrow(subset(data.frame(cbind(nei1=as.vector(B[,i]),x=x)),nei1==1 & x==1))
            
            #2.1 无意识易感者
            if(m[i]==0){
              random_n<-runif(1,min=0,max=1)
              ifelse(random_n < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
            }else{
              #2.2 有意识易感者
              beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
              random_n<-runif(1,min=0,max=1)
              ifelse(random_n < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
            }
          }
          
          
          
          #第二步，更新信息层
          #1. 感染者自动获取意识
          if(x[i]==1){
            n[i]<-1
            
          }else if(x[i]==0 & m[i]==1){
            #2. 有意识者遗忘
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n>delta,n[i]<-m[i],n[i]<-0)
            
          }else{
            #3. 无意识者获取 
            
            #邻居有意识者节点数量
            Njm1<-nrow(subset(data.frame(cbind(nei1=as.vector(A[,i]),m=m)),nei1==1 & x==1))
            
            #3.1 从大众媒体
            #3.2 "或"从有意识的邻居
            #信息素养
            random_n<-runif(1,min=0,max=1)
            random_n1<-runif(1,min=0,max=1)
            ifelse(random_n<lamda1*theta[i] | random_n1<1-(1-lamda2*theta[i])^Njm1,n[i]<-1,n[i]<-0)
          }
          
          x[i]<-y[i]    #将y更新给x
          m[i]<-n[i]    #将n更新给m      
          
        }    
        
        rho_A[l]<-sum(m)/N #收集有意识节点的比例
        rho_I[l]<-sum(x)/N #收集感染者比例
      }  
      
      rho_AI<-data.frame(rho_A[l],rho_I[l])
      
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
      random_num<-runif(N,min=0,max=1)
      x<-ifelse(random_num>initial_wl,0,1)
      
      #设置信息层初始状态
      random_num<-runif(N,min=0,max=1)
      m<-ifelse(random_num>initial_xx,0,1)
      
      lamda1<-l/termi
      
      #stp:状态更新次数 state update times
      for(t in 1:stp){
        
        n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
        y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
        
        for(i in 1:N){
          
          #第一步，先更新物理层（被感染，自动获取意识）
          #1.感染者康复
          if(x[i]==1){                    #recovered #infected
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n<mu, y[i]<-0, y[i]<-1)
            
          }else{
            #2.易感者感染
            
            #邻居感染者节点数量
            Njx1<-nrow(subset(data.frame(cbind(nei1=as.vector(B[,i]),x=x)),nei1==1 & x==1))
            
            #2.1 无意识易感者
            if(m[i]==0){
              random_n<-runif(1,min=0,max=1)
              ifelse(random_n < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
            }else{
              #2.2 有意识易感者
              beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
              random_n<-runif(1,min=0,max=1)
              ifelse(random_n < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
            }
          }
          
          
          
          #第二步，更新信息层
          #1. 感染者自动获取意识
          if(x[i]==1){
            n[i]<-1
            
          }else if(x[i]==0 & m[i]==1){
            #2. 有意识者遗忘
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n>delta,n[i]<-m[i],n[i]<-0)
            
          }else{
            #3. 无意识者获取 
            
            #邻居有意识者节点数量
            Njm1<-nrow(subset(data.frame(cbind(nei1=as.vector(A[,i]),m=m)),nei1==1 & x==1))
            
            #3.1 从大众媒体
            #3.2 "或"从有意识的邻居
            #信息素养
            random_n<-runif(1,min=0,max=1)
            random_n1<-runif(1,min=0,max=1)
            ifelse(random_n<lamda1*theta[i] | random_n1<1-(1-lamda2*theta[i])^Njm1,n[i]<-1,n[i]<-0)
          }
          
          x[i]<-y[i]    #将y更新给x
          m[i]<-n[i]    #将n更新给m      
          
        }    
        
        rho_A[l]<-sum(m)/N #收集有意识节点的比例
        rho_I[l]<-sum(x)/N #收集感染者比例
        
      }  
      
      rho_AI<-data.frame(rho_A[l],rho_I[l])
      
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
      random_num<-runif(N,min=0,max=1)
      x<-ifelse(random_num>initial_wl,0,1)
      
      #设置信息层初始状态
      random_num<-runif(N,min=0,max=1)
      m<-ifelse(random_num>initial_xx,0,1)
      
      lamda2<-l/termi
      
      #stp:状态更新次数 state update times
      for(t in 1:stp){
        
        n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
        y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
        
        for(i in 1:N){
          
          #第一步，先更新物理层（被感染，自动获取意识）
          #1.感染者康复
          if(x[i]==1){                    #recovered #infected
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n<mu, y[i]<-0, y[i]<-1)
            
          }else{
            #2.易感者感染
            
            #邻居感染者节点数量
            Njx1<-nrow(subset(data.frame(cbind(nei1=as.vector(B[,i]),x=x)),nei1==1 & x==1))
            
            #2.1 无意识易感者
            if(m[i]==0){
              random_n<-runif(1,min=0,max=1)
              ifelse(random_n < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
            }else{
              #2.2 有意识易感者
              beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
              random_n<-runif(1,min=0,max=1)
              ifelse(random_n < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
            }
          }
          
          
          
          #第二步，更新信息层
          #1. 感染者自动获取意识
          if(x[i]==1){
            n[i]<-1
            
          }else if(x[i]==0 & m[i]==1){
            #2. 有意识者遗忘
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n>delta,n[i]<-m[i],n[i]<-0)
            
          }else{
            #3. 无意识者获取 
            
            #邻居有意识者节点数量
            Njm1<-nrow(subset(data.frame(cbind(nei1=as.vector(A[,i]),m=m)),nei1==1 & x==1))
            
            #3.1 从大众媒体
            #3.2 "或"从有意识的邻居
            #信息素养
            random_n<-runif(1,min=0,max=1)
            random_n1<-runif(1,min=0,max=1)
            ifelse(random_n<lamda1*theta[i] | random_n1<1-(1-lamda2*theta[i])^Njm1,n[i]<-1,n[i]<-0)
          }
          
          x[i]<-y[i]    #将y更新给x
          m[i]<-n[i]    #将n更新给m      
          
        }    
        
        rho_A[l]<-sum(m)/N #收集有意识节点的比例
        rho_I[l]<-sum(x)/N #收集感染者比例
        
      }  
      
      rho_AI<-data.frame(rho_A[l],rho_I[l])
      
      temp1<-data.frame(Repeat=Repeat,alpha=alpha,lamda2=lamda2,rho_AI)
      
      temp2<-rbind(temp2,temp1)
    }
  }
  return(temp2)
}

#5. Exp5. theta-ratio 注释版
get_MC2_rhoIA_by_lambda1_x<-function(Repeat){
  
  for(l1 in 1:termi){
    
    library(magrittr)
    library(poweRlaw)
    
    minmaxscale<-function(a){
      center <- sweep(a, 2, apply(a, 2, min),'-') 
      R <- apply(a, 2, max) - apply(a,2,min)   
      k<-(sweep(center, 2, R, "/")) 
      return(0.999*k+0.001)
    }
    
    #遍历x,即alpha参数
    set.seed(0808)
    z<-l1/termi+1
    # theta<-rpldis(N,xmin=1,alpha = z ) %>% 
    #   data.frame() %>% 
    #   minmaxscale() %>% 
    #   unlist() %>% 
    #   as.numeric()
    
    theta<-as.numeric(unlist(minmaxscale(data.frame(rpldis(N,xmin=1,alpha = z )))))
    
    #确定4/5位数的解释比例(未标准化前)
    # set.seed(0808)
    # theta<-rpldis(N,xmin=1,alpha = 2.0)
    #explaination<-sum(theta[theta>=quantile(theta,probs = seq(0, 1, 0.2))[5]])/sum(theta)
    
    for(l in 1:termi){
      
      #物理层初始概率 被感染的比例
      initial_wl<-0.2
      #信息层初始概率 有意识的比例
      initial_xx<-0.6 
      
      #设置物理层初始状态
      #MC generates random deviates.about the uniform distribution
      #产生随机数，并进行判断<---蒙特卡洛
      random_num<-runif(N,min=0,max=1)
      x<-ifelse(random_num>initial_wl,0,1)
      
      #设置信息层初始状态
      random_num<-runif(N,min=0,max=1)
      m<-ifelse(random_num>initial_xx,0,1)
      
      lamda1<-l/termi
      
      #stp:状态更新次数 state update times
      for(t in 1:stp){
        
        n<-rep(0,N) #收集更新虚拟层节点防范意识信息 初始为m
        y<-rep(0,N) #收集更新物理层节点感染信息     初始为x
        
        for(i in 1:N){
          
          #第一步，先更新物理层（被感染，自动获取意识）
          #1.感染者康复
          if(x[i]==1){                    #recovered #infected
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n<mu, y[i]<-0, y[i]<-1)
            
          }else{
            #2.易感者感染
            
            #邻居感染者节点数量
            Njx1<-nrow(subset(data.frame(cbind(nei1=as.vector(B[,i]),x=x)),nei1==1 & x==1))
            
            #2.1 无意识易感者
            if(m[i]==0){
              random_n<-runif(1,min=0,max=1)
              ifelse(random_n < 1-(1-beta_U)^Njx1,y[i]<-1,y[i]<-0)
            }else{
              #2.2 有意识易感者
              beta_A<-alpha*gamma*beta_U+(1-alpha)*beta_U
              random_n<-runif(1,min=0,max=1)
              ifelse(random_n < 1-(1-beta_A)^Njx1,y[i]<-1,y[i]<-0)
            }
          }
          
          
          #第二步，更新信息层
          #1. 感染者自动获取意识
          if(x[i]==1){
            n[i]<-1
            
          }else if(x[i]==0 & m[i]==1){
            #2. 有意识者遗忘
            random_n<-runif(1,min=0,max=1)
            ifelse(random_n>delta,n[i]<-m[i],n[i]<-0)
            
          }else{
            #3. 无意识者获取 
            
            #邻居有意识者节点数量
            Njm1<-nrow(subset(data.frame(cbind(nei1=as.vector(A[,i]),m=m)),nei1==1 & x==1))
            
            #3.1 从大众媒体
            #3.2 "或"从有意识的邻居
            #信息素养
            random_n<-runif(1,min=0,max=1)
            random_n1<-runif(1,min=0,max=1)
            ifelse(random_n<lamda1*theta[i] | 
                     random_n1<1-(1-lamda2*theta[i])^Njm1,n[i]<-1,n[i]<-0)
          }
          
          x[i]<-y[i]    #将y更新给x
          m[i]<-n[i]    #将n更新给m      
          
        }    
        
        rho_A[l]<-sum(m)/N #收集有意识节点的比例
        rho_I[l]<-sum(x)/N #收集感染者比例
      } 
      
      rho_AI<-data.frame(rho_A[l],rho_I[l])
      
      temp1<-data.frame(Repeat=Repeat,x=z,lamda1=lamda1,rho_AI)
      
      temp2<-rbind(temp2,temp1)
    }
  }
  return(temp2)
}

