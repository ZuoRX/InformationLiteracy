suppressPackageStartupMessages(suppressWarnings({
  library(igraph)
  library(progress)
  library(magrittr)
  library(reticulate)
  library(tidyverse)
  library(parallel)
  library(foreach)
  library(doParallel)
}))


#1. 收集实验1和实验2所用数据
get_rhoA_rhoI_data<-function(l){
  #相当于给无意识的易感者一个初始被感染概率
  beta_U<-l/ter
  
  # MMCA
  MMCA<-20
  
  #给定一个初始状态概率值
  #0.01 0.05 0.1 0.2 0.3 
  PAI<-rep(0.2,N)   #probability of awareness-infected
  #0.1 0.2 0.3 0.4 0.5 
  PUS<-rep(0.4,N)   #probability of unawareness-susceptible
  #probability of awareness-susceptible
  PAS<-rep(0.4,N)   

  
  #构建0值序列，存储状态更新值
  PAI_UPDATE<-rep(0,N)
  PUS_UPDATE<-rep(0,N)
  PAS_UPDATE<-rep(0,N)
  


  #文章中的公式（1）三个概率值
  #the probability for node i not getting the information by any neighbors.
  r<-rep(0,N)
  #the probability for node i not being infected by any neighbors if i was unaware.
  qu<-rep(0,N)
  #the probability for node i not being infected by any neighbors if i was aware.
  qa<-rep(0,N)
  
  #N维方阵,还是对应公式（1），需要进行矩阵运算，所以这里构建一个方阵来存储数据
  R<-matrix(0, nrow = N, ncol = N)
  QU<-matrix(0, nrow = N, ncol = N)
  QA<-matrix(0, nrow = N, ncol = N)
  
  
  #t:时间步
  #i:节点i
  #j:i的邻居节点
  for(t in 1:MMCA){
    for (i in 1:N){
      for(j in 1:N){
        #给个判断，high-->1  or  low-->2
        R[j,i]<-1-A[j,i]*(PAI[j]+PAS[j])*theta[i]*lamda2
        QU[j,i]=1-B[j,i]*PAI[j]*beta_U
        #alpha概率到行为   gamma概率降低被感染概率
        QA[j,i]=1-B[j,i]*PAI[j]*(alpha*gamma*beta_U+(1-alpha)*beta_U)
      }  
      #return the product(乘积) of all the values 
      r[i]<-prod(R[,i])
      qu[i]<-prod(QU[,i])
      qa[i]<-prod(QA[,i])
      
      #马尔科夫转移状态
      #公式（2）
      
      PUS_UPDATE[i]<-(1-theta[i]*lamda1)*{PAI[i]*delta*mu+
          PUS[i]*r[i]*qu[i]+
          PAS[i]*qu[i]*delta}
      
      PAS_UPDATE[i]<-
        PAI[i]*{delta*theta[i]*lamda1+(1-delta)}*mu+
        PUS[i]*{(1-r[i])+r[i]*theta[i]*lamda1}*qa[i]+
        PAS[i]*{delta*theta[i]*lamda1+(1-delta)}*qa[i]
      
      PAI_UPDATE[i]<-PAI[i]*(1-mu)+
        PUS[i]*((1-r[i])*(1-qa[i])+r[i]*(1-theta[i]*lamda1)*(1-qu[i])+
                  r[i]*theta[i]*lamda1*(1-qa[i]))+
        PAS[i]*(delta*(1-theta[i]*lamda1)*(1-qu[i])+delta*theta[i]*lamda1*(1-qa[i])+
                  (1-delta)*(1-qa[i]))
    }
    
    PAI=PAI_UPDATE
    PUS=PUS_UPDATE
    PAS=PAS_UPDATE
  }
  
  PA<-PAS+PAI
  PI<-PAI
  
  #整体平均情况
  rho_A[l]=sum(PA)/N
  rho_I[l]=sum(PI)/N
  
  rho_AI<-data.frame(l,beta_U,rho_A[l],rho_I[l])
}



#3. 收集实验3所用数据
get_rhoIA_by_beta_alpha<-function(l1){
  
  alpha<-(2*l1)/(2*ter)
  
  #set.seed(111)
  #￥n<-sample(c(rep(1,floor(N*ratio)),rep(2,N-floor(N*ratio))),N,replace = F)
  
  for(l2 in 1:ter){
    
    #相当于给无意识的易感者一个初始被感染概率
    beta_U<-(2*l2)/(2*ter)
    #theta[i]<-l2/(2*ter)+0.49
    
    # MMCA
    MMCA<-20
    
    #给定一个初始状态概率值
    PAI<-rep(0.2,N)   #probability of awareness-infected
    PUS<-rep(0.4,N)   #probability of unawareness-susceptible
    PAS<-rep(0.4,N)   #probability of awareness-susceptible
    
    #构建0值序列，存储状态更新值
    PAI_UPDATE<-rep(0,N)
    PUS_UPDATE<-rep(0,N)
    PAS_UPDATE<-rep(0,N)
    
    #文章中的公式（1）三个概率值
    #the probability for node i not getting the information by any neighbors.
    r<-rep(0,N)
    #the probability for node i not being infected by any neighbors if i was unaware.
    qu<-rep(0,N)
    #the probability for node i not being infected by any neighbors if i was aware.
    qa<-rep(0,N)
    
    #N维方阵,还是对应公式（1），需要进行矩阵运算，所以这里构建一个方阵来存储数据
    R<-matrix(0, nrow = N, ncol = N)
    QU<-matrix(0, nrow = N, ncol = N)
    QA<-matrix(0, nrow = N, ncol = N)
    
    #t:时间步
    #i:节点i
    #j:i的邻居节点
    for(t in 1:MMCA){
      for (i in 1:N){
        for(j in 1:N){
          R[j,i]<-1-A[j,i]*(PAI[j]+PAS[j])*theta[i]*lamda2
          
          QU[j,i]=1-B[j,i]*PAI[j]*beta_U
          #alpha概率到行为   gamma概率降低被感染概率
          QA[j,i]=1-B[j,i]*PAI[j]*(alpha*gamma*beta_U+(1-alpha)*beta_U)
        }  
        #return the product(乘积) of all the values 
        r[i]<-prod(R[,i])
        qu[i]<-prod(QU[,i])
        qa[i]<-prod(QA[,i])
        
        #马尔科夫转移状态
        #公式（2）
        PUS_UPDATE[i]<-(1-theta[i]*lamda1)*{PAI[i]*delta*mu+
            PUS[i]*r[i]*qu[i]+
            PAS[i]*qu[i]*delta}
        
        PAS_UPDATE[i]<-
          PAI[i]*{delta*theta[i]*lamda1+(1-delta)}*mu+
          PUS[i]*{(1-r[i])+r[i]*theta[i]*lamda1}*qa[i]+
          PAS[i]*{delta*theta[i]*lamda1+(1-delta)}*qa[i]
        
        PAI_UPDATE[i]<-PAI[i]*(1-mu)+
          PUS[i]*((1-r[i])*(1-qa[i])+r[i]*(1-theta[i]*lamda1)*(1-qu[i])+
                    r[i]*theta[i]*lamda1*(1-qa[i]))+
          PAS[i]*(delta*(1-theta[i]*lamda1)*(1-qu[i])+delta*theta[i]*lamda1*(1-qa[i])+
                    (1-delta)*(1-qa[i]))
      }
      
      PAI=PAI_UPDATE
      PUS=PUS_UPDATE
      PAS=PAS_UPDATE
    }
    
    PA<-PAS+PAI
    PI<-PAI
    
    #整体平均情况
    rho_A[l1]=sum(PA)/N
    rho_I[l1]=sum(PI)/N
    
    
    rho_AI<-data.frame(rho_A[l1],rho_I[l1])
    
    temp1<-data.frame(l1,alpha,beta_U,rho_AI)
    
    temp2<-rbind(temp2,temp1)
  }
  return(temp2)
}



#41. Exp4-lambda1-alpha
get_rhoIA_by_lambda1_alpha<-function(l1){
  
  alpha<-(2*l1)/(2*ter)
  
  
  for(l2 in 1:ter){
    
    #相当于给无意识的易感者一个初始被感染概率
    lamda1<-(2*l2)/(2*ter)
    #theta[i]<-l2/(2*ter)+0.49
    
    # MMCA
    MMCA<-20
    
    #给定一个初始状态概率值
    PAI<-rep(0.2,N)   #probability of awareness-infected
    PUS<-rep(0.4,N)   #probability of unawareness-susceptible
    PAS<-rep(0.4,N)   #probability of awareness-susceptible
    
    #构建0值序列，存储状态更新值
    PAI_UPDATE<-rep(0,N)
    PUS_UPDATE<-rep(0,N)
    PAS_UPDATE<-rep(0,N)
    
    #文章中的公式（1）三个概率值
    #the probability for node i not getting the information by any neighbors.
    r<-rep(0,N)
    #the probability for node i not being infected by any neighbors if i was unaware.
    qu<-rep(0,N)
    #the probability for node i not being infected by any neighbors if i was aware.
    qa<-rep(0,N)
    
    #N维方阵,还是对应公式（1），需要进行矩阵运算，所以这里构建一个方阵来存储数据
    R<-matrix(0, nrow = N, ncol = N)
    QU<-matrix(0, nrow = N, ncol = N)
    QA<-matrix(0, nrow = N, ncol = N)
    
    #t:时间步
    #i:节点i
    #j:i的邻居节点
    for(t in 1:MMCA){
      for (i in 1:N){
        for(j in 1:N){
          if(A[i,j]==1){
            R[j,i]<-1-A[j,i]*(PAI[j]+PAS[j])*theta[i]*lamda2
            
            QU[j,i]=1-B[j,i]*PAI[j]*beta_U
            #alpha概率到行为   gamma概率降低被感染概率
            QA[j,i]=1-B[j,i]*PAI[j]*(alpha*gamma*beta_U+(1-alpha)*beta_U)
          }else{
            R[j,i]=1
            QU[j,i]<-1
            QA[j,i]<-1
          }
        }  
        #return the product(乘积) of all the values 
        r[i]<-prod(R[,i])
        qu[i]<-prod(QU[,i])
        qa[i]<-prod(QA[,i])
        
        #马尔科夫转移状态
        #公式（2）
        PUS_UPDATE[i]<-(1-theta[i]*lamda1)*{PAI[i]*delta*mu+
            PUS[i]*r[i]*qu[i]+
            PAS[i]*qu[i]*delta}
        
        PAS_UPDATE[i]<-
          PAI[i]*{delta*theta[i]*lamda1+(1-delta)}*mu+
          PUS[i]*{(1-r[i])+r[i]*theta[i]*lamda1}*qa[i]+
          PAS[i]*{delta*theta[i]*lamda1+(1-delta)}*qa[i]
        
        PAI_UPDATE[i]<-PAI[i]*(1-mu)+
          PUS[i]*((1-r[i])*(1-qa[i])+r[i]*(1-theta[i]*lamda1)*(1-qu[i])+
                    r[i]*theta[i]*lamda1*(1-qa[i]))+
          PAS[i]*(delta*(1-theta[i]*lamda1)*(1-qu[i])+delta*theta[i]*lamda1*(1-qa[i])+
                    (1-delta)*(1-qa[i]))
        
      }
      
      PAI=PAI_UPDATE
      PUS=PUS_UPDATE
      PAS=PAS_UPDATE
    }
    
    PA<-PAS+PAI
    PI<-PAI
    
    #整体平均情况
    rho_A[l1]=sum(PA)/N
    rho_I[l1]=sum(PI)/N
    
    rho_AI<-data.frame(rho_A[l1],rho_I[l1])
    
    temp1<-data.frame(l1,alpha,lamda1,rho_AI)
    
    temp2<-rbind(temp2,temp1)
  }
  return(temp2)
}




#42. Exp4-lambda2-alpha
get_rhoIA_by_lambda2_alpha<-function(l1){
  
  alpha<-(2*l1)/(2*ter)
  
  for(l2 in 1:ter){
    
    #相当于给无意识的易感者一个初始被感染概率
    lamda2<-(2*l2)/(2*ter)
    #theta[i]<-l2/(2*ter)+0.49
    
    # MMCA
    MMCA<-20
    
    #给定一个初始状态概率值
    PAI<-rep(0.2,N)   #probability of awareness-infected
    PUS<-rep(0.4,N)   #probability of unawareness-susceptible
    PAS<-rep(0.4,N)   #probability of awareness-susceptible
    
    #构建0值序列，存储状态更新值
    PAI_UPDATE<-rep(0,N)
    PUS_UPDATE<-rep(0,N)
    PAS_UPDATE<-rep(0,N)
    
    #文章中的公式（1）三个概率值
    #the probability for node i not getting the information by any neighbors.
    r<-rep(0,N)
    #the probability for node i not being infected by any neighbors if i was unaware.
    qu<-rep(0,N)
    #the probability for node i not being infected by any neighbors if i was aware.
    qa<-rep(0,N)
    
    #N维方阵,还是对应公式（1），需要进行矩阵运算，所以这里构建一个方阵来存储数据
    R<-matrix(0, nrow = N, ncol = N)
    QU<-matrix(0, nrow = N, ncol = N)
    QA<-matrix(0, nrow = N, ncol = N)
    
    #t:时间步
    #i:节点i
    #j:i的邻居节点
    for(t in 1:MMCA){
      for (i in 1:N){
        for(j in 1:N){
          if(A[i,j]==1){
            R[j,i]<-1-A[j,i]*(PAI[j]+PAS[j])*theta[i]*lamda2
            
            QU[j,i]=1-B[j,i]*PAI[j]*beta_U
            #alpha概率到行为   gamma概率降低被感染概率
            QA[j,i]=1-B[j,i]*PAI[j]*(alpha*gamma*beta_U+(1-alpha)*beta_U)
          }else{
            R[j,i]=1
            QU[j,i]<-1
            QA[j,i]<-1
          }
        }  
        #return the product(乘积) of all the values 
        r[i]<-prod(R[,i])
        qu[i]<-prod(QU[,i])
        qa[i]<-prod(QA[,i])
        
        #马尔科夫转移状态
        #公式（2）

        PUS_UPDATE[i]<-(1-theta[i]*lamda1)*{PAI[i]*delta*mu+
            PUS[i]*r[i]*qu[i]+
            PAS[i]*qu[i]*delta}
        
        PAS_UPDATE[i]<-
          PAI[i]*{delta*theta[i]*lamda1+(1-delta)}*mu+
          PUS[i]*{(1-r[i])+r[i]*theta[i]*lamda1}*qa[i]+
          PAS[i]*{delta*theta[i]*lamda1+(1-delta)}*qa[i]
        
        PAI_UPDATE[i]<-PAI[i]*(1-mu)+
          PUS[i]*((1-r[i])*(1-qa[i])+r[i]*(1-theta[i]*lamda1)*(1-qu[i])+
                    r[i]*theta[i]*lamda1*(1-qa[i]))+
          PAS[i]*(delta*(1-theta[i]*lamda1)*(1-qu[i])+delta*theta[i]*lamda1*(1-qa[i])+
                    (1-delta)*(1-qa[i]))
        
      }
      
      PAI=PAI_UPDATE
      PUS=PUS_UPDATE
      PAS=PAS_UPDATE
    }
    
    PA<-PAS+PAI
    PI<-PAI
    
    #整体平均情况
    rho_A[l1]=sum(PA)/N
    rho_I[l1]=sum(PI)/N
    
    rho_AI<-data.frame(rho_A[l1],rho_I[l1])
    
    temp1<-data.frame(l1,alpha,lamda2,rho_AI)
    
    temp2<-rbind(temp2,temp1)
  }
  return(temp2)
}




#5. 收集实验5所用数据
get_rhoIA_by_lambda1_x<-function(l1){
  
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
  x<-(2*l1)/(2*ter)+1
  theta<-rpldis(N,xmin=1,alpha = x ) %>% 
    data.frame() %>% 
    minmaxscale() %>% 
    unlist() %>% 
    as.numeric()
  
  #确定4/5位数的解释比例(未标准化前)
  # set.seed(0808)
  # theta<-rpldis(N,xmin=1,alpha = 2.0)
  #explaination<-sum(theta[theta>=quantile(theta,probs = seq(0, 1, 0.2))[5]])/sum(theta)
  
  for(l2 in 1:ter){
    
    #相当于给无意识的易感者一个初始被感染概率
    lamda1<-(2*l2)/(2*ter)
    #theta[i]<-l2/(2*ter)+0.49
    
    # MMCA
    MMCA<-20
    
    #给定一个初始状态概率值
    PAI<-rep(0.2,N)   #probability of awareness-infected
    PUS<-rep(0.4,N)   #probability of unawareness-susceptible
    PAS<-rep(0.4,N)   #probability of awareness-susceptible
    
    #构建0值序列，存储状态更新值
    PAI_UPDATE<-rep(0,N)
    PUS_UPDATE<-rep(0,N)
    PAS_UPDATE<-rep(0,N)
    
    #文章中的公式（1）三个概率值
    #the probability for node i not getting the information by any neighbors.
    r<-rep(0,N)
    #the probability for node i not being infected by any neighbors if i was unaware.
    qu<-rep(0,N)
    #the probability for node i not being infected by any neighbors if i was aware.
    qa<-rep(0,N)
    
    #N维方阵,还是对应公式（1），需要进行矩阵运算，所以这里构建一个方阵来存储数据
    R<-matrix(0, nrow = N, ncol = N)
    QU<-matrix(0, nrow = N, ncol = N)
    QA<-matrix(0, nrow = N, ncol = N)
    
    #t:时间步
    #i:节点i
    #j:i的邻居节点
    for(t in 1:MMCA){
      for (i in 1:N){
        for(j in 1:N){
          if(A[i,j]==1){
            R[j,i]<-1-A[j,i]*(PAI[j]+PAS[j])*theta[i]*lamda2
            
            QU[j,i]=1-B[j,i]*PAI[j]*beta_U
            #alpha概率到行为   gamma概率降低被感染概率
            QA[j,i]=1-B[j,i]*PAI[j]*(alpha*gamma*beta_U+(1-alpha)*beta_U)
          }else{
            R[j,i]=1
            QU[j,i]<-1
            QA[j,i]<-1
          }
        }  
        #return the product(乘积) of all the values 
        r[i]<-prod(R[,i])
        qu[i]<-prod(QU[,i])
        qa[i]<-prod(QA[,i])
        
        #马尔科夫转移状态
        #公式（2）
        PUS_UPDATE[i]<-(1-theta[i]*lamda1)*{PAI[i]*delta*mu+
            PUS[i]*r[i]*qu[i]+
            PAS[i]*qu[i]*delta}
        
        PAS_UPDATE[i]<-
          PAI[i]*{delta*theta[i]*lamda1+(1-delta)}*mu+
          PUS[i]*{(1-r[i])+r[i]*theta[i]*lamda1}*qa[i]+
          PAS[i]*{delta*theta[i]*lamda1+(1-delta)}*qa[i]
        
        PAI_UPDATE[i]<-PAI[i]*(1-mu)+
          PUS[i]*((1-r[i])*(1-qa[i])+r[i]*(1-theta[i]*lamda1)*(1-qu[i])+
                    r[i]*theta[i]*lamda1*(1-qa[i]))+
          PAS[i]*(delta*(1-theta[i]*lamda1)*(1-qu[i])+delta*theta[i]*lamda1*(1-qa[i])+
                    (1-delta)*(1-qa[i]))
        
      }
      
      PAI=PAI_UPDATE
      PUS=PUS_UPDATE
      PAS=PAS_UPDATE
    }
    
    PA<-PAS+PAI
    PI<-PAI
    
    #整体平均情况
    rho_A[l1]=sum(PA)/N
    rho_I[l1]=sum(PI)/N
    
    rho_AI<-data.frame(rho_A[l1],rho_I[l1])
    
    temp1<-data.frame(l1,x,lamda1,rho_AI)
    
    temp2<-rbind(temp2,temp1)
  }
  return(temp2)
}





