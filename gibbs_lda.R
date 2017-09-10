
explodeTf <- function(m_tf){
  
  M <- dim(m_tf)[1]
  X <- vector("list", M)
  
  for(j in 1:M){
    row <- m_tf[j,]
    ind <- which(!row == 0)
    
    explodedRow <- sapply(ind, function(i) {rep(i,row[i])})
    r <- unlist(explodedRow, use.names = F)
    X[[j]] <- r
  }
  X
}

createZ <- function(X){
  M <- length(X)
  Z <- vector("list", M)
  for(j in 1:M){
    Z[[j]] <- rep(0,length(X[[j]]))
  }
  Z
} 

sampleTopic <- function(K, wt, dt, j, current_topic, alpha, beta) {
  new_topic <- ((wt[word,current_topic] + beta)/(sum(wt[,current_topic])+V*beta)) * ((dt[j,current_topic] + alpha)/(sum(dt[j,]) + K*alpha))
  new_topic
}

inferLDA <- function(K, m_tf, alpha, beta){
  tic("inferLDA")
  V <- dim(m_tf)[2]
  M <- dim(m_tf)[1]
  
  X <- explodeTf(m_tf)
  Z <- createZ(X)
  
  wt <- array(0, dim = c(V,K))
  dt <- array(0, dim = c(M,K))
  
  # init
  for(j in 1:M){
    row <- X[[j]]
    for(i in 1:length(row)){
      word <- X[[j]][i]
      topic <- sample(K,1)
      Z[[j]][i] <- topic
      wt[word,topic] <- wt[word,topic] + 1
      dt[j,topic] <- dt[j,topic] + 1
    }
  }
  
  # sample
  
  tic("single sample")
  for(q in 1:1){
    tic("single sample")
    for(j in 1:M){
      row <- X[[j]]
      hiddenRow <- Z[[j]]
      for(i in 1:length(row)){
        word <- row[i]
        current_topic <- hiddenRow[i]
        wt[word,current_topic] <- wt[word,current_topic] - 1
        dt[j,current_topic] <- dt[j,current_topic] - 1
        #new_topic <- ((wt[word,current_topic] + beta)/(sum(wt[,current_topic])+V*beta)) * ((dt[j,current_topic] + alpha)/(sum(dt[j,]) + K*alpha))
        dist_num <- ((wt[word,] + beta)/(colSums(wt)+V*beta)) * ((dt[j,] + alpha)/(sum(dt[j,]) + K*alpha))
        dist <- dist_num/sum(dist_num)
        new_topic <- sample(1:K, 1, replace=TRUE, prob=dist )
        
        
        #new_topic
        #print(paste('i=',i))
        #print(paste('j=',j))
        #print(paste('word=',word))
        #print(paste('current_topic=',current_topic))
        #print(paste('wt[word,current_topic]=',wt[word,current_topic]))
        #print(paste('dt[j,current_topic]=',dt[j,current_topic]))
        #print(paste('new_topic=',new_topic))
        hiddenRow[i] <- new_topic
        wt[word,new_topic] <- wt[word,new_topic] + 1
        dt[j,new_topic] <- dt[j,new_topic] + 1
      }
    }
    toc()
  }
  
  toc()
}





