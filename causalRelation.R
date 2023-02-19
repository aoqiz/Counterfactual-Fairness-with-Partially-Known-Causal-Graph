rm(list=ls())
suppressPackageStartupMessages(library(pcalg))

#### FindCriticalSet
FindCriticalSet <- function(m,x,y)
{
  #q denotes unvisited nodes/ nodes in queue
  #v denotes visited nodes
  q <- v <- previous <- c<- rep(0,1000) 
  criticalSet <- rep(0,length(m[,1])) 
  i <- k <- h <- 1
  q <- x           
  tmp <- m
  ## previous will remember the previous node
  ## on the path, so we can check for definite status
  previous[1] <- q[1]
  c[1] <- 0
  
  while(q[k]!=0 & k<=i)
  {
    t <- q[k]
    #mark t as visited
    v[k] <- t       
    if((k>1) & (t==y) & !(c[k] %in% criticalSet))
    {
      criticalSet[h] <- c[k]
      h <- h+1
    }
    k <- k+1
    #in this for cycle adds all children of t and all nodes j  
    # such that t-j is in the pdag and  <previous[k-1],t,j> is of def. status
    ##i'm using the amat.cpdag encoding: amat[i,j] = 0, amat[j,i]=1 iff i -> j
    for(j in 1:length(tmp[1,])) 
      if (tmp[j,t] != 0)
      {#cat(previous[k-1],t,j,"\n")
        if ((tmp[j,t] ==1 & tmp[t,j] == 0) | (previous[k-1]==t) | (tmp[j,previous[k-1]] ==0 & tmp[previous[k-1],j] ==0))
        {
          #only add nodes that haven't been added
          if (k-1==1 | (k-1>1 & tmp[j,x] ==0 & tmp[x,j] ==0)) 
          {
            ifelse(k-1==1, tempc<-j, tempc<-c[k-1])
            index <- which(q==j)
            IfExists <- function(indx)
            {
              for (ind in indx){
                if (previous[ind]==t & c[ind]==tempc){return(TRUE)}
              }
              return(FALSE)
            }
            if(length(index)==0 | !IfExists(index))
            {
              i <- i+1
              previous[i] <- t
              q[i] <- j
              c[i] <- tempc
            }
          } 
        }
      }
  }
  sort(setdiff(criticalSet,c(0)))
}


##### IdentifyCausalRelation
IdentifyCausalRelation <- function(m,x,y)
{
  tmp <- m
  C <- FindCriticalSet(tmp,x,y)
  if (length(C) == 0)
  {
    #cat(y,"is a definite non-descendant of", x, "\n")
    return(1)
  }
  for (i in 1:length(C)) {
    if (tmp[x,C[i]] == 0 & tmp[C[i],x] ==1) #x -> C
    {
     # cat(y,"is a definite descendant of", x, "\n")
      return(3)
    }
  }
  for (i in 1:length(C)){
    for (j in 1:length(C)) {
      if((i !=j) & tmp[C[i],C[j]]==0 & tmp[C[j],C[i]]==0) #C is incomplete.(example 1)
      {
      #  cat(y,"is a definite descendant of", x, "\n")
        return(3)
      }
    }
  }
  #cat(y,"is a possible descendant of", x, "\n") #C is complete.(example 2)
  return(2)
}

##### IdentifyAllCausalRelation
IdentifyAllCausalRelation <- function(m, x)
{
  tmp <- m
  lenx <- length(x)
  lenm <- length(tmp[,1])
  j <- k <- l <- rep(1,lenx)
  defDes <- matrix(0,lenx,lenm)
  possDes <- matrix(0,lenx,lenm)
  defNonDes <- matrix(0,lenx,lenm)
  for (t in 1: lenx)
  {
    for (i in 1:lenm)
    {
      if (i != x[t])
      {
        CRtype = IdentifyCausalRelation(tmp, x[t], i)
        if (CRtype == 1){defNonDes[t,j[t]]<-i; j[t]<-j[t]+1}
        else if (CRtype == 2){possDes[t,k[t]]<-i; k[t]<-k[t]+1}
        else{defDes[t,l[t]]<-i; l[t]<-l[t]+1}
      }
      else{defDes[t,l[t]]<-i; l[t]<-l[t]+1}
    }    
  }
  InterFun <- function(matr)
  {
    inter = matr[1,]
    for(i in 1:nrow(matr))
    {
      inter <- intersect(inter, matr[i,]) 
    }
    return(inter)
  }
  defNonDes <- sort(setdiff(as.vector(InterFun(defNonDes)),c(0)))
  possDes <- sort(setdiff(InterFun(possDes),c(0)))
  defDes <- sort(setdiff(InterFun(defDes),c(0)))
  result <- list(defNonDes,possDes,defDes)
  names(result) <- c('defNonDes','possDes','defDes')
  return(result)
}

