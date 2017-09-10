



X <- explodeTf(m_tf)

X
dim(m_tf)

row <- m_tf[10,]
length(row)

f <- function(a,b){
  print(paste('a=',a,' b=',b))
}

x <- c(1,2,3)
y <- c(3,2,1)

f(x,y)

sapply(list(x,y), f, )

#single row of tf matrix can be simulated using

genDist <- function(size){
  v <- runif(size)
  v/sum(v)
}

wordDist <- genDist(V)
rmultinom(1, size=100, prob = wordDist)

#M documents can be simulated using
rmultinom(M, size=100, prob = wordDist)


gibbs<-function(N=50000,thin=1000)
{
  mat=matrix(0,ncol=2,nrow=N)
  x=0
  y=0
  for (i in 1:N) {
    for (j in 1:thin) {
      x=rgamma(1,3,y*y+4)
      y=rnorm(1,1/(x+1),1/sqrt(2*x+2))
    }
    mat[i,]=c(x,y)
  }
  names(mat)=c("x","y")
  mat
}

run <- function(){
  tic("gibbs")
  gibbs()
  toc()
}
run()

