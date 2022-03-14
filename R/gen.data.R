
gen.data<-function(model="2PL",samplesize=1000,itemsize=100,
                   theta.mean=0,theta.sd=1, a.mean=0, a.sd=0.2,
                   b.mean=0, b.sd=1, c.min=0, c.max=0.25) {

  a <- rlnorm(itemsize,a.mean, a.sd)
  b <- rnorm(itemsize, b.mean, b.sd)
  c <- runif(itemsize, min = c.min, max=c.max)
  theta <- rnorm(samplesize, 0,1)
  responses <- matrix(NA, nrow=samplesize, ncol=itemsize)

  if(model=="3PL"){

    pij3 <- function(a,b,c,theta) {
      c+ (1+c)*(1/(1+exp(-1*a*(theta-b))))
    }

    for( i in 1:samplesize ) {
      for( j in 1:itemsize ) {
        responses[i,j]<-ifelse(pij3(a=a[j], b=b[j],c=c[j], theta[i]) < runif(1) , 0 ,1)
      }
    }
  }

  else if(model=="2PL" | is.null(model)==TRUE ){
    pij2 <- function(a,b,theta) {
      1/(1+exp(-1*a*(theta-b)))
    }

    for( i in 1:samplesize ) {
      for( j in 1:itemsize ) {
        responses[i,j]<-ifelse(pij2(a=a[j], b=b[j], theta[i]) < runif(1) , 0 ,1)
      }
    }
  }

  else if(model=="Rasch"){
    pij <- function(b,theta) {
      1/(1+exp(-1*(theta-b)))
    }

    for( i in 1:samplesize ) {
      for( j in 1:itemsize ) {
        responses[i,j]<-ifelse(pij(b=b[j], theta[i]) < runif(1) , 0 ,1)
      }
    }
  }

  else{
    stop("Please select one of following options as a string : Rasch, 2PL, 3PL (Default model is 2PL) ")
  }

  names<-paste("i",1:ncol(responses),sep = "_")
  colnames(responses)<-names
  responses<-data.frame(responses)

  return(responses)
}
