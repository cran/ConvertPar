\name{conv.ann}
\alias{conv.ann}
\title{Estimating IRT Item Parameters with Small Samples via Artificial Neural Networks}
\usage{
conv.ann(small.data, train.data, model="2PL",layers=1,learningrate=NULL,treshold=0.01)
}
\description{
This function can be used to estimate IRT item parameters (2 PL) using CTT-based item statistics from small samples via artificial neural networks.
}
\arguments{
\item{small.data}{matrix or data frame: contains small sample dichotomous participant response matrix.}

\item{train.data}{matrix or data frame: contains a dichotomous response matrix to use training of ANN model. This matrix should be contain as much as possible participants for more accurate estimations.The "gen.data" function can be used to obtain a simulative response matrix. }

\item{model}{string: option for desired IRT model. 'Rasch' or '2PL' ('2PL' is default)}

\item{layers}{vector: a vector of integers specifying the number of hidden neurons (vertices) in each layer.}

\item{learningrate}{numeric: a numeric value specifying the learning rate.}

\item{treshold}{numeric: a numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria.}
}
\value{
This function returns a \code{list} including following:
\itemize{
\item a matrix: Predicted IRT Parameters
\item a matrix: Item Parameters of Training Data
}
}

\examples{

  ## Genarating item and ability parameters (1000 participants, 100 items)

  a <- rlnorm(100,0,0.3)
  b <- rnorm(100,0,1)
  responses <- matrix(NA, nrow=1000, ncol=100)
  theta <- rnorm(1000, 0,1)

 ### Defining Response Function (2 PL)

  pij <- function(a,b,theta) {
      1/(1+exp(-1*a*(theta-b)))
    }

 ### Creating Response Matrix and column names.

   for( i in 1:1000 ) {
    for( j in 1:100 ) {
      responses[i,j]<-ifelse(pij(a=a[j], b=b[j], theta[i]) < runif(1) , 0 ,1)

    }
  }

  names<-paste("i",1:ncol(responses),sep = "_")

  colnames(responses)<-names
  train<-as.data.frame(responses)

  small.index<-sample(1:nrow(train),100,replace=FALSE)

  small<-train[small.index,]


  ### Conducting Function

  conv.ann(small.data=small, train.data=train, model="2PL",layers=c(2,2),
  learningrate=NULL,treshold=0.01)

}
