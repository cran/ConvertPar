\name{gen.data}
\alias{gen.data}
\title{Generating Dichotomous Data Sets based on Logistic IRT Models (Rasch, 2PL, 3PL).}
\usage{
gen.data(model="2PL",samplesize=1000,itemsize=100,
theta.mean=0,theta.sd=1, a.mean=0, a.sd=0.2,b.mean=0,
b.sd=1, c.min=0, c.max=0.25)}
\description{
This function can be used for generating dichotomous response matrices based on Logistic IRT Models. Sample size, item number, parameter distributions can be specified.
}
\arguments{

\item{model}{string: option for desired IRT model. 'Rasch', '2PL' or '3PL'  ('2PL' is default)}

\item{samplesize}{numeric: Desired Sample size (Default 1000).}

\item{itemsize}{numeric: Desired item number (Default 100).}

\item{theta.mean}{numeric: mean value of theta normal distribution (Default 0).}

\item{theta.sd}{numeric: standart deviation of theta normal distribution (Default 1).}

\item{a.mean}{numeric: mean value of a parameters log normal distribution (Default 0).}

\item{a.sd}{numeric: standart deviation of a parameters log normal distribution (Default 0.2).}

\item{b.mean}{numeric: mean value of b parameters normal distribution (Default 0).}

\item{b.sd}{numeric: standart deviation of b parameters normal distribution (Default 1).}

\item{c.min}{numeric: minimum value of c parameters uniform distribution (Default 0).}

\item{c.max}{numeric: maximum value of c parameters uniform distribution (Default 0.25).}
}

\value{
This function returns a \code{a data frame} containing simulated dichotomous response matrix.
}

\examples{

 gen.data(model="2PL",
 samplesize=1000,
 itemsize=100,
 theta.mean=0,
 theta.sd=1,
 a.mean=0,
 a.sd=0.2,
 b.mean=0,
 b.sd=1,
 c.min=0,
 c.max=0.25)}

