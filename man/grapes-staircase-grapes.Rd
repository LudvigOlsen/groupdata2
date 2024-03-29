% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/staircase_remainder.R
\name{\%staircase\%}
\alias{\%staircase\%}
\alias{staircase}
\title{Find remainder from 'staircase' method}
\usage{
size \%staircase\% step_size
}
\arguments{
\item{size}{Size to staircase (Integer)}

\item{step_size}{Step size (Integer)}
}
\value{
Remainder (Integer).
Returns \code{0} if the last group has the size of the second last group + step size.
}
\description{
\Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}

When using the \code{"staircase"} method,
the last group might not have the size of the second last
group + step size. Use \code{\%staircase\%} to find this remainder.
}
\examples{
# Attach packages
library(groupdata2)

100 \%staircase\% 2

# Finding remainder with value 0
size = 150
for (step_size in c(1:30)){
 if(size \%staircase\% step_size == 0){
   print(step_size)
 }}

}
\seealso{
Other staircase tools: 
\code{\link{\%primes\%}()},
\code{\link{group}()},
\code{\link{group_factor}()}

Other remainder tools: 
\code{\link{\%primes\%}()}
}
\author{
Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
}
\concept{remainder tools}
\concept{staircase tools}
