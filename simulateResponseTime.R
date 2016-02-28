require(data.table)
require(ggplot2)

# Compile the calculateResponseTime function
Rcpp::sourceCpp('responseTimeGenerator.cpp')

poissonInterarrival = function(n, rateParameter) {
  return(sapply(1:n,function(i) -log(1.0 - runif(1)) / rateParameter))
}

# Randomly calculate message interarrivals using Poisson processes
messages = data.table("Interarrival" = nextTime(1000,1/10))
# Sum up interarrivals to get timestamps for each message
messages[,Timestamp:=cumsum(Interarrival)]

# Assigning each message a constant service time.  You could use a distribution here
serviceTime=5
messages[,serviceTime := serviceTime]

# Use Rcpp to calculate the response times
messages[,responseTime := calculateResponseTime(Timestamp, serviceTime)]

quantile(messages$responseTime, c(0.5,0.75,0.9,0.95,0.99), na.rm=T)