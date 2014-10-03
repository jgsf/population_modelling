library(ggplot2)
# set parameters
n.units <- 10
time.units <- 10 
n.iterations <- 100

# text labels for units

text.labels <- data.frame(Unit.Number = paste("Unit",1:n.units))

# cbind.rep definition
cbind.rep <- function(x, times) matrix(x, length(x), times, byrow = FALSE)

# generate sequences of numbers that add up to 100% & create distribution matrix
number.in.distribution.matrix <- n.units * time.units
unrolled.distribution.matrices <- matrix(0,ncol=(number.in.distribution.matrix),nrow=n.units)
for (i in 1:n.units) {
  generated.int <- sample(1:100,number.in.distribution.matrix,replace=TRUE)
  total.generated <- sum(generated.int)
  unrolled.dist.matrix <- generated.int/total.generated
  unrolled.distribution.matrices[i,] <- unrolled.dist.matrix
  unrolled.distribution.matrices[1,1] <- 1
}
# set the last unit to be the sink
unrolled.distribution.matrices[nrow(unrolled.distribution.matrices),] = 0

# create the seed distribution


# stock vector generating randomly
stock.vector.matrix <- matrix(sample(50:150,time.units*n.units,replace=TRUE),ncol=(time.units),nrow=n.units)

# create the seed stock
stock.vector.matrix[,1] <- 0
stock.vector.matrix[1,1] <- 1

# unit destination
unit.destination.matrix <- matrix(0,n.units,n.units)
stock.history <- cbind(1:n.units,matrix(0,n.units,1),stock.vector.matrix)
counter <- 0

# iterate
for (i in 1:n.iterations){
  counter <- counter + 1
  for (j in 1:n.units) {
    # roll distribution matrix
    distribution.matrix <- matrix(unrolled.distribution.matrices[j,],nrow=time.units,ncol=n.units)
    control.matrix <- matrix(1, time.units, n.units)
    control.matrix[,j]=0
    control.matrix[,1]=0
    distribution.matrix <- distribution.matrix*control.matrix
    if(j==1){
      distribution.matrix[1,1]=1
      dmatrix <- distribution.matrix
    }
    #unit destinations
    unit.destinations <- stock.vector.matrix[j,]%*%distribution.matrix
    if(j==1 & i==1){
      udest <- unit.destinations
    }
    unit.destination.matrix[j,] <- unit.destinations
    # outflow
    element.matrix <- cbind.rep(t(stock.vector.matrix[j,]),n.units)
    outflow <- rowSums(element.matrix*distribution.matrix)
    #end stock
    stock.vector.matrix[j,] <- stock.vector.matrix[j,]- t(outflow)
  }
  inflow.from.system <- colSums(unit.destination.matrix)
  inflow.new <- 0 # matrix(100,1,n.units)
  inflow <- inflow.from.system + inflow.new
  # time shift
  stock.vector.matrix <- stock.vector.matrix %*% cbind(matrix(0,time.units,1),diag(1,time.units,time.units-1))
  stock.vector.matrix[,1] <- t(inflow)
  stock.history <- rbind(stock.history,cbind(1:n.units,matrix(counter,n.units,1),stock.vector.matrix))
}

stock.plot <- function(x) {
  stock.tracker <- rowSums(x[,3:ncol(x)])
  stock.tracker <- cbind(x[,1:2],stock.tracker)
  stock.tracker.df <- as.data.frame(stock.tracker)
  ggplot(data=stock.tracker.df, aes(x=V2,y=stock.tracker,group=V1,colour=V1)) + geom_line()
}

stock.plot(stock.history)