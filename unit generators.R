# load plotting library
library(ggplot2)
# set parameters
n.units <- 10
time.units <- 10 
n.iterations <- 100

# text labels for units - currently unused

text.labels <- data.frame(Unit.Number = paste("Unit",1:n.units))

# cbind.rep definition - combine lots of the same column vector 
cbind.rep <- function(x, times) matrix(x, length(x), times, byrow = FALSE)

# generate sequences of numbers that add up to 100% & create distribution matrix
# size of each distribution matrix
number.in.distribution.matrix <- n.units * time.units
# define the unrolled distribution matrix - each row is an 'unrolled matrix' i.e. a row vecto
unrolled.distribution.matrices <- matrix(0,ncol=(number.in.distribution.matrix),nrow=n.units)
# fill in the distribution matrices
for (i in 1:n.units) {
  generated.int <- sample(1:100,number.in.distribution.matrix,replace=TRUE)
  total.generated <- sum(generated.int)
  unrolled.dist.matrix <- generated.int/total.generated
  # add to the unrolled distribution matrices matrix
  unrolled.distribution.matrices[i,] <- unrolled.dist.matrix
  # set the outflow from unit 1 tenure 1 to 100% for the seed unit - at the moment incomplete so commented out.
  # unrolled.distribution.matrices[1,1] <- 1
}
# set the last unit to be the sink (i.e. there are no outflows)
unrolled.distribution.matrices[nrow(unrolled.distribution.matrices),] = 0

# stock vector generating randomly
# this row randomly generates the initial stock for each unit. It contains an outflow to itself - something we want to get rid of. More on that later.
stock.vector.matrix <- matrix(sample(50:150,time.units*n.units,replace=TRUE),ncol=(time.units),nrow=n.units)

# create the seed stock - currently unused so commented out
# stock.vector.matrix[,1] <- 0
# stock.vector.matrix[1,1] <- 1

# unit destination matrix - we'll use this at the end of the iteration to update the stocks
unit.destination.matrix <- matrix(0,n.units,n.units)
# create a stock history matrix to add to at the end of each iteration - this gives us the time series
stock.history <- cbind(1:n.units,matrix(0,n.units,1),stock.vector.matrix)
# iteration counter
counter <- 0

# iteratation algorithm
# 1. take initial stock
# 2. calculate the outflow for each unit by time unit (which becomes in the inflow)
# 3. calculate end stock for the time period
# 4. move the stock forward one time unit
# 5. add in the new inflow
# 6. save in stock history
# 7. begin new iteration
for (i in 1:n.iterations){
  counter <- counter + 1
  for (j in 1:n.units) {
    # roll distribution matrix. here we create each distribution matrix for j from the unrolled distribution matrices matrix
    distribution.matrix <- matrix(unrolled.distribution.matrices[j,],nrow=time.units,ncol=n.units)
    # the control matrix is used to delete flows from a unit to itself.
    control.matrix <- matrix(1, time.units, n.units)
    control.matrix[,j]=0
    # this is to prevent units flowing back to the seed unit - currently incomplete so commented out.
    # control.matrix[,1]=0
    
    # edit the distribution matrix using the control matrix
    distribution.matrix <- distribution.matrix*control.matrix
    
    # trying to sort out the seed unit - incomlpete so commmented out
    # if(j==1){
    #   distribution.matrix[1,1]=1
    # }
    
    #unit destinations. This function takes the distribution matrix multiplied by the stock to give the row vector of the unit outflows
    unit.destinations <- stock.vector.matrix[j,]%*%distribution.matrix
    # add it to the total distribution matrix. at the end we'll add up the columns to get the total flows to each unit
    unit.destination.matrix[j,] <- unit.destinations
    
    # outflow by time unit. This updates the time units in each of the unit matrices,
    # using the cbind.rep to create a t by n matrix
    element.matrix <- cbind.rep(t(stock.vector.matrix[j,]),n.units)
    # element-wise multiplation and row sums give us our updated time units
    outflow <- rowSums(element.matrix*distribution.matrix)
    #end stock
    stock.vector.matrix[j,] <- stock.vector.matrix[j,]- t(outflow)
  }
  # defining inflow from our unit destinations - adding the columns together
  inflow.from.system <- colSums(unit.destination.matrix)
  # can hard code in here additional inflow before seed unit up and running
  inflow.new <- 0 # matrix(100,1,n.units)
  # add them together...
  inflow <- inflow.from.system + inflow.new
  # time shift for next iteration - the stock for each time unit moved to the next time unit
  # the cbind creates the matrix that will move along - basically the identity matrix for a t by t-1 matrix, with a zero column matrix tacked on the front
  stock.vector.matrix <- stock.vector.matrix %*% cbind(matrix(0,time.units,1),diag(1,time.units,time.units-1))
  # add in the inflow...
  stock.vector.matrix[,1] <- t(inflow)
  # update the stock history with the results of the iteration
  stock.history <- rbind(stock.history,cbind(1:n.units,matrix(counter,n.units,1),stock.vector.matrix))
}

# defining the function that will plot the stock of each unit over time
stock.plot <- function(x) {
  stock.tracker <- rowSums(x[,3:ncol(x)])
  stock.tracker <- cbind(x[,1:2],stock.tracker)
  stock.tracker.df <- as.data.frame(stock.tracker)
  ggplot(data=stock.tracker.df, aes(x=V2,y=stock.tracker,group=V1,colour=V1)) + geom_line()
}