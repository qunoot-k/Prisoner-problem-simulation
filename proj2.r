# Contributors:
# Qunoot Khaleeq_s2314595, Morgan Gallagher_s2463128, Banan Alharthi_s2451066

# repo: https://github.com/qunoot-k/Practical2.git

# Contribution:
# Qunoot - Implement strategy 1, 2, Pone for strategy 1, 2, Pall for strategy 1
# Morgan - Implement strategy 3, Pone for strategy 3, Pall for strategy 2 and 3, 
      #example code for each strategy n=5 and n=50, wrote remarks on results 
# Banan - created dloop function and ran the example and plot

strategy_one <- function(n, k, card){

	# Calculate the probability (0 or 1) a prisoner finds their number 
	# in 2n boxes given n tries, starting with box number k, the prisoner's number

	# Input: 
	# n - number of tries/max boxes prisoner can open
	# k - prisoner's number
	# card - vector of unique random generated numbers of length 2n  

	# Output:
	# success - 1 if the prisoner finds the number, 0 otherwise
	
  box <- c(k)
  success = 0
  for (i in 1:n) {
    if (card[box] == k) {success = 1; break} 
		box <- card[box]}
  return(success)
  }

strategy_two <- function(n, k, card){

        # Calculate the probability (0 or 1) a prisoner finds their number
        # in 2n boxes given n tries, starting with a random box number.
	      # If a card contains a number of a box previously opened, it is failure 

        # Input:
        # n - number of tries/max boxes prisoner can open
        # k - prisoner's number
        # card - vector of unique random generated numbers of length 2n

        # Output:
        # success - 1 if the prisoner finds the number, 0 otherwise

  box <- sample(1:(2*n),1)
  success = 0
  for (i in 1:n) {
    current_number <- card[box[i]]
    if (current_number == k) {success = 1; break}
    else if (current_number %in% box) {break}
    box[i+1] <- current_number}
  return(success)
  }

strategy_three <- function(n, k, card){
  
  # Calculate the probability (0 or 1) a prisoner finds their number
  # Open n random boxes out of 2n
  # If none of the n random boxes contains the prisoner number, it is a failure
        
  #Input:
  # n - number of tries, or number of random boxes opened
  # k - prisoner's number
  # card - vector of unique random generated numbers of length 2n
  
  # Output:
  # success - 1 if the prisoner finds the number, 0 otherwise

  rand_boxes <- sample(1:(2*n),n)
  success = 0
  for (i in rand_boxes) {
    if (card[i] == k) {success = 1; break}}
  return(success)
  }


Pone <- function(n, k, strategy, nreps) {

	# Estimates the probability of a single prisoner 
	# succeeding in finding their number

	# Input:
	# n - number of tries/max boxes prisoner can open
	# k - prisoner's number
	# strategy - 1, 2, or 3 to be implemented
	# nreps - number of replicate simulations
  
	# Output:
	# probability of a prisoner succeeding
	
  cat("The probability of a prisoner succeeding, using strategy "
      , strategy, " is ")
  if (strategy == 1) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
			strategy_one(n, k, card)})
    cat(sum(unlist(found))/nreps, "\n")}
  
  else if (strategy == 2) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      strategy_two(n, k, card)})
    cat(sum(unlist(found))/nreps, "\n")}
  
  else if (strategy == 3) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      strategy_three(n, k, card)})
    cat(sum(unlist(found))/nreps, "\n")}
}

Pall <- function(n, strategy, nreps) {

        # Estimates the probability of all prisoners
        # succeeding in finding their number to be set free

        # Input:
        # n - number of tries/max boxes prisoner can open
        # strategy - 1, 2, or 3 to be implemented
        # nreps - number of times to replicate Pone

        # Output:
        # probability of all prisoners succeeding

  cat("The probability of all prisoners succeeding, using strategy "
      , strategy, " is ")
  if (strategy == 1) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_one(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}

  else if (strategy == 2) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_two(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}
  
  else if (strategy == 3) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_three(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}
  }


nreps <- c(10000)
n <- c(5)
k <- sample(1:(2*n), 1)
cat("\n Running", 2*n,"prisoners simulation:\n")
#Estimating the individual success probabilities for n = 5 for each strategy
#Running simulation 10,000 times to get probability 
Pone(n, k, 1, nreps)
Pone(n, k, 2, nreps)
Pone(n, k, 3, nreps)

#Estimating the joint success probabilities for n = 5 for each strategy
#Running simulation 10,000 times to get probability 
Pall(n, 1, nreps)
Pall(n, 2, nreps)
Pall(n, 3, nreps)

n <- c(50)
k <- sample(1:(2*n), 1)
cat("\n Running", 2*n,"prisoners simulation:\n")
#Estimating the individual success probabilities for n = 50 for each strategy
#Running simulation 10,000 times to get probability 
Pone(n, k, 1, nreps)
Pone(n, k, 2, nreps)
Pone(n, k, 3, nreps)

#Estimating the joint success probabilities for n = 50 for each strategy
#Running simulation 10,000 times to get probability 
Pall(n, 1, nreps)
Pall(n, 2, nreps)
Pall(n, 3, nreps)


#Results: 
#What's surprising is that there is a little over 30% chance that all prisoners
#will go free if they implement strategy 1. All strategies have a positive  
#probability under Pone but strategies 2 and 3 seem to be ineffective under Pall 
#heading towards probability 0 of success. Strategy 1 continues to have a  
#surprisingly consistent/high probability compared to the others under Pall. 

#The probability of strategy 1 using Pall is interesting. Because we would 
#expect the event of one prisoner going in and using strategy 1 to be 
#independent of the next prisoner. But this is not the case because Pall using  
#strategy 1 has a probability of about 0.3, and not 0. If the events were  
#independent we would expect Pall to be (Pone*Pone*Pone...*Pone) n times, so 
#(0.5)^n = 0, which is not true according to the simulations. So the event of 
#one prisoner going in and implementing strategy 1 is not independent of another
#going in and also implementing strategy 1. 


# To look further into this matter, dloop function will be created to calculate 
# the probabilities of each length of the nested loop of cards and boxes 
# occurring until it's equal to k.

dloop <- function(n,nreps) {
  
  # Simulate nreps trials of a 2n vector that represents cards inside boxes  
  # and estimate the probabilities for each length of the nested loops 
  # being equal to the initial box k
  
  # Input:
  # n - half of the number of cards and boxes in each trial
  # nreps - number of trials simulated
  
  # Output:
  # v - 2n-vector of probabilities of each length of the nested loops occurring
  
  v <- array(0,2*n)                     # 2n-vector to hold the probabilities
  
  for (i in 1:nreps) {
    u = sample(1:(2*n),2*n)              # card shuffle
    box <- sample(1:(2*n),2*n)           # all the boxes
    c <- array(0,2*n)                    # loop length counter vector
    for (ii in 1:(2*n)) {
    k <- box[ii]                         # initial box
    l <- box[ii]                         # variable to hold nested values
    for (iii in 1:(2*n)) {
      if (u[l] == k) {c[iii] <- c[iii] + 1; break}
      l <- u[l]}
    c <- c/c; c[is.na(c)] <- 0}          # loop length for each card shuffle
    v <- v + c}                          # sum of loop lengths nreps trials
  
  v <- v/(sum(v))                        # calculate the probabilities
  return(v)
  }    

n <- 50
cat("\n Running the nested loops simulation for n =",n,": \n")
v <- dloop(n,nreps)
cat("The probability of no nested loops longer than 50 is",sum(v[1:n]),"\n")
#par(las = 2); plot(v[1:n],type="h"); plot(v[n:(2*n)],type="h")

plot(v,type="h")
