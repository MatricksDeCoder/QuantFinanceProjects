#Bayesian Inference Coin Flipping 

#1. define outcomes
heads = 1
tails = 0

#2. define probabilistic model for the data (likelihood)-
#   we decide to use the binomial distribution
#   probabilities of getting 0,1,2 etc heads is given below
dbinom(c(0,1,2), size=2, prob= 0.5) #fair coin
dbinom(c(0,1,2), size=2, prob= 0.6) #biased towards heads
dbinom(0:10, size=10, prob = 0.50) # 10 coin flips with fair coin
       
#3. define the prior distribution
#   define the space of all theta values
theta = seq(0,1,length=11) #e.g 0,1,0,2 etc fair coin is 0.5
theta
#   turn the values into a triangular function(symmetric aspect)
probs = pmin(theta, 1-theta)
probs
#   normalize the probabilities to get prior probabilities
prior  = probs/sum(probs)
prior

#4. collect the data and update the likelihood
nHeads = 7
nTails = 3
likelihood = theta^nHeads*(1-theta)^nTails
likelihood
prior # to compare with the likehood based on data observed

#5. apply Bayes Theorem to update the posterior
posterior = (likelihood*prior)/sum(likelihood)
posterior

#6. plot the prior, posterior, likelihood
plot(theta,
     prior,
     main="Prior Distribution",
     type= 'h',
     xlab=expression(theta),
     ylab=expression(P(theta)))
points(theta,prior)
plot(theta,
     likelihood,
     main="Likelihood Function",
     type= 'h',
     xlab=expression(theta),
     ylab=expression(paste("P(D|", theta, ")")))
points(theta, likelihood, cex=2)
plot(theta,
     posterior,
     main="Posterior",
     type= 'h',
     xlab=expression(theta),
     ylab=expression(paste("P(", theta, "|D)")))
points(theta, posterior, cex=2)
