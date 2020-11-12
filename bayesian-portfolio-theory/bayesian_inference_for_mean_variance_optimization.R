# Bayesian mean variance optimisation
# Based on Finaltis case study (Croisille et al., 2016)
# Average volatilities

#1. Build the prior distribution
bucketMeans = c(0.233, 0.297, 0.369, 0.433, 0,676) #volatilities
bucketSigma = 0.028 #assume same volatility each bucket
  
#2. Observe a data point
data        = 0.239

#3. Compute the z-scores
z_scores    = (data-bucketMeans)/bucketSigma
z_scores

#4. Height of pdf at z-score
prob        = round(dnorm(z_scores), 4)
prob

#5. Normalize the probabilities
probs = prob/sum(prob)
probs

#6. Value to make use of
value = probs[1]*bucketMeans[1] + probs[2]*bucketMeans[2]
value
