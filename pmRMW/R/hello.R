# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


# reference
# https://darrenjw.wordpress.com/2010/09/20/the-pseudo-marginal-approach-to-exact-approximate-mcmc-algorithms/

#Generate obesrved data
x = rexp(100, 5)


# prior that X has a particular distribution?  And we're interested in the parameter of the dist?

### Set up normal M-H

get_ll = function(x, lambda){
  ll = sum(dexp(x, rate = lambda, log = T))   #difference is that we can't calculate this?
  #ll = sum(dexp(x, rate = lambda, lot = T) + rnorm(1))  #is this what we can calculate instead?
  ll
}


stepMH = function(x, lambda, sigma){

  lambda_prime = lambda + runif(1)   # new proposal  # q(lambda', lambda)
  a_prob = get_ll(x, lambda_prime) - get_ll(x, lambda)  #subtract instead of divide because we're working with logs

  u = runif(1)

  if(u < a_prob){
    lambda = lambda_prime
  }

  return(lambda)
}

stepMH(x, 1, 0.03)


##' @param x = data
##' @param lambda = initial proposal for parameter value
##' @param sigma = SD of the proposal distribution
##' @param l = the expected jump size (depends on sigma, but also the prob that a proposal is accepted, so a function of the posterior)

runMH = function(x, lambda, sigma, N){
  lambdas = c(lambda)
  for(i in seq_len(N-1)){
    lambdas[i+1] = stepMH(x, lambdas[i], sigma)
  }
  return(lambdas)
}


plot(runMH(x, 3, 0.5, 5000))
