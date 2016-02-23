#################################################
### Gauss-Seidel Algorithm with Newton Update ###
#################################################
#' @name GSA(Gauss-Seidel Algorithm)
#' @details For Generalized Linear Model
#' @author fibears
#' @param x : Predictors(Include the intercept) 
#' @param y : Response variable
#' @param f : Log-likelihood function
#' @param df : First order derivative function of f on beta(j)
#' @param ddf : Second order derivative function of f on beta(j)
#' @param b : Coefficient Estimators
#' @param eps : Critical value
f <- function(x,y,b){
    e <- exp(x %*% b)
    p <- e/(1 + e)
    f <- sum(y*(x %*% b) - log(1+e))
    return(f)
}
df <- function(x,y,j,b){
    e <- exp(x %*% b)
    p <- e/(1 + e)
    df <- t(y-p) %*% x[,j]
    return(df)
}
ddf <- function(x,y,j,b){
    e <- exp(x %*% b)
    p <- e/(1 + e)
    ddf <- -t(p*(1-p)) %*% (x[,j]*x[,j])
    return(ddf)
}
GSA <- function(x,y,f,df,ddf,beta = beta0,eps = 1e-06){ 
    # beta0 is the initial value of coefficients
    d <- ncol(x)
    b0 <- rep(0, d)
    b1 <- b0
    diff <- 1
    k <- 1
    while(abs(diff) > eps){
        for(j in 1:d){
            b1[j] <- b0[j] - df(x,y,j,b0)/ddf(x,y,j,b0)
            diff <- abs(f(x,y,b1)-f(x,y,b0))/abs(f(x,y,b0))
            if(abs(diff) < eps) break            # If the criteria is met, stop!
            b0 <- b1
        }
        k <- k + 1
    }
    beta.gsa <- b1
    return(beta.gsa)
}

#################################################
############ Simulation Experiment ##############
#################################################

#### Simulation ####
# Set initial parameters
set.seed(1992)                      # Eliminate random effect #
n <- 200; d <- 10; rho <- 0.5; alpha <- 2
beta0 <- as.matrix(c(alpha,1,2,0,0,3,0,0,0,-2,0), ncol = 1)     # Initial beta value
mu <- rep(0, d) # Mean vector
Sigma <- matrix(data = 0, nrow = d, ncol = d)   # Covariance matrix 
for(i in 1:d){
    for(j in 1:d){
        Sigma[i,j] <- rho^(abs(i-j))
    }
}
x <- mvrnorm(n, mu, Sigma)                      # Initial predictor (For GLM)
x.new <- cbind(rep(1,n),x)                      # Add the intercept (For GSA)
e <- exp(x.new %*% beta0)
p <- e/(1 + e)                                  # Calculate the probability
## Simulate the response variable ##
y <- as.matrix(apply(p, 1, function(x) rbinom(1,1,x)), ncol = 1)

#####################################
### Maximum Likelihood Estimation ###
#####################################
## Part 1 : Estimated by Gauss-Seidel Algorithm ##
beta.gsa <- GSA(x.new,y,f,df,ddf)
## Part 2 : Estimated by GLM ##
glm1=glm(y~x, family=binomial(link="logit"))
beta.glm <- glm1$coefficients
beta1.summary <- cbind(beta0,beta.gsa, beta.glm)
colnames(beta1.summary)[1] <- "beta.true"
print(beta1.summary)

#############################################
############### Conclusion ##################
#############################################
#   From the reslut of "beta1.summary", we can find that 
#   the maximum likelihood estimation computed by Gauss-Seidel Algorithm 
#   is consistedwith the value estimated by R function "glm".Both of two
#   methods can correctly estimate the non-zero coefficients in raw model.
#   And the absolute value of beta.glm is larger than value of beta.gsa.
