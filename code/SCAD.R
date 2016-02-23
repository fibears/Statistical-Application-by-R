################################################
##### SCAD Penalized Likelihood Estimation #####
################################################
#' @name pen.SCAD
#' @author fibears
#' @description Algorithm for SCAD penalized Likelihood Estimation
#' @param x : Predictors(Include the intercept) 
#' @param y : Response variable
#' @param b : Coefficient Estimators
#' @param eps : Critical value
#' @param pen : First order derivative of SCAD penality function
#' @param df.pen : First order derivative function of f.pen on beta(j)
#' @param ddf.pen : Second order derivative function of f.pen on beta(j)
#' @param lam : penality parameter
#' @param a : penality parameter
pen <- function(b, j ,lam = 2, a = 3.7){
    f <- function(x,lam,a){
        value <- ifelse((a*lam-x) > 0, a*lam-x, 0)
        f <- value/((a-1)*lam)
        return(f)
    }
    pen <- ifelse(abs(b[j]) <= lam, lam, f(abs(b[j]),lam,a))
    return(pen)
}

df.pen <- function(x,y,j,b){
    e <- exp(x %*% b)
    p <- e/(1 + e)
    df <- t(y-p) %*% x[,j] - pen(b,j)/abs(b[j])*b[j]
    return(df)
}
ddf.pen <- function(x,y,j,b){
    e <- exp(x %*% b)
    p <- e/(1 + e)
    ddf <- -t(p*(1-p)) %*% (x[,j]*x[,j]) - pen(b,j)/abs(b[j])
    return(ddf)
}
pen.SCAD <- function(
    x,y,
    f.pen,df.pen,ddf.pen,
    lam = 2, a = 3.7, eps = 1e-05,
    b0 = beta.glm, delta = 1e-06
){
    # beta.glm is the initial value of coefficients
    b0 <- as.matrix(b0, ncol = 1)
    d <- ncol(x)
    b1 <- b0
    diff <- 1
    k <- 1
    while(diff > eps){
        for(j in 1:d){
            if(b0[j] == 0) next;            # If the criteria is met, jump out the circle!
            b1[j] <- b0[j] - df.pen(x,y,j,b0)/ddf.pen(x,y,j,b0) 
            b1[b1 < delta] <- 0
            diff <- sum((b1-b0)^2)/sum(b0^2)
            if(abs(diff) < eps) break;            # If the criteria is met, stop!
            b0 <- b1
        }
    }
    return(b1)
}

#################################################
############ Simulation Experiment ##############
#################################################

############################################
### SCAD Penalized Likelihood Estimation ###
############################################
## Part 1 : Estimated by pen.SCAD ##
beta.pen <- pen.SCAD(x.new,y,f.pen,df.pen,ddf.pen)
## Part 2 : Estimated by R function--"ncvreg"
SCAD.cv <- cv.ncvreg(x,y,penalty='SCAD')
SCAD <- ncvreg(x,y,penalty='SCAD',lambda=SCAD.cv$lambda.min)
beta.ncvreg <- SCAD$beta
plot(ncvreg(x,y,penalty='SCAD'))
beta2.summary <- cbind(beta0,beta.pen,beta.ncvreg)
colnames(beta2.summary) <- c("beta.true","beta.pen","beta.ncvreg")
print(beta2.summary)

#############################################
############### Conclusion ##################
#############################################
#   From the result of "beta2.summary", we can find that 
#   the pen.SCAD select the variable sets which include the intercept,x1,x2,x5.
#   And the ncvreg select the variable sets which include the intercept,x1,
#   x2,x5,x9.
#   As we know, the truely important variables is the intercept,x1,x2,x5,x9.
#   So, we can say that two algorithm perform well, and ncvreg is 
#   better than pen.SCAD.
#   (This result is highly relative to the sample data!)
##############################################
