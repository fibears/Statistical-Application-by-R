
# Bayesian Bandits
library(ggplot2)
library(plyr)
library(reshape2)
# Define function
#' @param n: number of pulls
#' @param k: number of arms
#' @title Bayesian Bandits
#' @author fibears
#' @return The summary plot

f <- function(n,k){
    set.seed(123)
    theta <- round(runif(k), 1)
    success <- rep(1, k)
    failure <- rep(1, k)
    for(i in 1:n){
        p <- numeric()
        for(j in 1:k){
            p[j] <- rbeta(1, success[j], failure[j])
        }
        id <- which.max(p)
        success.ind <- ifelse(p[id] <= theta[id], 1, 0)
        failure.ind <- ifelse(p[id] > theta[id], 1, 0)
        success[id] <- success[id] + success.ind
        failure[id] <- failure[id] + failure.ind
    }
    # density plot
    x <- seq(0,1,length=100)
    xx <- numeric()
    for(z in 1:k){
        xx <- cbind(xx, dbeta(x, success[z], failure[z]))
    }
    x <- cbind(x, xx)
    x <- as.data.frame(x)
    names(x) <- c("raw",paste(rep("arm",k),1:k, sep = ""))
    x <- melt(x, id.vars = "raw", value.name = "density")
    theta <- as.data.frame(theta)
    theta[,"name"] <- c(rep(paste(rep("arm",k),1:k, sep = "")))
    dp <- ggplot(data = x, aes(x=raw, y=density, fill = variable)) +
        geom_area(alpha = 0.3,position = "dodge") +
        geom_line(aes(colour = variable)) +
        geom_vline(aes(xintercept = theta, colour = name), data = theta,
                    linetype = "dashed", size = 0.5) +
        ggtitle(paste("Posteriors After",n,"pulls")) 
    return(dp)
}

f(100,k=3)

f(3000,4)


