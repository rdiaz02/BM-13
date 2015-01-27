f <- function(n) {
    s1 <- 1:n
    ln <- log(s1)
    nln <- s1 * ln
    my <- max(s1, max(ln, nln))
    plot(s1, s1, type = "l", ylim = c(0, my), lty = 1,
         ylab= "")
    par(new = TRUE)
    plot(s1, ln, type = "l", ylim = c(0, my), lty = 2,
         ylab = "")
    par(new = TRUE)
    plot(s1, nln, type = "l", ylim = c(0, my), lty = 3,
         ylab = "")
    legend(x = 1, y = my, lty = c(1, 2, 3),
           legend = c("x", "log(x)", "x log(x)"),
           xjust = 0, yjust = 1)
}


factorialIter <- function(x) {
    cat("\n factorialIter has been called \n")
    if(x == 1)
        return(1)
    value <- 1
    for(i in 2:x) {
        cat("\n      inside the for loop with value of i ", i, "\n")
        value <- value * i
    }
    return(value)
}

factorialRecurse <- function(x) {
    cat("   factorialRecurse has been called with x = ", x, "\n")
    if(x == 1) 
        return(1)
    return(x * factorialRecurse(x - 1))
}


factorialRecurse2 <- function(x, d = 0) {
    cat("   factorialRecurse has been called with x = ", x, "\n")
    if(x == 1) {
        cat("   The base case: 1\n")
        return(1)
    } else {
        d <- d + 1
        cat(rep("  ", d))
        return(x * factorialRecurse2(x - 1, d))
    }
}


