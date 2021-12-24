# based on normality test in DescTools
normality.test <- function (x, digits=4) {
    x <- rstudent(x)
    x <- na.omit(x)
    n <- length(x)
    m1 <- sum(x)/n
    m2 <- sum((x - m1)^2)/n
    m3 <- sum((x - m1)^3)/n
    m4 <- sum((x - m1)^4)/n

    J <- sqrt(pi/2) * mean(abs(x - median(x)))
    J2 <- J^2
    b1 <- (m3/(J2)^(3/2))^2
    b2 <- (m4/(J2)^2)
    vk <- 64/n
    vs <- 6/n
    ek <- 3
    statistic <- b1/vs + (b2 - ek)^2/vk

    p.value <- 1 - pchisq(statistic, df = 2)

    cat("Robust Jarque Bera Test for Normality\n")
    cat("Chi-square(2) = ",
        round(statistic, digits),
        ", p < ", format.pval(p.value, digits), "\n", sep="")
  }
