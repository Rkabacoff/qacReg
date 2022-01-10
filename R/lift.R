#' Lift and gain charts
#'
#' @description
#' Create lift and gain charts for binary
#' classification problems
#'
#' @param actual actual class.
#' @param prob predicted probability of target class.
#' @param positive which class is the target class?
#'
#' @import ggplot2
#' @export
#'
#' @return a data frame with lift and gain values for each decile.
#' @examples
#' # logistic regression example
#' fit <- glm(am ~ . , mtcars, family=binomial)
#' pred <- predict(fit, newdata=mtcars, type="response")
#' results <- ggliftPlot(mtcars$am, pred, positive="1")
#' print(results)
ggliftPlot <- function(actual, prob, positive="yes"){

  # bind global variables for CRAN
  Letters <- prob_positive <- decile <- nresp <- NULL
  pctevents <- gain <- x <- y <- cumlift <- NULL

  df <- data.frame(prob_positive = prob, actual=actual)

  # df <- df %>% mutate(decile = 11 - ntile(prob_positive, 10))
  #
  # tbl1 <- df %>% group_by(decile) %>% summarize(ncases=n())
  #
  # tbl2 <- df %>% filter(actual == positive) %>%
  #   group_by(decile) %>% summarize(nresp=n())
  #
  # tbl <- inner_join(tbl1, tbl2, by="decile")
  # tbl <- tbl %>% mutate(cumresp = cumsum(nresp))
  # tbl <- tbl %>% mutate(pctevents = round(nresp/sum(nresp)*100, 2))
  # tbl <- tbl %>% mutate(gain = cumsum(pctevents))
  # tbl <- tbl %>% mutate(cumlift = gain /(decile*10))
  # tbl <- tbl %>% mutate(decile = 10* decile)
  #
  # # add 0,0
  # tbl0 <- tbl[1,] %>% mutate(decile=0, gain=0)
  # rndm <- tibble(x=seq(0,100, 10), y=seq(0,100, 10))

  quant10 <- quantile(prob, seq(0, 1, .1))
  print(quant10)
  cut10 <- cut(prob, breaks=quant10, include.lowest=TRUE, labels=seq(1:10))
  df <- data.frame(prob_positive = prob, actual=actual)
  df$decile <- 11 - as.numeric(as.character(cut10))

  tbl1 <- as.data.frame(table(df$decile))
  names(tbl1) <- c("decile", "ncases")
  tbl2 <- df[df$actual == positive, ]
  tbl2 <- as.data.frame(table(tbl2$decile))
  names(tbl2) <- c("decile", "nresp")

  tbl <- merge(tbl1, tbl2, by="decile")
  tbl$decile <- as.numeric(as.character(tbl$decile))
  tbl$cumresp <- cumsum(tbl$nresp)
  tbl$pctevents <- round(tbl$nresp/sum(tbl$nresp)*100, 2)
  tbl$gain <- cumsum(tbl$pctevents)
  tbl$cumlift <- tbl$gain/(tbl$decile*10)
  tbl$decile <-  10 * tbl$decile

  # add 0,0
  tbl0 <- tbl[1, , drop=FALSE]
  tbl0$decile <-tbl0$gain <- 0
  rndm <- data.frame(x=seq(0,100, 10), y=seq(0,100, 10))

  # create gain chart
  p1 <- ggplot(data=rbind(tbl0, tbl), aes(x=decile, y=gain)) +
    geom_point(size=3) + geom_line() +
    scale_x_continuous(breaks = seq(0, 100, 10),
                       limits=c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 100, 10),
                       limits=c(0, 101)) +
    geom_point(data=rndm, aes(x=x, y=y), color="red") +
    geom_line(data=rndm, aes(x=x, y=y), color="red", linetype=2) +
    labs(title = "Gain Chart",
         x = "% of data",
         y = "% of events") +
    theme(panel.grid.minor=element_blank())

  # create lift chart
  rndm <- tibble(x=seq(10,100, 10), y=1)
  p2 <- ggplot(data=tbl, aes(x=decile, y=cumlift)) +
    geom_point() + geom_line() +
    scale_x_continuous(breaks = seq(10, 100, 10),
                       limits=c(10, 100)) +
    geom_point(data=rndm, aes(x=x, y=y), color="red") +
    geom_line(data=rndm, aes(x=x, y=y), color="red", linetype=2) +
    labs(title = "Lift Chart",
         x = "% of data",
         y = "Lift") +
    theme(panel.grid.minor.x=element_blank())

  plot(p1)
  plot(p2)
  return(as.data.frame(tbl))

}
