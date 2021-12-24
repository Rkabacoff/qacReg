
library(readr)
auto_mpg <- read_table2("auto-mpg.csv", col_names = FALSE,na = "?")
auto_mpg$X11 <- ifelse(is.na(auto_mpg$X11), "", auto_mpg$X11)
auto_mpg$carname <- paste(auto_mpg$X9, auto_mpg$X10, auto_mpg$X11)
auto_mpg$X9 <- auto_mpg$X10 <- auto_mpg$X11 <- NULL
auto_mpg <- auto_mpg[,c(9, 1:8)]
names(auto_mpg) <- c("carname", "mpg", "cyl", "disp", "hp",
                     "wt", "accel", "year", "origin")
auto_mpg$carname <- gsub('"', '', auto_mpg$carname)
auto_mpg <- na.omit(auto_mpg)
auto_mpg$origin <- factor(auto_mpg$origin)
auto_mpg$cyl <- factor(auto_mpg$cyl)
auto_mpg$year <- factor(auto_mpg$year)
