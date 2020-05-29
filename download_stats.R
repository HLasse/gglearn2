library("ggplot2")
pacman::p_load("dlstats")

x <- cran_stats(c("groupdata2", "cvms"))

sum(x$downloads[x$package == "ggplot2"])
if (!is.null(x)) {
  head(x)
  ggplot(x, aes(end, downloads, group=package, color=package)) +
    geom_line() + geom_point(aes(shape=package))
}

