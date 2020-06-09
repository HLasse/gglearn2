# Graphs for flowchart
pacman::p_load(tidyverse, skimr, gapminder)


rm_theme <- function() {
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y =element_blank()
        )
}

# size: 400*650

# Point
point <- ggplot(iris, aes(Sepal.Length, Petal.Width)) +
  geom_point(size = 0.5, color = "black") +
  rm_theme()

# + lm
p_lm <- point + geom_smooth(method = "lm", color = "black", se = F)
# + loess
p_loess <- point + geom_smooth(method = "loess", color = "black", se = F)

# Line
lin <- ggplot(filter(gapminder, country == "Uruguay"), aes(year, lifeExp)) +
  geom_line(color = "black") +
  rm_theme()

# Boxplot
box <- ggplot(filter(gapminder, country %in%  c("Uruguay", "Paraguay")), aes(country, lifeExp)) +
  geom_boxplot(color = "black") +
  rm_theme()
# Violin
viol <- ggplot(iris, aes(Species, Sepal.Length)) +
  geom_violin(color = "black") +
  rm_theme()

# Density
dens <- ggplot(cars, aes(x = speed)) +
  geom_density(color = "black", fill = "black") + 
  geom_hline(yintercept=0, colour="white", size=1) +
  rm_theme()

# Histogram
hist <- ggplot(filter(gapminder, continent == "Europe"), aes(x = gdpPercap)) +
  geom_histogram(color = "black", fill = "black") + 
  rm_theme()

# QQ
normal <- data.frame(x = rnorm(200))

qq <- ggplot(normal, aes(sample = x)) +
  geom_qq(size = 0.5) +
  geom_qq_line() +
  rm_theme()

plts <- c(qq, hist, dens,  viol,  box, lin,  p_loess,  p_lm,  point)
nam <- c("qq", "hist", "dens", "violin", "box", "line", "smooth", "linear", "scatter")

i <- 1
for (plt in plts){
  ggsave(paste0(nam[i], ".png"), plt, width = 2.256, height = 1.38, units = "in")
  i <- i + 1
}

#ggsave("box.png", box, width = 2.256, height = 1.38, units = "in")
