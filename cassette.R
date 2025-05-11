library(ggplot2)
library(dplyr)



a_green <- "#00FF00"
bg <- "#2F4F4F"
outline <- "#A9A9A9"
datachips_green <- "#36ff33"
datachips_green2 <- "#1e8449"
datachips_grey <- "#6D6D6F"
datachips_pink <- "#ea27c2"
lavender = "#bf80ff"
dark <- "#34495e"
datachips_yellow <- "#FFFF1A"
datachips_light <- "#e1f1ef"

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                   #panel.background = element_rect(fill = dark),
                   #plot.background = element_rect(fill = dark),
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
                   #panel.grid.major = element_blank(),
                   #panel.grid.minor = element_blank(),
                   legend.position = "none",
                   plot.title = element_text(
                     family = "mono" ,
                     size = 52,
                     
                     hjust = 0.5,
                     vjust = -1,
                     face = "bold",
                     color = "white"
                   )
)

#populate data frame with coordinates
cassette_tape <- data.frame(x = rep(-16:16, each = 21), y = rep(-10:10, times = 33), 
                       c = 0)

x <- rep(-16:16, each = 21)
y <- rep(-10:10, times = 33)
c <- rep(0, 693)
cassette_tape <- data.frame(x, y, c)




cassette_tape$c[abs(x) %in% c(0:2)] <- c(0, 0, 1, 1, 1, 1, 2, 2, 2, 1, 3, 1, 1, 1, 2, 2, 2, 1, 1, 0, 0)
cassette_tape$c[abs(x) %in% c(3, 4, 8)] <- c(0, 0, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 0, 0)
cassette_tape$c[abs(x) %in% c(5,7)] <- c(0, 0, 1, 1, 1, 1, 2, 2, 2, 1, 3, 3, 3, 1, 2, 2, 2, 1, 1, 0, 0)
cassette_tape$c[abs(x) == 6] <- c(0, 0, 1, 1, 0, 1, 2, 2, 2, 1, 3, 0, 3, 1, 2, 2, 2, 1, 1, 0, 0)
cassette_tape$c[abs(x) == 9] <- c(0, 0, 1, 0, 1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 1, 1, 0, 0)
cassette_tape$c[abs(x) %in% 10:12] <- c(0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 4, 5, 4, 2, 2, 2, 2, 1, 1, 0, 0)
cassette_tape$c[abs(x) %in% 13:14] <- c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

cassette_tape %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 12, shape = 22, color="transparent") +
  scale_fill_manual(values = c( "transparent", "grey70", datachips_pink, "grey10", "yellow", "blue")) +
  pix_theme +
  xlim(-14, 14) +
  ylim(-8, 8)
