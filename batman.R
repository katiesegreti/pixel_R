library(ggplot2)
library(dplyr)


## MICHAEL KEATON AS BATMAN

bg <- "#d31ad0"

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                   axis.text = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.title = element_blank(),
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

x <- rep(-16:16, each = 23)
y <- rep(-11:11, times = 33)
c <- rep(0, 759)
bruce_wayne <- data.frame(x, y, c)


batman <- bruce_wayne %>%
  #add yellow background layer first
  mutate(c = case_when(
    (abs(y) %in% 0:2 & abs(x) %in% 0:14) ~ 1,
    (abs(y) %in% 3:4 & abs(x) %in% 0:13) ~ 1,
    (abs(y) == 5 & abs(x) %in% 0:12) ~ 1,
    (abs(y) == 6 & abs(x) %in% 0:11) ~ 1,
    (abs(y) == 7 & abs(x) %in% 0:9) ~ 1,
    (abs(y) == 8 & abs(x) %in% 0:7) ~ 1,
    (abs(y) == 9 & abs(x) %in% 0:3) ~ 1,
    TRUE ~ c
  ) 
  ) %>%
  # add the bat
  mutate(c = case_when(
    (abs(y) %in% 0:2) & (abs(x) %in% 0:13) ~ 2,
    (y == 3) & ( abs(x) %in% c(0:3, 6:12)) ~ 2,
    (y == -3) & ( abs(x) %in% c(0:6, 9:12)) ~ 2,
    (y == 4) & ( abs(x) %in% c(0:2, 7:12)) ~ 2,
    (y == -4) & ( abs(x) %in% c(0:2, 5, 10:12)) ~ 2,
    (y == 5) & ( abs(x) %in% c(0:2, 7:10)) ~ 2,
    (y == -5) & ( abs(x) %in% c(0, 1, 5, 10)) ~ 2,
    (y == 6) & ( abs(x) %in% c(0:2, 7:8)) ~ 2,
    (y == -6) & ( abs(x) %in% c(0, 9)) ~ 2,
    (y == 7) & ( abs(x) %in% c(0:2, 6:7)) ~ 2,
    (y == 8) & ( abs(x) == 2) ~ 2,
    
    TRUE ~ c
  )
  )


batman %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 12, shape = 22, color="transparent") +
  scale_fill_manual(values = c( bg, "yellow", "black")) +
  pix_theme 





