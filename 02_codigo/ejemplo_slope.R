pacman::p_load(tidyverse)

tibble(var = c(30, 45, 50, 25, 10, 15),
       edo = c("Ags", "Ags", "BC", "BC", "BCS", "BCS"),
       año = c(2010, 2015, 2010, 2015, 2010, 2015)) %>% 
  ggplot(aes(x = año, y = var, group = edo)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_x_continuous(breaks = c(2010, 2015),
                     limit = c(2009, 2016))

