library(tidyverse)
library(ggalluvial)
library(paletteer)

n <- 1000
set.seed(42)
tbl <- tibble(id = rep(1:n, 2),
              time = factor(rep(c("Status 1", "Status 2"), each = n)),
              status = factor(c(sample(LETTERS[1:5], size = n, replace = TRUE, 
                                       prob = c(0.35,0.25,0.2,0.2,0.1)),
                                sample(LETTERS[c(1,3,5,6,7)], size = n, replace = TRUE, 
                                       prob = c(0.4,0.3,0.15,0.1,0.05)))))
  
clrs <- setNames(str_sub(paletteer_c("ggthemes::Sunset-Sunrise Diverging", length(levels(tbl$status))),1,7),
                 levels(tbl$status))

ggplot(tbl,
       aes(x = time, stratum = status, alluvium = id,
           y = 1, fill = status, label = status)) +
  scale_x_discrete(expand = c(.15, .15)) +
  geom_flow(color = "white") +
  geom_stratum(alpha = .5, color = "white") +
  geom_text(stat = "stratum", size = 3, fontface = "bold", color = "grey20") +
  scale_fill_manual(values = clrs) +
  labs(y = "", x = "", fill = "", title = "Status 1 --> Status 2") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 10),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) 
