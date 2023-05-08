library(tidyverse)
library(paletteer)

#### data (distribution of value by groups (stages/ time points/ ...)) ####

plt_df <- tibble(stage = factor(rep(1:4, each = 5), 
                                labels = c("I", "II", "III", "IV")),
                 value = factor(rep(c(1:5), 4), 
                                labels = c("<100", "100-199", "200-349", "350-499", ">=500")),
                 pct = c(0.07,0.09,0.23,0.24,0.37,
                         0.19,0.15,0.26,0.21,0.19,
                         0.34,0.22,0.2,0.13,0.11,
                         0.7,0.11,0.07,0.05,0.07))

#### set parameters for plot ####

var_x <- "stage"                          # variable name for x axis (stage/ time point/ ...)
var_y <- "pct"                            # variable name for y axis (share of value group in x group)
var_value <- "value"                      # variable name for "area" variable (for which distribution is estimated)
area_lbls_x <- c(1.5,1.8,2,2.5,3.5)       # x coordinates for area labels (should be inside interval [1, number of points for x variable])
area_lbls_y <- c(0.87,0.68,0.5,0.36,0.2)  # y coordinates for area labels (should be inside interval [0, 1])
plt_title <- "Value over the Stage"       # plot title
x_title <- ""                             # x axis title

  # labels for x axis
if (is.factor(plt_df[[var_x]])) {
  x_lbls <- levels(plt_df[[var_x]])  
} else {
  x_lbls <- unique(na.omit(plt_df[[var_x]]))
} 

  # labels for areas (groups of value variable)
if (is.factor(plt_df[[var_value]])) {
  area_lbls <- levels(plt_df[[var_value]])  
} else {
  area_lbls <- unique(na.omit(plt_df[[var_value]]))
}

  # colors for areas
clrs <- setNames(str_sub(paletteer_d("ggsci::green_material")[seq(2,10,2)],1,7),
                 area_lbls)

  # colors for area labels
clrs_lbls <- setNames(c(rep("grey20", 2), rep("white", 3)), area_lbls)


#### draw a plot ####

plt_lab <- tibble(
  # labels for areas
  value = rev(area_lbls),
  # x and y coordinates for labels
  x = area_lbls_x,
  y = area_lbls_y)

ggplot() +
  geom_area(aes(x = as.numeric(.data[[var_x]]), y = .data[[var_y]], 
                fill = fct_rev(.data[[var_value]])),
            plt_df, color = "white", alpha = 0.8)  +
  geom_text(aes(x = x, y = y, label = value, color = value),
            plt_lab, size = 3, fontface = "bold", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0,1,0.1),
                     labels = scales::percent_format(accuracy = 1),
                     sec.axis = dup_axis(labels = sprintf("%.0f%%", seq(100,0,-10)))) +
  scale_x_continuous(labels = x_lbls,
                     expand = c(0.05, 0)) +
  scale_fill_manual(values = clrs) +
  scale_color_manual(values = clrs_lbls) +
  labs(y = "", x = x_title, fill = "", 
       title = plt_title) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey50", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 11),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

