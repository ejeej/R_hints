library(tidyverse)
library(forcats)
library(viridisLite)
library(colorspace)


#### data ####

data("diamonds")
plt_data <- diamonds


#### set parameters for plot ####

var_group <- "cut"    # groups
var_value <- "price"  # variable to draw a plot with
med_dig <- 0          # number of decimal digits for median
mean_dig <- 0         # number of decimal digits for mean
x_min <- 0            # minimum value for x axis
x_max <- 20000        # maximum value for x axis
x_int <- 4000         # interval between breaks for x axis
plt_title <- "Price by quality of the diamond cut"
x_title <- "Price, USD"


#### draw a plot ####

if (is.factor(plt_data[[var_group]])) {
  lvls <- levels(plt_data[[var_group]])
} else {
  lvls <- unique(na.omit(plt_data[[var_group]]))
}
n_groups <- length(lvls)

x_breaks <- seq(x_min, x_max, x_int)
if (nchar(x_max) > 3 & x_int %/% 100 == floor(x_int %/% 100)) {
  x_lbls <- paste0(x_breaks/1000, ifelse(x_breaks != 0, "K", ""))
} else {
  x_lbls <- x_breaks
}

plt_lbls <- plt_data %>%
  group_by(.data[[var_group]]) %>%
  summarise(med = round(median(.data[[var_value]], na.rm = TRUE), med_dig),
            mean = round(mean(.data[[var_value]], na.rm = TRUE), mean_dig)) %>%
  mutate(lbl = sprintf("%s\n\nMedian - $%.f\nMean - $%.f", .data[[var_group]], med, mean))

clrs <- setNames(rev(viridis(n_groups)), lvls)

ggplot(plt_data, aes(x = as.numeric(.data[[var_group]]), 
                     y = .data[[var_value]], 
                     fill = .data[[var_group]])) +
  ggdist::stat_halfeye(adjust = 0.4, justification = -0.07, .width = 0, 
                       alpha = 0.9, point_colour = NA,
                       show.legend = FALSE) +
  geom_boxplot(aes(color = .data[[var_group]]), 
               width = 0.08, outlier.colour = NA, alpha = 0.5,
               show.legend = FALSE) +
  # geom_text(aes(x = as.numeric(.data[[var_group]]) + 0.6, y = 11000, label = lbl),
  #           plt_lbls, hjust = 0.5, vjust = 1, size = 3.5, color = "grey20",
  #           fontface = "bold") +
  coord_flip() +
  scale_x_continuous(breaks = (1:n_groups)+0.3,
                     labels = plt_lbls$lbl,
                     expand = c(0.02,0,0,0)) +
  scale_y_continuous(limits = c(x_min, x_max),
                     breaks = x_breaks,
                     labels = x_lbls,
                     expand = c(0.01, 0)) +
  scale_fill_manual(values = clrs) +
  scale_color_manual(values = darken(clrs, 0.5)) +
  labs(x = "", y = x_title, title = plt_title) +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = .4, colour = "grey50"),
        axis.line.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.4, color = "grey80", linetype = "dotted"),
        panel.grid.major.y = element_blank())