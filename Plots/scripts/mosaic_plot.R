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

var_x <- "stage"                      # variable name for x axis (stage/ time point/ ...)
var_y <- "pct"                        # variable name for y axis (share of value group in x group)
var_value <- "value"                  # variable name for "area" variable (for which distribution is estimated)
y_diff_lbl <- 0.005                   # (var_y - y_diff_lbl) will be set as y coordinate for labels for each value group inside x group
y_lbl_dig <- 0                        # number of decimal points in labels
plt_title <- "Value over the Stage"   # plot title
x_title <- ""                         # x axis title
value_title <- "Value"                # legend title (for var_value)

  # colors for areas
clrs <- str_sub(paletteer_d("ggsci::green_material")[seq(10,1,-2)],1,7)

  # colors for area labels
clrs_lbls <- c(rep("grey20", 2), rep("white", 3))


#### draw a plot ####

  # labels
plt_lbl <- plt_df %>%
  mutate(lab = sprintf("%.f%%", round(.data[[var_y]]*100, y_lbl_dig))) %>%
  group_by(.data[[var_x]]) %>%
  mutate(y = cumsum(.data[[var_y]]) - y_diff_lbl) %>%
  ungroup()


ggplot(data = plt_df, aes(x = .data[[var_x]], y = .data[[var_y]], fill = fct_rev(.data[[var_value]]))) +
  geom_bar(stat = "identity", position = position_stack(), width = 1,
           alpha = 0.9, color = "white") +
  geom_text(aes(y = y, label = lab, color = .data[[var_value]]), 
            plt_lbl,
            size = 3.2, fontface = "bold", vjust = 1,
            show.legend = FALSE) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.1),
                     labels = scales::percent_format(accuracy = 1),
                     sec.axis = dup_axis(labels = sprintf("%.0f%%", seq(100,0,-10)))) +
  scale_fill_manual(values = clrs) +
  scale_color_manual(values = clrs_lbls) +
  labs(y = "", x = x_title, fill = value_title, title = plt_title) +
  # guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", colour = "grey50"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(face = "bold", size = 11),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))
