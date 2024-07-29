library(ggplot2)

theme_jeopardy <- function() {
  theme(
    panel.background = element_rect(fill=NA, color = NA),
    plot.background = element_rect(fill=alpha("#231ab6", 1), color="white"),
    plot.margin = margin(30, 30, 30, 30),
    axis.text = element_text(color = "white", size = 18),
    axis.text.x = element_text(color = "white", size = 18),
    axis.text.y = element_text(color = "white", size = 18),
    axis.text.x.top = element_text(color = "white", size = 18),
    axis.text.x.bottom = element_text(color = "white", size = 18),
    axis.text.y.left = element_text(color = "white", size = 18),
    axis.text.y.right = element_text(color = "white", size = 18),
    axis.title = element_text(color = "white", size = 20),
    axis.title.x = element_text(color = "white", size = 20),
    axis.title.y = element_text(color = "white", size = 20),
    axis.title.x.top = element_text(color = "white", size = 20),
    axis.title.x.bottom = element_text(color = "white", size = 20),
    axis.title.y.left = element_text(color = "white", size = 20),
    axis.title.y.right = element_text(color = "white", size = 20),
    strip.text = element_text(color = "white", face = "bold", size = 18, hjust = 0),
    strip.text.x = element_text(color = "white", face = "bold", size = 18, hjust = 0),
    strip.text.y = element_text(color = "white",face = "bold",  size = 18, hjust = 0),
    strip.text.x.top = element_text(color = "white", face = "bold", size = 18),
    strip.text.x.bottom = element_text(color = "white", face = "bold", size = 18),
    strip.text.y.left = element_text(color = "white", face = "bold",  size = 18),
    strip.text.y.right = element_text(color = "white", face = "bold", size = 18),
    plot.title = element_text(color = "white", size = 32),
    plot.subtitle = element_text(color = "white", size = 26),
    strip.background = element_rect(fill=alpha("#231ab6", 1), color=NA),
    panel.grid.major.x = element_line(linewidth = 0.125, color="#747ed8"),
    panel.grid.major.y = element_line(linewidth = 0.125, color="#747ed8"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )
}

yellow_j <- "#fef758"
blue_j <- "#231ab6"
pink_j <- "#EF3265"
orange_j <- "#e98548"
purple_j <- "#6418B6"
j_grad <- c("#6418B6", "#EF3265", "#FFF720")

# scale_fill_gradientn(
#   colors = c("#6418B6", "#EF3265", "#FFF720"),
#   trans = "log10",
#   na.value = "#00000000"
# )

save_me <- function(plot = last_plot(), filename) {
  ggsave(
    filename = filename, 
    plot = plot,
    width = 13.333, 
    height = 7.5, 
    dpi = 300, 
    units = "in"
  )
}
