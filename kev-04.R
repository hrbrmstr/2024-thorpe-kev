library(tidyverse)
library(hrbrthemes)

kev_now <- read_csv("~/gn/labs-viz-data/docs/kev.csv")

source("theme-j.R")

kev_now |> 
  arrange(
    dateAdded
  ) |> 
  count(
    dateAdded
  ) |> 
  mutate(
    csum = cumsum(n)
  ) |> 
  ggplot() +
  geom_area(
    aes(dateAdded, csum),
    size = 1, 
    color = yellow_j,
    fill = alpha(orange_j, 1/4)
  ) +
  geom_vline(
    xintercept = c(as.Date("2022-02-23"), as.Date("2022-06-09")),
    linetype = "dotted",
    color = orange_j
  ) +
  geom_text(
    data = data.frame(
      x = as.Date("2022-04-17"),
      y = 850,
      label = "← UKR KEV Drops →"
    ),
    aes(
      x = x, y = y, label = label
    ),
    color = orange_j
  ) +
  scale_y_comma() +
  labs(
    x = NULL, y = "# KEV Entries",
    title = "CISA KEV Entries Trajectory",
    subtitle = "Cumulative sum of KEV entries by release."
  ) + 
  theme_ipsum_gs(grid=TRUE) +
  theme_jeopardy()
