library(tidyverse)
library(hrbrthemes)
library(treemapify)

source("theme-j.R")

scales::gradient_n_pal(
  colours = c("#6418B6", "#EF3265", "#FFF720"),
  values = sort(unique(vendors$ct))
) -> s

kev <- readRDS("kev-source-of-truth.rds")

kev$vendorProject |> 
  sort() |> 
  table() |> 
  sort() |> 
  enframe() |> 
  select(
    vendor = 1,
    ct = 2
  ) |> 
  mutate(
    vendor = sprintf("%s (%s)", vendor, ct),
    ct = as.integer(ct)
  ) -> vendors

vendors |> 
  ggplot(
    aes(
      area = ct, 
      fill = ct, 
      label = vendor
    )
  ) +
  geom_treemap(
    color = blue_j,
    layout = "scol",
    size = 2
  ) +
  geom_treemap_text(
    aes(
      size = ct,
      color = I(ifelse(ct<5, "black", "white"))
    ),
    layout = "scol",
    family = font_gs,
    place = "centre", 
    grow = FALSE,
    min.size = 0,
  ) +
  scale_fill_gradientn(
    colors = rev(c("#6418B6", "#EF3265", "#FFF720")),
    trans = "log10",
    na.value = "#00000000"
  ) +
  scale_size_area(
    max_size = 30,
    trans = "sqrt"
  ) +
  labs(
    title = "KEV Vendor Rogues Gallery"
  ) +
  theme_ipsum_gs(grid="") +
  theme_jeopardy() +
  theme(
    legend.position = "none"
  )



