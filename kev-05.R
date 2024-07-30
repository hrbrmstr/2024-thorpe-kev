library(tidyverse)
library(hrbrthemes)
library(treemapify)
library(waffle)

source("theme-j.R")

scales::gradient_n_pal(
  colours = c("#6418B6", "#EF3265", "#FFF720"),
  values = sort(unique(vendors$ct))
) -> s

dput(colorRampPalette(c("#6418B6", "#EF3265", "#FFF720"))(6))

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
    vendor = vendor,
    vendor_lab = sprintf("%s (%s)", vendor, ct),
    ct = as.integer(ct)
  ) -> vendors

vendors |> 
  arrange(desc(ct)) |> 
  mutate(
    pct = proportions(ct),
    cpct = cumsum(pct)
  ) |> 
  mutate(
    vendor = vendor |> 
      fct_other(
        keep = c("Microsoft", "Apple", "Cisco", "Adobe", "Google")
      )
  ) |> 
  count(
    vendor, wt = ct, name = "ct"
  ) |> 
  mutate(
    vendor = factor(
      vendor,
      levels = c("Microsoft", "Apple", "Cisco", "Adobe", "Google", "Other")
    )
  ) |> 
  arrange(
    vendor
  ) |> 
  ggplot() +
  geom_waffle(
    aes(fill = vendor, values = ct),
    color = blue_j,
    size = 1.5,
    n_row = 20
  ) +
  scale_x_continuous(expand = c(0,0,0,0)) +
  scale_y_continuous(expand = c(0,0,0,0)) +
  scale_fill_manual(
    values = c(
      "Microsoft" = "#9B2295", 
      "Apple" = "#D32C75", 
      "Cisco" = "#F25957", 
      "Adobe" = "#F8A83B", 
      "Google" = "#FFF720", 
      "Other" = "grey80"
    )
  ) +
  coord_equal() +
  guides(
    fill = guide_legend(
      nrow = 1
    )
  ) +
  labs(
    title = "KEV Vendor Rogues Gallery",
    subtitle = "Five organizations make up just under 50% of KEV CVEs",
    fill = NULL
  ) +
  theme_ipsum_gs(grid="") +
  theme_jeopardy() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.y.left = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.text = element_text(family = font_gs, size = 16)
  )


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



