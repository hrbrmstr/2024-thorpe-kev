library(tidyverse)
library(hrbrthemes)
library(ggfx)

source("theme-j.R")

kev <- jsonlite::fromJSON("https://rud.is/data/kev.json")$vulnerabilities
range(kev_race$dateAdded)
kev |> 
mutate(
  dateAdded = as.Date(substring(dateAdded, 1, 10))
) |> 
arrange(dateAdded) |> 
group_by(vendorProject) |> 
arrange(dateAdded) |> 
slice(1) |> 
ungroup() |> 
select(
  vendorProject,
  dateAdded
) |>
arrange(dateAdded, vendorProject) |> 
mutate(
  vendorProject = fct_inorder(vendorProject)
) -> kev_race

write_csv(kev_race, "~/Desktop/kev-race.csv")

ttfy <- function(yr, lw=1, sz=10) {
  kev_race |> 
  mutate(
    kev_year = year(dateAdded)
  ) |> 
  filter(
    dateAdded != "2021-11-03",
    kev_year == yr
  ) |> 
  ggplot() +
  with_outer_glow(
    geom_segment(
      aes(
        x = as.Date("2021-11-03"), xend=dateAdded, y = vendorProject, yend=vendorProject
      ),
      color = yellow_j,
      linewidth = lw
    ),
    colour = orange_j,
    sigma = 10
  )  +
  scale_x_date(
    expand = expansion(mult = c(0, 0)),
    date_breaks = "6 months",
    date_labels = "%Y\n%b",
    sec.axis = dup_axis(),
    limits = range(kev_race$dateAdded)
  ) +
  labs(
    x = NULL, y = NULL, 
    title = sprintf("Time To First KEV In %s", yr)
  ) +
  theme_ipsum_gs(grid="X") +
  theme_jeopardy() +
  theme(
    axis.line.y.left = element_line(linewidth = 0.125, color="#747ed8"),
    panel.grid.major = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.y.left = element_text(size = sz),
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.text = element_text(family = font_gs, size = 16)
  )
}

ttfy(2021, lw=3, sz=16)
ttfy(2022, lw=1, sz=10)
ttfy(2023, lw=3, sz=14)
ttfy(2024, lw=3, sz=16)


kev_race |> 
mutate(
  quarter = floor_date(dateAdded, unit = "3 months")
) |> 
count(
  quarter
) |> 
arrange(quarter) |> 
mutate(
  pct = proportions(n),
  csum = cumsum(n),
  cpct = cumsum(pct)
) -> ydf

ydf |>  
ggplot() +
geom_area(
  aes(quarter, cpct),
  size = 1, 
  color = yellow_j,
  fill = alpha(orange_j, 1/4)
) +
geom_vline(
  xintercept = as.Date("2022-10-01"),
  linetype = "dotted",
  color = yellow_j,
  linewidth = 2
) +
scale_y_percent() +
labs(
  x = NULL, y = NULL,
  title = "When Were Vendors First Added To KEV?",
  subtitle = "77% of the vendors currently on the list were added in the first 12 months of KEV's life."
) + 
theme_ipsum_gs(grid=TRUE) +
theme_jeopardy()
