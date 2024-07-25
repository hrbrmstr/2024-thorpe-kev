library(tidyverse)
library(patchwork)
library(ggbeeswarm)
library(hrbrthemes)

theme_jeopardy <- function() {
  theme(
    panel.background = element_rect(fill="#231ab6", color="#231ab6"),
    plot.background = element_rect(fill="#231ab6", color="white"),
    plot.margin = margin(30, 30, 30, 30),
    axis.text = element_text(color = "white", size=16),
    panel.grid.major.x = element_line(size = 0.125, color="#747ed8"),
    panel.grid.major.y = element_line(size = 0.125, color="#747ed8"),
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
j_grad <- c("#6418B6", "#EF3265", "#FFF720")

kev <- readRDS("kev-source-of-truth.rds")

kev_cve_cwes <- read_csv("kev-cve-cwes.csv", col_types="ccc")

kev |> 
  select(
    v2 = cvssV2_baseScore, 
    v3 = cvssV3_baseScore,
    everything()
  ) |> 
  filter(
    !(is.na(v2) & is.na(v3))
  ) |> 
  mutate(
    cvss = case_when(
      is.na(v3) ~ v2,
      TRUE ~ v3
    )
  ) -> for_cvss
  
for_cvss |> 
  ggplot() +
  geom_density(
    aes(cvss, y = stat(count)),
    fill = alpha(yellow_j, 3/4),
    color = yellow_j
  ) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_comma() +
  labs(
    x = "CVSS Score (V2 when no V3 available)", y = "# CVEs",
    title = "CVSS Score Distribution Of KEV"
  ) +
  theme_jeopardy() -> cvss_dist_kev

for_cvss |> 
  ggplot() +
  geom_density(
    aes(cvss, y = stat(count)),
    fill = alpha(yellow_j, 3/4),
    color = yellow_j
  ) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_comma() +
  facet_wrap(~group, ncol=1) +
  labs(
    x = "CVSS Score (V2 when no V3 available)", y = "# CVEs",
    title = "CVSS Score Distribution Of KEV By Group"
  ) +
  theme_jeopardy() -> cvss_dist_by_kev_group

for_cvss |> 
  mutate(
    delta = dateAdded - publishedDate
  ) |> 
  filter(
    delta > 0,
    !is.na(delta)
  ) |> 
  ggplot() +
  geom_histogram(
    aes(delta),
    fill = alpha(yellow_j, 3/4),
    color = yellow_j
  ) +
  # geom_density(
  #   aes(delta, y = stat(count)),
  #   fill = alpha(yellow_j, 3/4),
  #   color = yellow_j
  # ) +
  scale_y_comma() +
  facet_wrap(~group, ncol=1) +
  labs(
    x = "CVSS Age", y = "# CVEs",
    title = "CVSS Age Distribution Of KEV By Group"
  ) +
  theme_jeopardy() -> cve_age_dist_by_group

for_cvss |> 
  mutate(
    delta = dateAdded - publishedDate,
    pubYear = year(dateAdded)
  ) |> 
  filter(
    delta > 0,
    !is.na(delta)
  ) |> 
  ggplot() +
  geom_histogram(
    aes(delta),
    fill = alpha(blue_j, 3/4),
    color = blue_j
  ) +
  scale_y_comma() +
  facet_wrap(~pubYear, nrow=1) +
  labs(
    x = "CVSS Age", y = "# CVEs",
    title = "CVSS Age Distribution Of KEV Year"
  ) +
  theme_ipsum_gs(grid="Y") -> cvs_age_by_kev_year


for_cvss |> 
  rename(
    cve_id = label
  ) |> 
  select(
    cve_id, group
  ) |> 
  left_join(
    kev_cve_cwes
  ) |> 
  filter(!is.na(cweID)) |> 
  count(group, cweID) |> 
  mutate(
    cweID = fct_reorder(cweID, n, sum)
  ) |> 
  ggplot() +
  geom_tile(
    aes(group, cweID, fill = n)
  ) +
  scale_x_discrete(
    expand = c(0,0,0,0),
    position = "top"
  ) +
  scale_fill_viridis_c() +
  theme_ipsum_gs(grid="")


for_cvss |> 
  rename(
    cve_id = label
  ) |> 
  select(
    cve_id, group
  ) |> 
  left_join(
    kev_cve_cwes
  ) |> 
  filter(!is.na(cweID)) -> cwes_by_group

cwes_by_group |> 
  filter(
    group == "UKR Conflict"
  ) |> 
  pull(cweID) |> 
  unique() -> ukr_cwes_all

cwes_by_group |> 
  filter(
    group != "UKR Conflict"
  ) |> 
  pull(cweID) |> 
  unique() -> first_and_everything_cwes

kev_cve_cwes |> 
  filter(
    cweID %in% setdiff(
      ukr_cwes_all,
      first_and_everything_cwes
    )    
  ) |> 
  distinct(cweID, name) |> 
  arrange(name)
