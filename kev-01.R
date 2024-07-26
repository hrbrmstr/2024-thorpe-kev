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

# scale_fill_gradientn(
#   colors = c("#6418B6", "#EF3265", "#FFF720"),
#   trans = "log10",
#   na.value = "#00000000"
# )

cols(
  type = col_character(),
  label = col_character(),
  epssScore = col_double(),
  patched = col_logical(),
  exploits = col_character(),
  numExploits = col_double(),
  publishedDate = col_date(format = ""),
  cvssCategoryEstimate = col_character(),
  impact = col_character(),
  exploitation = col_character(),
  description = col_character(),
  patch = col_character(),
  mitigation = col_character(),
  processing_time_s = col_double(),
  cvssV2_integrityImpact = col_character(),
  cvssV2_availabilityImpact = col_character(),
  cvssV2_authentication = col_character(),
  cvssV2_accessComplexity = col_character(),
  cvssV2_baseScore = col_double(),
  cvssV2_vectorString = col_character(),
  cvssV2_confidentialityImpact = col_character(),
  cvssV2_accessVector = col_character(),
  cvssV3_integrityImpact = col_character(),
  cvssV3_availabilityImpact = col_character(),
  cvssV3_scope = col_character(),
  cvssV3_attackVector = col_character(),
  cvssV3_userInteraction = col_character(),
  cvssV3_baseScore = col_double(),
  cvssV3_attackComplexity = col_character(),
  cvssV3_confidentialityImpact = col_character(),
  cvssV3_privilegesRequired = col_character(),
  cvssV3_vectorString = col_character(),
  vendorProject = col_character(),
  product = col_character(),
  vulnerabilityName = col_character(),
  dateAdded = col_date(format = ""),
  knownRansomwareCampaignUse = col_character()
) -> kev_cols

kev <- read_csv("feedly-plus-kev.csv", col_types = kev_cols)

kev

list(
  list(begin = as.Date("2021-11-03"), end = as.Date("2021-11-03")),
  list(begin = as.Date("2021-11-17"), end = as.Date("2022-02-22")),
  list(begin = as.Date("2022-02-23"), end = as.Date("2022-06-09")),
  list(begin = as.Date("2022-06-10"), end = as.Date("2022-12-31")),
  list(begin = as.Date("2022-06-10"), end = as.Date("2024-07-24")),
  list(begin = as.Date("2023-01-01"), end = as.Date("2024-01-01")),
  list(begin = as.Date("2023-01-01"), end = as.Date("2024-07-24"))
) -> groups_for_avg

groups_for_avg |> 
  map(\(.g) {
    kev |> 
      filter(
        between(dateAdded, .g$begin, .g$end)
      ) |> 
      mutate(
        cve_age_at_kev_drop = dateAdded - publishedDate
      ) |> 
      pull(cve_age_at_kev_drop) -> .x

    ifelse(.x < 0, 0, .x)
    
  }) -> ages_per_group

map(1:6, \(.i) {
  ggplot() +
    geom_quasirandom(
      aes("", ages_per_group[[.i]]),
      size = 1/2
    ) +
    geom_text(
      aes(
        x = 0, y = 7000, 
        label = sprintf("Mean: %s", scales::comma(trunc(mean(ages_per_group[[.i]])))),
      ),
      color = "blue",
      hjust = 0
    ) +
    geom_text(
      aes(
        x = 0, y = 5000, 
        label = sprintf("Median: %s", scales::comma(trunc(median(ages_per_group[[.i]])))),
      ),
      color = "red",
      hjust = 0
    ) +
    geom_hline(
      yintercept = mean(ages_per_group[[.i]]),
      colour = "blue"
    ) +
    geom_hline(
      yintercept = median(ages_per_group[[.i]]),
      colour = "red"
    ) +
    scale_y_comma(
      limits = c(0, 8000)
    ) +
    labs(
      x = NULL, y = "CVE Age",
      title = sprintf("%s — %s", groups_for_avg[[.i]]$begin, groups_for_avg[[.i]]$end)
    ) +
    theme_ipsum_gs(grid="Y")
}) -> g

g[[1]] + g[[2]] + g[[3]] + g[[4]] + g[[5]] + g[[6]] + plot_layout(ncol=3)

kev |> 
  mutate(
    cve_age_at_kev_drop = dateAdded - publishedDate,
    qtr = quarter(dateAdded, type = "date_last")
  ) |> 
  select(
    qtr,
    cve_age_at_kev_drop
  ) |> 
  ggplot() +
  geom_quasirandom(
    aes(qtr, cve_age_at_kev_drop),
    size = 1/2
  ) +
  scale_y_comma() +
  theme_ipsum_gs(grid="Y")


list(
  list(begin = as.Date("2021-11-03"), end = as.Date("2021-11-03")),
  list(begin = as.Date("2021-11-17"), end = as.Date("2022-02-22")),
  list(begin = as.Date("2022-02-23"), end = as.Date("2022-06-09")),
  list(begin = as.Date("2022-06-10"), end = as.Date("2022-12-31")),
  list(begin = as.Date("2022-06-10"), end = as.Date("2024-07-24")),
  list(begin = as.Date("2023-01-01"), end = as.Date("2024-01-01")),
  list(begin = as.Date("2023-01-01"), end = as.Date("2024-07-24"))
) -> groups_for_avg

kev |> 
  mutate(
    group = case_when(
      dateAdded == as.Date("2021-11-03") ~ "\nFirst KEV",
      between(dateAdded, as.Date("2022-02-23"), as.Date("2022-06-09")) ~ "\nUKR Conflict",
      between(dateAdded, as.Date("2021-11-17"), as.Date("2022-02-22")) |
        dateAdded >= as.Date("2022-06-10") ~ "\nEverything Else"
    ) |> 
      factor(
        levels = c("\nFirst KEV", "\nUKR Conflict", "\nEverything Else")
      )
  ) |> 
  filter(!is.na(label)) -> kev

kev |> 
  mutate(
    cve_age_at_kev_drop = dateAdded - publishedDate,
  ) |> 
  select(
    group,
    cve_age_at_kev_drop
  ) |> 
  ggplot() +
  geom_quasirandom(
    aes(group, cve_age_at_kev_drop),
    size = 1,
    color = yellow_j
  ) +
  scale_y_continuous(
    expand = c(0,0,0,0),
    breaks = c(365, 365*3, 365*5, 365*10, 365*20),
    labels = c("1 year", "3 years", "5 years", "10 years", "20 years")
  ) +
  labs(
    x = NULL, y = NULL,
    title = "KEV CVE Age By Group\n"
  ) +
  theme_ipsum_gs(grid="Y") +
  theme_jeopardy() +
  theme(
    axis.line.x.bottom = element_line(color = "white", size = 1/2)
  )

kev |> 
  mutate(
    cve_age_at_kev_drop = dateAdded - publishedDate,
  ) -> kev_source_of_truth

saveRDS(kev_source_of_truth, "kev-source-of-truth.rds")


geom_violin(
  aes("", cvss),
  fill = alpha(orange_j, 1/4),
  color = yellow_j,
  size = 1
) +
geom_quasirandom(
  aes("", cvss),
  color = orange_j
) 