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

latest_kev <- read_csv("~/gn/labs-viz-data/docs/kev.csv")

kev |> 
  rename(
    cve_id = "label"
  ) |> 
  left_join(
    latest_kev |> 
      select(
        cve_id = cveID,
        dueDate
      )
  ) |> 
  mutate(
    days_to_fix = dueDate - dateAdded,
    year = year(dateAdded)
  ) |> 
  select(
    group,
    dateAdded,
    days_to_fix
  ) -> kev_due_delta

kev_due_delta |> 
  mutate(
    length = ifelse(days_to_fix <= 14, "Short", "Long"),
    days_to_fix = as.numeric(days_to_fix)
  ) |> 
  filter(
    days_to_fix >= 0
  ) |> 
  ggplot() +
  geom_vline(
    xintercept = c(as.Date("2022-02-23"), as.Date("2022-06-09")),
    linetype = "dotted"
  ) +
  geom_point(
    aes(dateAdded, days_to_fix, color = length)
  ) +
  scale_color_manual(
    values = c(
      "Short" = "red",
      "Long" = "steelblue"
    )
  ) +
  scale_y_continuous(
    trans = "log10",
    label = scales::comma_format()
  ) +
  theme_ipsum_gs(grid="XY")

kev |> 
  rename(
    cve_id = "label"
  ) |> 
    left_join(
      latest_kev |> 
        select(
          cve_id = cveID,
          dueDate
        )
    ) |> 
  mutate(
    days_to_fix = as.numeric(dueDate - dateAdded),
    year = year(dateAdded),
    qtr = quarter(dateAdded, type = "date_last"),
    dow = wday(dateAdded, label = TRUE, abbr = FALSE) |> 
      factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") |> rev())
  ) |>
  filter(year != 2021) |> 
  count(dateAdded, days_to_fix, dow) |> 
  filter(dow == "Friday") |> 
  arrange(
    desc(dateAdded)
  ) |> 
  count(days_to_fix, sort=TRUE)


kev |> 
  rename(
    cve_id = "label"
  ) |> 
  mutate(
    year = year(dateAdded),
    qtr = quarter(dateAdded, type = "date_last"),
    dow = wday(dateAdded, label = TRUE, abbr = FALSE) |> 
      factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") |> rev())
  ) |>
  filter(year != 2021) |> 
  count(dateAdded, dow) |> 
  ggplot() +
  geom_point(
    aes(dateAdded, dow, size = n),
    shape = 21,
    stroke = 3/4,
    color = "white",
    fill = alpha("steelblue", 4/5),
    show.legend = FALSE
  ) +
  scale_x_date(
    position = "top",
    sec.axis = dup_axis()
  ) +
  scale_size_area(
    max_size = 20
  ) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_ipsum_gs(grid="XY")

kev |> 
  rename(
    cve_id = "label"
  ) |> 
  mutate(
    year = year(dateAdded),
    qtr = quarter(dateAdded, type = "date_last"),
    dow = wday(dateAdded, label = TRUE, abbr = FALSE) |> 
      factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") |> rev())
  ) |>
  filter(year != 2021) |> 
  count(qtr, dow) |> 
  group_by(qtr) |> 
  mutate(
    pct = proportions(n) * 100
  ) |> 
  ungroup() |> 
  complete(qtr, dow) |> 
  ggplot() +
  geom_tile(
    aes(qtr, dow, fill = pct),
    color = "white",
    size = 1
  ) +
  geom_text(
    aes(
      qtr, dow, 
      label = sprintf("%s\n(%s)", n, scales::percent(pct/100, 1)),
      color = I(ifelse(pct < 30, "white", "black"))
    ),
  ) +
  scale_x_date(
    expand = c(0,0,0,0),
    position = "top"
  ) +
  scale_fill_viridis_c(
    begin = 0.2,
    end = 0.9,
    option = "magma",
    na.value = "grey80",
    trans = "log10"
  ) +
  theme_ipsum_gs(grid="")
