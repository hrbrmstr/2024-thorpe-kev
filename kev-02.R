library(tidyverse)
library(patchwork)
library(ggbeeswarm)
library(hrbrthemes)

source("theme-j.R")

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
    fill = alpha(orange_j, 1/4),
    color = yellow_j,
    size = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, 1),
    limits = c(0, 10)
  ) +
  scale_y_comma() +
  labs(
    x = NULL, y = "# CVEs",
    title = "CVSS Score Distribution Of KEV",
    subtitle = "CVSS Score (V2 when no V3 available)"
  ) +
  theme_jeopardy() 

for_cvss |> 
  ggplot() +
  geom_violin(
    aes("", cvss),
    fill = alpha(orange_j, 1/4),
    color = yellow_j,
    size = 1
  ) +
  geom_quasirandom(
    aes("", cvss),
    color = orange_j
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 1),
    limits = c(0, 10)
  ) +
  # scale_x_comma() +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "CVSS Score Distribution Of KEV",
    subtitle = "CVSS Score (V2 when no V3 available)"
  ) +
  theme_ipsum_gs() +
  theme_jeopardy() 

for_cvss |> 
  ggplot() +
  geom_density(
    aes(cvss, y = stat(count)),
    color = yellow_j,
    fill = alpha(orange_j, 1/4),
    size = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, 1),
    limits = c(0, 10)
  ) +
  scale_y_comma() +
  facet_wrap(~group, ncol=1) +
  labs(
    x = NULL, y = NULL,
    title = "CVSS Score Distribution Of KEV By Group"
  ) +
  theme_ipsum_gs(grid="XY") +
  theme_jeopardy() +
  theme(
    panel.spacing.y = unit(3/4, "in")
  )

for_cvss |> 
  mutate(
    delta = dateAdded - publishedDate
  ) |> 
  filter(
    delta > 0,
    !is.na(delta)
  ) |> 
  ggplot() +
  geom_vline(
    xintercept = c(365, 365*3, 365*5, 365*10, 365*20),
    color = orange_j
  ) +
  geom_label(
    data = data.frame(
      x = c(365, 365*3, 365*5, 365*10, 365*20),
      y = rep(170, 5),
      label = c("  1 year", "  3 years", "  5 years", "  10 years", "20 years   "),
      hjust = c(0, 0, 0, 0, 1)
    ),
    aes(
      x = x, y = y, label = label, hjust = hjust
    ), 
    vjust = 1,
    color = yellow_j,
    label.size = 0,
    fill = "#231ab6"
  ) +  
  geom_vline(
    xintercept = c(365, 365*3, 365*5, 365*10, 365*20),
    color = orange_j
  ) +
  geom_histogram(
    aes(delta),
    fill = alpha(orange_j, 1/4),
    color = yellow_j
  ) +
  scale_x_comma() +
  scale_y_comma() +
  facet_wrap(~group, ncol=1) +
  labs(
    x = "CVE Age", y = "# CVEs",
    title = "CVE Age Distribution Of KEV By Group"
  ) +
  theme_ipsum_gs(grid="XY") +
  theme_jeopardy() +
  theme(
    panel.spacing.y = unit(3/4, "in"),
    axis.text.x.bottom = element_blank()
  )

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
    fill = alpha(orange_j, 1/4),
    color = yellow_j,
    size = 0.25
  ) +
  scale_x_comma(
    breaks = c(365, 365*10, 365*20),
    labels = c("1\nyear(s)", "10", "20")
  ) +
  facet_wrap(~pubYear, nrow=1) +
  labs(
    x = "CVSS Age", y = "# CVEs",
    title = "CVSS Age Distribution Of KEV Year"
  ) +
  theme_ipsum_gs(grid="Y") +
  theme_jeopardy()


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
    length = ifelse(days_to_fix <= 10, "Short", "Long"),
    days_to_fix = as.numeric(days_to_fix)
  ) |> 
  filter(
    days_to_fix >= 0
  ) |> 
  ggplot() +
  geom_vline(
    xintercept = c(as.Date("2022-02-23"), as.Date("2022-06-09")),
    linetype = "dotted",
    color = orange_j
  ) +
  geom_text(
    data = data.frame(
      x = as.Date("2022-04-17"),
      y = 70,
      label = "← UKR KEV Drops →"
    ),
    aes(
      x = x, y = y, label = label
    ),
    color = orange_j
  ) +
  # geom_point(
  #   aes(dateAdded, days_to_fix),
  #   color = yellow_j,
  #   show.legend = FALSE
  # ) +
  geom_point(
    aes(dateAdded, days_to_fix, color = length),
    alpha = 1,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Short" = yellow_j,
      "Long" = pink_j
    )
  ) +
  scale_y_continuous(
    trans = "log10",
    label = scales::comma_format()
  ) +
  labs(
    x = NULL, y = "# Days",
    title = "Days To Fix"
  ) +
  theme_ipsum_gs(grid="XY") +
  theme_jeopardy()

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
    stroke = 2/4,
    color = "white",
    fill = alpha(purple_j, 4/5),
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
    x = NULL, y = NULL,
    title = "KEV Drops By Weekday"
  ) +
  theme_ipsum_gs(grid="XY") +
  theme_jeopardy()

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
