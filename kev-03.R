library(tidyverse)

list.files(
  path = "~/Development/cisa-known-exploited-vulns/docs",
  "\\d+-\\d+-\\d+-cisa-kev.csv",
  full.names = TRUE
) |> 
map_df(\(.x) {
  suppressWarnings(suppressMessages(read.csv(
    .x, header=TRUE
  ))) -> .y
  
  if ("Date.Added.to.Catalog" %in% colnames(.y)) {
    .y |> 
    rename(
      dateAdded = Date.Added.to.Catalog
    ) -> .y  
  }
  
  if ("CVE" %in% colnames(.y)) {
    .y |> 
    rename(
      cveID = CVE
    ) -> .y  
  }
  
  .y |> 
  mutate(
    day = .x |> basename() |> substring(1, 10) |> as.Date()
  )
}, .progress = TRUE) -> xdf

xdf |> 
  select(day, cveID, dateAdded, k=knownRansomwareCampaignUse) |> 
  left_join(
    data.frame(
      dateAdded = sort(unique(xdf$dateAdded)),
      actualDateAdded = sort(unique(xdf$dateAdded)) |> 
        lubridate::parse_date_time(
          c("%m/%d/%Y", "%Y-%m-%d", "%d-%b-%y")
        ) |> as.Date()
    )    
  ) |> 
  select(-dateAdded) |> 
  mutate(
    k = ifelse(is.na(k), "Unassigned", k)
  ) |> 
  arrange(day) |> 
  group_by(cveID, k) |> 
  slice(1) |> 
  ungroup() |> 
  spread(k, day) |> 
  filter(
    !is.na(Known)
  ) |> 
  filter(!is.na(Unknown)) |> 
  select(-Unassigned) |> 
  print(n=41) |> 
  mutate(
    delta = Known - Unknown
  ) |> 
  print(n=100) |> 
  arrange(
    delta
  ) |> 
  print(n=100)