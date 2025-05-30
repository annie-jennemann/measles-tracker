## Loading in libraries

library(rvest)
library(tidyverse)
library(jsonlite)
library(DatawRappr)

## Load in API key, chart keys and data keys

api_key <- Sys.getenv("API_KEY")
measles_weekly <- Sys.getenv("CHART_KEY")
measles_map <- Sys.getenv("CHART_KEY1")
measles_annual <- Sys.getenv("CHART_KEY2")

datawrapper_auth(api_key =  api_key, overwrite=TRUE)

########### WEEKLY BAR CHART ###########

## load data

weekly_us <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesWeekly.json") %>%
  mutate(week_end = as.Date(week_end)) %>%
  filter(week_start > "2023-12-30") %>%
  select(week_end, cases)

previous_data_path <- "last_weekly_us.csv"

if (file.exists(previous_data_path)) {
  previous_data <- read_csv(previous_data_path, col_types = cols())
} else {
  previous_data <- tibble()
}

normalize_df <- function(df) {
  df %>%
    mutate(
      week_end = as.Date(week_end),
      cases = as.numeric(cases)
    ) %>%
    arrange(week_end) %>%
    select(week_end, cases)
}

weekly_us <- normalize_df(weekly_us)
previous_data <- normalize_df(previous_data)

comparison_result <- all.equal(weekly_us, previous_data, check.attributes = FALSE)
print(comparison_result)

data_changed <- !isTRUE(comparison_result)
print(paste("Data changed:", data_changed))


if (data_changed) {
  message("Data has changed. Updating chart and timestamp...")
  
  # Save current data for next comparison
  write_csv(weekly_us, previous_data_path)
  
  # Update Datawrapper chart
  dw_data_to_chart(weekly_us, measles_weekly)
  
  
  dates <- weekly_us %>% mutate(week_end2 = format(week_end, format = "%B %d"))
  
  dates <- dates %>% mutate(week_end2 = gsub(" 0", " ", dates$week_end2))
  
  # Pull out last date
  
  last_date = tail(dates$week_end2, n=1)
  
  ## pull total cases
  
  total_cases_25 <- weekly_us %>% filter(week_end > "2024-12-28") %>% mutate(cases = as.numeric(cases)) %>% summarise(total_cases = sum(cases))
  
  total_cases_25 <- prettyNum(total_cases_25, big.mark = ",", scientific = FALSE)
  
  ## pull today's date
  
  today <- format(Sys.Date(), "%B %d")
  
  dw_edit_chart(measles_weekly, 
                title = "Confirmed weekly cases of measles in the U.S.",
                source_name = "Centers for Disease Control and Prevention",
                byline = "Annie Jennemann/Hearst TV",
                intro = paste("There have been ",total_cases_25,"positive measles cases in 2025."),
                annotate = paste("<i>CDC data as of",today,".<br>Data by last day of the week.</i>"),
                publish = list(
                  "blocks" = list(
                    "get-the-data" = FALSE)
                ),
                data = list(
                  "column-format" = list(
                    "week_end" = list(
                      "type" = "date"
                    )
                  )
                ),
                visualize = list(
                  "base-color" = "#D6842F"
                )
  )
  
  dw_publish_chart(measles_weekly)
  
  system('git config --global user.name "github-actions"')
  system('git config --global user.email "github-actions@github.com"')
  system("git add last_weekly_us.csv")
  system('git commit -m "Update saved weekly measles data snapshot" || echo "No changes to commit"')
  system("git push")
  message("CSV committed and pushed.")
  
} else {
  message("No changes detected in data. Skipping update.")
}


########### BINNED US MAP ###########

state <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesMap.json") %>%
  filter(year == "2025")

# Define snapshot path
map_data_path <- "last_map_us.csv"

# Load previous snapshot if it exists
if (file.exists(map_data_path)) {
  previous_state <- read_csv(map_data_path, col_types = cols())
  message("previous state-level data loaded.")
} else {
  previous_state <- tibble()
  message("No previous state-level data found.")
}

# Normalize and sort data
normalize_map <- function(df) {
  df %>%
    mutate(across(everything(), as.character)) %>%
    arrange(state)
}

state <- normalize_map(state)
previous_state <- normalize_map(previous_state)

# Compare
map_changed <- !isTRUE(all.equal(state, previous_state, check.attributes = FALSE))
print(paste(" Map data changed:", map_changed))

if (map_changed) {
  message("State data has changed. Updating map...")
  
  # Save new snapshot
  write_csv(state, map_data_path)
  
  # Count states with non-zero cases
  count_states <- state %>% filter(cases_range != "0") %>% summarise(count = n()) %>% pull(count)
  
  # Update chart
  dw_data_to_chart(state, measles_map)
  
  dw_edit_chart(
    measles_map,
    title = "Confirmed measles cases by state",
    source_name = "Centers for Disease Control and Prevention",
    byline = "Annie Jennemann/Hearst TV",
    intro = paste("There have been", count_states, "states with positive cases of measles in 2025.", '<br><br>
<b style="border-right:18px solid #feedde;"></b>&nbsp;0&nbsp;&nbsp;
<b style="border-right:18px solid #fdd0a2;"></b>&nbsp;1-9&nbsp;&nbsp;
<b style="border-right:18px solid #fdae6b;"></b>&nbsp;10-49&nbsp;&nbsp;
<b style="border-right:18px solid #fd8d3c;"></b>&nbsp;50-99&nbsp;&nbsp;
<b style="border-right:18px solid #e6550d;"></b>&nbsp;100-249&nbsp;&nbsp;
<b style="border-right:18px solid #a63603;"></b>&nbsp;250+'),
    annotate = paste("<i>CDC data as of",today,"</i>"),
    publish = list(blocks = list("get-the-data" = FALSE)),
    data = list("column-format" = list("week_end" = list(type = "date"))),
    visualize = list(
      group = "cases_range",
      "map-color-group" = "cases_range",
      colorscale = list(
        enabled = TRUE,
        map = list(
          "250+" = "#a63603",
          "100-249" = "#e6550d",
          "50-99" = "#fd8d3c",
          "10-49" = "#fdae6b",
          "1-9" = "#fdd0a2",
          "0" = "#feedde"
        )
      )
    )
  )
  
  dw_publish_chart(measles_map)
  
  # Commit snapshot
  system("git add last_map_us.csv")
  system('git commit -m "Update state-level measles map snapshot" || echo "No changes to commit"')
  system("git push")
  
} else {
  message("No changes detected in state map data.")
}

########### YEAR BAR CHART ###########

# ---------- YEAR BAR CHART ---------- #

# Load annual cases
annual_cases <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesYear.json") %>%
  filter(filter == "2000-Present*")

annual_data_path <- "last_annual_us.csv"

# Load previous
if (file.exists(annual_data_path)) {
  previous_annual <- read_csv(annual_data_path, col_types = cols())
  message("previous annual data loaded.")
} else {
  previous_annual <- tibble()
  message("No previous annual data found.")
}

# Normalize
normalize_annual <- function(df) {
  df %>%
    mutate(across(everything(), as.character)) %>%
    arrange(year)
}

annual_cases <- normalize_annual(annual_cases)
previous_annual <- normalize_annual(previous_annual)

# Compare
annual_changed <- !isTRUE(all.equal(annual_cases, previous_annual, check.attributes = FALSE))
print(paste("Annual data changed:", annual_changed))

if (annual_changed) {
  message("Annual data changed. Updating chart...")
  
  write_csv(annual_cases, annual_data_path)
  
  dw_data_to_chart(annual_cases, measles_annual)
  
  dw_edit_chart(
    measles_annual,
    title = "Confirmed measles cases by year, 2000–2025",
    source_name = "Centers for Disease Control and Prevention",
    byline = "Annie Jennemann/Hearst TV",
    intro = paste("There have been", total_cases_25, "positive measles cases in 2025."),
    annotate = paste("<i>CDC data as of",today,"</i>"),
    publish = list(blocks = list("get-the-data" = FALSE)),
    visualize = list("base-color" = "#D6842F")
  )
  
  dw_publish_chart(measles_annual)
  
  system("git add last_annual_us.csv")
  system('git commit -m "Update annual measles snapshot" || echo "No changes to commit"')
  system("git push")
  
} else {
  message("No changes detected in annual data.")
}
