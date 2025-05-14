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

weekly_us <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesWeekly.json")

## Changing week end to date and also making new dates so that it can be added to the note

weekly_us <- weekly_us %>% mutate(week_end = as.Date(week_end)) %>% filter(week_start > "2023-12-30")

weekly_us <- weekly_us %>% select(week_end, cases)

dates <- weekly_us %>% mutate(week_end2 = format(week_end, format = "%B %d"))

dates <- dates %>% mutate(week_end2 = gsub(" 0", " ", dates$week_end2))

## Update measles weekly bar chart, also update time stamp on chart

dw_data_to_chart(weekly_us, measles_weekly)

# Pull out last date

last_date = tail(dates$week_end2, n=1)

## pull total cases

total_cases_25 <- weekly_us %>% filter(week_end > "2024-12-28") %>% mutate(cases = as.numeric(cases)) %>% summarise(total_cases = sum(cases))

total_cases_25 <- prettyNum(total_cases_25, big.mark = ",", scientific = FALSE)

## pull today's date

today <- Sys.Date()

today <- data_frame(today)

today <- today %>% mutate(today = format(today, format = "%B %d"))

## chart properties

dw_edit_chart(measles_weekly, 
              title = "Confirmed weekly cases of measles in the U.S.",
              source_name = "Centers for Disease Control and Prevention",
              byline = "Annie Jennemann/Hearst TV",
              intro = paste("There have been ",total_cases_25,"positive measles cases in 2025."),
              annotate = paste("<i>Chart last updated",today,"<br>Last data reported for the week ending ",last_date,"</i>"),
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

## publish chart

dw_publish_chart(measles_weekly)

########### BINNED US MAP ###########

## load data

state <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesMap.json") %>% filter(year == "2025")

## Update data

dw_data_to_chart(state, measles_map)

## states with cases

count_states <- state %>% filter(cases_range != "0") %>% summarise(count = n())

## chart properties

dw_edit_chart(
  measles_map,
  title = "Confirmed measles cases by state",
  source_name = "Centers for Disease Control and Prevention",
  byline = "Annie Jennemann/Hearst TV",
  intro = paste("There have been",count_states," states with positive cases of measles in 2025.",'<br><br>
<b style="border-right:18px solid #feedde;"></b>&nbsp;0&nbsp;&nbsp;
<b style="border-right:18px solid #fdd0a2;"></b>&nbsp;1-9&nbsp;&nbsp;
<b style="border-right:18px solid #fdae6b;"></b>&nbsp;10-49&nbsp;&nbsp;
<b style="border-right:18px solid #fd8d3c;"></b>&nbsp;50-99&nbsp;&nbsp;
<b style="border-right:18px solid #e6550d;"></b>&nbsp;100-249&nbsp;&nbsp;
<b style="border-right:18px solid #a63603;"></b>&nbsp;250+'),
  annotate = paste("<i>Chart last updated",today,".<br>CDC data updates weekly on Friday's."),
  publish = list(
    blocks = list("get-the-data" = FALSE)
  ),
  data = list(
    "column-format" = list(
      "week_end" = list(type = "date")
    )
  ),
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

## publish chart

dw_publish_chart(measles_map)

########### YEAR BAR CHART ###########

## load data

annual_cases <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesYear.json") %>% filter(filter == "2000-Present*")

## update data

dw_data_to_chart(annual_cases, measles_annual)

## chart properties

dw_edit_chart(
  measles_annual,
  title = "Confirmed measles cases by year, 2000-2025",
  source_name = "Centers for Disease Control and Prevention",
  byline = "Annie Jennemann/Hearst TV",
  intro = paste("There have been ",total_cases_25,"positive measles cases in 2025."),
  annotate = paste("<i>Chart last updated",today,".<br>CDC data updates weekly on Friday's."),
  publish = list(
    blocks = list("get-the-data" = FALSE)
  ),
  visualize = list(
    "base-color" = "#D6842F"
  )
)

## publish chart

dw_publish_chart(measles_annual)
