
## Loading in libraries

library(rvest)
library(tidyverse)
library(jsonlite)
library(DatawRappr)


## Load in measles files from JSON - this is weekly cases from Jan 2023 to now

weekly_us <- fromJSON("https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesWeekly.json")


## Changing week end to date and also making new dates so that it can be added to the note

weekly_us <- weekly_us %>% mutate(week_end = as.Date(week_end)) %>% filter(week_start > "2023-12-30")

weekly_us <- weekly_us %>% select(week_end, cases)

dates <- weekly_us %>% mutate(week_end2 = format(week_end, format = "%B %d"))

dates <- dates %>% mutate(week_end2 = gsub(" 0", " ", dates$week_end2))


# Load in API key and chart key

api_key <- Sys.getenv("API_KEY")
measles_weekly <- Sys.getenv("CHART_KEY")

datawrapper_auth(api_key =  api_key, overwrite=TRUE)


# Update measles weekly bar chart, also update time stamp on chart

dw_data_to_chart(weekly_us, measles_weekly)


# Pull out last date

last_date = tail(dates$week_end2, n=1)


# Edit metadata for creation (comment out once done the first time)

dw_edit_chart(measles_weekly, 
              title = "Confirmed weekly cases of measles in the U.S.",
              intro = paste("<i>Data as of the week ending",last_date,"</i>"),
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
                "base-color" = "#008495"
              )
              
)



# publish chart

dw_publish_chart(measles_weekly)
