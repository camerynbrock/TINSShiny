###
# Attach packages
###

library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(janitor)
library(lubridate)
library(tsibble)
library(leaflet)
library(sf)
library(tmap)
library(RColorBrewer)
library(rsconnect)

###
### Data wrangling
###

# Read in and clean up data to combine separate year files to one df

tins_2010 <- read_csv(here::here("RawData", "2010.csv"), 
                      col_types = cols(.default = "c")) %>%  
  select(-c(29, 30, 32)) %>% 
  head(212) 

tins_2011 <- read_csv(here::here("RawData", "2011.csv"), 
                      col_types = cols(.default = "c")) %>% 
  select(-c(29, 30, 32)) 

tins_2012 <- read_csv(here::here("RawData", "2012.csv"), 
                      col_types = cols(.default = "c"))

tins_2013 <- read_csv(here::here("RawData", "2013.csv"), 
                      col_types = cols(.default = "c"))

tins_2014 <- read_csv(here::here("RawData", "2014.csv"), 
                      col_types = cols(.default = "c"))

tins_2015 <- read_csv(here::here("RawData", "datafixed2015.csv"), 
                      col_types = cols(.default = "c")) %>% 
  select(-c(25:27)) %>% 
  rename("NOTE?" = "Comment")

tins_2016 <- read_csv(here::here("RawData", "2016.csv"), 
                      col_types = cols(.default = "c")) %>% 
  select(-c(25:27)) %>% 
  head(778) %>% 
  rename("NOTE?" = "Comment")

tins_2017 <- read_csv(here::here("RawData", "2017.csv"), 
                      col_types = cols(.default = "c")) %>% 
  head(167) %>% 
  rename("NOTE?" = "Comment")

tins_2018 <- read_csv(here::here("RawData", "2018.csv"), 
                      col_types = cols(.default = "c")) %>% 
  head(200) %>% 
  rename("NOTE?" = "Comment")

tins_2019 <- read_csv(here::here("RawData", "2019.csv"), 
                      col_types = cols(.default = "c")) %>% 
  head(29) %>% 
  rename("NOTE?" = "Comment")

# Read in and clean up net hours data

nethours_daily <- read_csv(here::here("RawData", "Nethours2010.csv")) %>% 
  clean_names() %>% 
  mutate(date = mdy(date))

# bind all years

tins <- bind_rows(tins_2010,
                  tins_2011,
                  tins_2012,
                  tins_2013,
                  tins_2014,
                  tins_2015,
                  tins_2016,
                  tins_2017,
                  tins_2018,
                  tins_2019)

# Tidy tins

tins_tidy <- tins %>% 
  clean_names() %>% 
  mutate(wing = as.numeric(wing),
         wght = as.numeric(wght)) %>% 
  mutate(date = lubridate::mdy(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         doy = lubridate::yday(date)) %>% 
  filter(!year == "2003") %>% 
  mutate(spec = case_when(spec %in% c("AUWA", "MYWA", "YRWA") ~ "YRWA",
                          spec %in% c("GWCS", "MWCS", "WCSP") ~ "WCSP",
                          T ~ spec)) %>% 
  filter(!spec == "NA")

# Did the case_when to remove subspecies & only include species
# Checked all species codes to ensure no more duplicates or typos
# Some that weren't typical: 
# We have RSFL instead of NOFL (red shafted flicker instead of northern)
# We have WEFL (western flycatcher) - it has since been split into PSFL and COFL
# We have YWAR for yellow warbler, looks like it is sometimes YEWA? 

# Update sites and time of interest
# Add taxonomy and viewer-friendly labels

tins_roi <- tins_tidy %>% 
  filter(loc %in% c("COEA", "BLCA", "POMA")) %>% 
  filter(month %in% c(8, 9, 10)) %>% 
  mutate(family = case_when(spec %in% c("STJA") ~ "Corvidae",
                            spec %in% c("RSFL", "WISA", "DOWO",
                                        "RBSA") ~ "Picidae",
                            spec %in% c("SSHA", "COHA") ~ "Accipitridae",
                            spec %in% c("AMRO", "HETH") ~ "Turdidae",
                            spec %in% c("BHGR", "WETA", "LAZB") ~ "Cardinalidae",
                            spec %in% c("GTTO", "WCSP", "SOSP", 
                                        "SAVS", "LISP", "BRSP",
                                        "ORJU", "SPTO", "GCSP",
                                        "CHSP", "FOSP", "LASP") ~ "Passerellidae",
                            spec %in% c("MOCH") ~ "Paridae",
                            spec %in% c("OSFL", "WIFL", "GRFL", 
                                        "WEWP", "DUFL", "HAFL",
                                        "BLPH", "WEFL") ~ "Tyrannidae",
                            spec %in% c("WAVI", "CAVI") ~ "Vireonidae",
                            spec %in% c("BCHU", "RUHU", "ANHU") ~ "Trochilidae",
                            spec %in% c("OCWA", "YWAR", "YRWA",
                                        "MGWA", "WIWA", "BTYW",
                                        "COYE", "NAWA", "HEWA") ~ "Parulidae", 
                            spec %in% c("PUFI", "PISI", "EVGR",
                                        "LEGO") ~ "Fringillidae",
                            spec %in% c("RCKI", "GCKI") ~ "Regulidae",
                            spec %in% c("RBNU", "PYNU") ~ "Sittidae",
                            spec %in% c("BRCR") ~ "Certhiidae",
                            spec %in% c("BUSH") ~"Aegithalidae",
                            spec %in% c("BHCO", "BUOR") ~ "Icteridae",
                            spec %in% c("MAWR", "HOWR", "BEWR") ~ "Troglodytidae",
                            spec %in% c("BARS") ~ "Hirundinidae",
                            spec %in% c("WISN") ~ "Scolopacidae",
                            spec %in% c("MODO") ~ "Columbidae",
                            spec %in% c("BGGN") ~ "Polioptilidae")) %>% 
  mutate(family_label = case_when(family == "Corvidae" ~ "Corvidae: Crows, Jays, & Magpies",
                                  family == "Picidae" ~ "Picidae: Woodpeckers",
                                  family == "Accipitridae" ~ "Accipitridae: Hawks, Eagles, & Kites",
                                  family == "Turdidae" ~ "Turdidae: Thrushes & Allies",
                                  family == "Cardinalidae" ~ "Cardinalidae: Cardinals & Allies",
                                  family == "Passerellidae" ~ "Passerellidae: Sparrows",
                                  family == "Paridae" ~ "Paridae: Chickadees & Titmice",
                                  family == "Tyrannidae" ~ "Tyrannidae: Tyrant Flycatchers",
                                  family == "Vireonidae" ~ "Vireonidae: Vireos",
                                  family == "Trochilidae" ~ "Trochilidae: Hummingbirds",
                                  family == "Parulidae" ~ "Parulidae: Warblers",
                                  family == "Fringillidae" ~ "Fringillidae: Finches & Allies",
                                  family == "Regulidae" ~ "Regulidae: Kinglets",
                                  family == "Sittidae" ~ "Sittidae: Nuthatches",
                                  family == "Certhiidae" ~ "Certhiidae: Treecreepers",
                                  family == "Aegithalidae" ~ "Aegithdalidae: Long-tailed Tits",
                                  family == "Icteridae" ~ "Icteridae: Blackbirds & Orioles",
                                  family == "Troglodytidae" ~ "Troglodytidae: Wrens",
                                  family == "Hirundinidae" ~ "Hirundinidae: Swallows",
                                  family == "Scolopacidae" ~ "Scolopacidae: Sandpipers & Allies",
                                  family == "Columbidae" ~ "Columbidae: Pigeons & Doves",
                                  family == "Polioptilidae" ~ "Polioptilidae: Gnatcatchers")) %>% 
  mutate(spec_label = case_when(spec == "STJA" ~ "Steller's Jay",
                                spec == "RSFL" ~ "Northern Flicker",
                                spec == "WISA" ~ "Williamson's Sapsucker",
                                spec == "DOWO" ~ "Downy Woodpecker",
                                spec == "RBSA" ~ "Red-breasted Sapsucker",
                                spec == "SSHA" ~ "Sharp-shinned Hawk",
                                spec == "COHA" ~ "Cooper's Hawk",
                                spec == "AMRO" ~ "American Robin",
                                spec == "HETH" ~ "Hermit Thrush",
                                spec == "BHGR" ~ "Black-headed Grosbeak",
                                spec == "WETA" ~ "Western Tanager",
                                spec == "LAZB" ~ "Lazuli Bunting",
                                spec == "GTTO" ~ "Green-tailed Towhee",
                                spec == "WCSP" ~ "White-crowned Sparrow",
                                spec == "SOSP" ~ "Song Sparrow",
                                spec == "SAVS" ~ "Savannah Sparrow",
                                spec == "LISP" ~ "Lincoln's Sparrow",
                                spec == "BRSP" ~ "Brewer's Sparrow",
                                spec == "ORJU" ~ "Dark-eyed Junco",
                                spec == "SPTO" ~ "Spotted Towhee",
                                spec == "GCSP" ~ "Golden-crowned Sparrow",
                                spec == "CHSP" ~ "Chipping Sparrow",
                                spec == "FOSP" ~ "Fox Sparrow",
                                spec == "LASP" ~ "Lark Sparrow",
                                spec == "MOCH" ~ "Mountain Chickadee",
                                spec == "OSFL" ~ "Olive-sided Flycatcher",
                                spec == "WIFL" ~ "Willow Flycatcher",
                                spec == "GRFL" ~ "Gray Flycatcher",
                                spec == "WEWP" ~ "Western Wood-Pewee",
                                spec == "DUFL" ~ "Dusky Flycatcher",
                                spec == "HAFL" ~ "Hammond's Flycatcher",
                                spec == "BLPH" ~ "Black Phoebe",
                                spec == "WEFL" ~ "Pacific-slope Flycatcher", 
                                spec == "WAVI" ~ "Warbling Vireo",
                                spec == "CAVI" ~ "Cassin's Vireo",
                                spec == "BCHU" ~ "Black-chinned Hummingbird",
                                spec == "RUHU" ~ "Rufous Hummingbird",
                                spec == "ANHU" ~ "Anna's Hummingbird",
                                spec == "OCWA" ~ "Orange-crowned Warbler",
                                spec == "YWAR" ~ "Yellow Warbler",
                                spec == "YRWA" ~ "Yellow-rumped Warbler",
                                spec == "MGWA" ~ "MacGillivray's Warbler",
                                spec == "WIWA" ~ "Wilson's Warbler",
                                spec == "BTYW" ~ "Black-throated Gray Warbler",
                                spec == "COYE" ~ "Common Yellowthroat",
                                spec == "NAWA" ~ "Nashville Warbler",
                                spec == "HEWA" ~ "Hermit Warbler",
                                spec == "PUFI" ~ "Purple Finch",
                                spec == "PISI" ~ "Pine Siskin",
                                spec == "EVGR" ~ "Evening Grosbeak",
                                spec == "LEGO" ~ "Lesser Goldfinch",
                                spec == "RCKI" ~ "Ruby-crowned Kinglet",
                                spec == "GCKI" ~ "Golden-crowned Kinglet",
                                spec == "RBNU" ~ "Red-breasted Nuthatch",
                                spec == "PYNU" ~ "Pygmy Nuthatch",
                                spec == "BRCR" ~ "Brown Creeper",
                                spec == "BUSH" ~ "Bushtit",
                                spec == "BHCO" ~ "Brown-headed Cowbird",
                                spec == "BUOR" ~ "Bullock's Oriole",
                                spec == "MAWR" ~ "Marsh Wren",
                                spec == "HOWR" ~ "House Wren",
                                spec == "BEWR" ~ "Bewick's Wren",
                                spec == "BARS" ~ "Barn Swallow",
                                spec == "WISN" ~ "Wilson's Snipe",
                                spec == "MODO" ~ "Mourning Dove",
                                spec == "BGGN" ~ "Blue-gray Gnatcatcher")) %>% 
  mutate(year_week = yearweek(date)) %>% 
  select(family, family_label, spec, spec_label, date, 
         loc, year_week, year, month, doy, note_2)

nethours_weekly <- nethours_daily %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(8, 9, 10)) %>% 
  mutate(year_week = yearweek(date)) %>% 
  group_by(year_week) %>% 
  summarize(nethours = sum(total_nethours, na.rm = TRUE))

tins_nethours <- tins_roi %>% 
  group_by(family, family_label, spec, spec_label, year, year_week) %>% 
  summarize(count = n()) %>% 
  inner_join(nethours_weekly, by = "year_week") %>% 
  mutate(count_per_hour = count / nethours) %>% 
  mutate(week = lubridate::isoweek(year_week)) %>% 
  select(family, family_label, spec, spec_label, year, 
         year_week, week, count, nethours, count_per_hour) 



###
### Top 10 Species
###

spec_top_10 <- tins_nethours %>% 
  group_by(spec_label) %>% 
  summarize(count = sum(count)) %>% 
  arrange(-count) %>% 
  head(10)



### 
### Recoveries Wrangling
###

recoveries <- read_csv(here::here("RawData", "Recoveries.csv")) %>% 
  clean_names() 

rec <- recoveries %>% 
  select(!c(long_band, lat_band, date_banded, location_banded)) %>% 
  rename("lat" = "lat_rec",
         "lon" = "long_rec",
         "location" = "recovery_location_descr",
         "date" = "date_recovered") %>% 
  mutate("band_rec" = "Recovered") %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  select(band_number, species, band_rec, date, age, sex, location, notes, lon, lat)

band <- recoveries %>% 
  select(!c(long_rec, lat_rec, date_recovered, recovery_location_descr)) %>% 
  rename("lat" = "lat_band",
         "lon" = "long_band",
         "location" = "location_banded",
         "date" = "date_banded") %>% 
  mutate("band_rec" = "Banded") %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  select(band_number, species, band_rec, date, age, sex, location, notes, lon, lat)

recoveries_tidy <- bind_rows(rec, band) %>% 
  arrange(band_number, date) 

sf <- st_as_sf(recoveries_tidy,
               coords = c("lon", "lat"),
               crs = 4326) %>%
  mutate(spec_label = case_when(
    species == "STJA" ~ "Steller's Jay",
    species == "WETA" ~ "Western Tanager",
    species == "GWCS" ~ "White-crowned Sparrow (Gambel's)",
    species == "AUWA" ~ "Yellow-rumped Warbler (Audubon's)")) %>% 
  mutate(year = lubridate::year(date)) %>% 
  select(band_number, species, spec_label, band_rec, date, year, age, sex, location, notes) 



### 
### Other 
###

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(12)


#####
###
### Shiny App 
###
#####


###
### User interface
###

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Tahoe Instititute for Natural Science Long-Term Bird Monitoring"),
  sidebarLayout(
    sidebarPanel(
      "This Shiny App displays data from the Tahoe Institute for Natural Science's long-term monitoring of Sierra Nevada songbirds (and some others!). Long-term monitoring is important to detect large-scale ecological patterns, such as in response to climate change. These data shown are from 2010-2019 and collection is ongoing. Visit", tags$a(href="http://www.tinsweb.org/", "tinsweb.org "), "for more information.",
      br(), br(),
      img(src = "tins-logo.png",
          width = "80%"),
      br(), br(),
      strong("Tab 1: Annual Bird Count."), 
      "View count per net hour (standardized; top graph) and total count (bottom graph) annually from 2010-2019. Options are to view all families, a single family, or a single species.",
      br(),
      strong("Tab 2: Seasonal bird count."), 
      "View count per net hour (standardized; top graph) and total count (bottom graph) seasonally with data from 2010-2019. Options are to view all families, a single family, or a single species.",
      br(),
      strong("Tab 3: Common Species."), 
      "Here you can learn more about the top ten species caught by TINS from 2010-2019.",
      br(), 
      strong("Tab 4: Recaptures."), 
      "There have been five instances of a bird banded by TINS being found elsewhere. These data are very useful because we can better understand where birds in Tahoe go afterwards. In this tab, you can view a map of where each species was banded then recaptured.",
      br(),  br(),
      "This Shiny App was created by Cameryn Brock (Bren School of Environmental Science & Management) with data collected and provided by Will Richardson (Tahoe Institute for Natural Science).",
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Annual Bird Count", 
                 br(),
                 fluidRow(
                   align = "center",
                   column(6,
                          selectInput(
                            inputId = "select_fam_total",
                            label = "First: Choose a family",
                            choices = c("All", unique(tins_nethours$family_label)))),
                   column(6,
                          selectInput(
                            inputId = "select_spec_total",
                            label = "Second: Choose a species",
                            choices = NULL))),
                 plotOutput(outputId = "total_fam_hour_graph"), 
                 br(), 
                 plotOutput(outputId = "total_fam_effort_graph")),
        tabPanel("Seasonal Bird Count",
                 br(),
                 fluidRow(
                   align = "center",
                   column(6,
                          selectInput(
                            inputId = "select_fam",
                            label = "First: Choose a family",
                            choices = c("All", unique(tins_nethours$family_label)))),
                   column(6,
                          selectInput(
                            inputId = "select_spec",
                            label = "Second: Choose a species",
                            choices = NULL))),
                 plotOutput(outputId = "seasonal_fam_hour_graph"), 
                 br(), 
                 plotOutput(outputId = "seasonal_fam_effort_graph")),
        tabPanel("Common Species",
                 br(),
                 fluidRow(
                   align = "center",
                   selectInput(
                     inputId = "select_top_10",
                     label = "Choose a species",
                     choices = unique(spec_top_10$spec_label)),
                 br(),
                 column(6,
                        br(), br(), br(),
                        span(textOutput(outputId = "spec_name"), style = "color:#2F99E8;font-weight:bold; font-size:28px"),
                        br(), 
                        span(textOutput(outputId = "no_banded"), style = "font-size:24px"),
                        br(),
                        span(textOutput(outputId = "spec_family"), style = "font-size:20px"),
                        span(textOutput(outputId = "spec_habitat"), style = "font-size:20px"),
                        span(textOutput(outputId = "spec_food"), style = "font-size:20px"),
                        span(textOutput(outputId = "spec_nest"), style = "font-size:20px"),
                        span(textOutput(outputId = "spec_behavior"), style = "font-size:20px")
                       ),
                 column(6,
                        imageOutput(outputId = "spec_image"),
                        br(), br(), br(), br(),
                        textOutput(outputId = "photo_credit"),
                        p("Species information acquired from", 
                          tags$a(href="https://www.birds.cornell.edu/home/", "The Cornell Lab of Ornithology"), ".")
                 ))),
        tabPanel("Band Recoveries",
                 br(),
                 fluidRow(
                   align = "center",
                   selectInput(
                     inputId = "select_rec",
                     label = "Choose a species",
                     choices = c("All", unique(sf$spec_label)))),
                 br(),
                 leafletOutput(outputId = "tmap",
                               height = 650))))))


###
### Server 
###

server <- function(input, output, session) {
  
  seasonal_fam <- reactive({
    tins_nethours %>% 
      filter(family_label == input$select_fam) })
  
  seasonal_spec <- reactive({
    tins_nethours %>% 
      filter(spec_label == input$select_spec) })
  
  total_fam <- reactive ({
    tins_nethours %>% 
      filter(family_label == input$select_fam_total) })
  
  total_spec <- reactive ({
    tins_nethours %>% 
      filter(spec_label == input$select_spec_total) })
  
  total_fam_avg <- reactive({
    tins_nethours %>% 
      filter(family_label == input$select_fam_total) %>% 
      group_by(spec_label, year) %>% 
      summarize(avg_yearly_hour = mean(count_per_hour)) })
  
  total_spec_avg <- reactive({
    tins_nethours %>% 
      filter(spec_label == input$select_spec_total) %>% 
      group_by(spec_label, year) %>% 
      summarize(avg_yearly_hour = mean(count_per_hour)) })
  
  total_all <- reactive({
    tins_nethours %>% 
      group_by(spec_label, year) %>% 
      summarize(avg_yearly_hour = mean(count_per_hour))})
  
  spec_top_10_chosen <- reactive({
    spec_top_10 %>% 
      filter(spec_label == input$select_top_10)
  })
  
  spec_top_10_fam <- reactive({
    tins_nethours %>% 
      filter(spec_label == input$select_top_10)
  })
  
  tins_nethours_all <- reactive({
    tins_nethours})
  
  tmap_sf <- reactive({
    sf %>% 
      filter(spec_label == input$select_rec)})
  

  
  
  observeEvent(seasonal_fam(),
               { updateSelectInput(
                 session, 
                 input = "select_spec",
                 choices = c("All", seasonal_fam()$spec_label)) })
  
  output$seasonal_fam_hour_graph <- renderPlot({
    
    if(input$select_fam == "All"){
      ggplot(tins_nethours_all(), aes(x = week, y = count_per_hour)) + 
        geom_jitter(color = "steelblue2",
                    alpha = 0.75,
                    size = 2,
                    width = 0.45) +
        geom_smooth(size = 0.5,
                    color = "gray75",
                    se = FALSE) +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Count per net hour",
             color = "Species") +
        scale_x_continuous(limits = c(31.5, 42.5),
                           breaks = c(32, 33, 34, 35, 36, 
                                      37, 38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))}
    
    else if(input$select_spec == "All"){
      ggplot(seasonal_fam(), aes(x = week, y = count_per_hour)) + 
        geom_jitter(aes(color = spec_label),
                    alpha = 0.75,
                    size = 2,
                    width = 0.45) +
        geom_smooth(size = 0.5,
                    color = "gray75",
                    se = FALSE) +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Count per net hour",
             color = "Species") +
        scale_color_manual(values = mycolors) +
        scale_x_continuous(limits = c(31.5, 42.5),
                           breaks = c(32, 33, 34, 35, 36, 
                                      37, 38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))}
    else({
      ggplot(seasonal_spec(), aes(x = week, y = count_per_hour)) + 
        geom_jitter(aes(color = spec_label),
                    alpha = 0.75,
                    size = 2,
                    width = 0.45) +
        geom_smooth(size = 0.5,
                    color = "gray75",
                    se = FALSE) +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Count per net hour",
             color = "Species") +
        scale_color_manual(values = mycolors) +
        scale_x_continuous(limits = c(31.5, 42.5),
                           breaks = c(32, 33, 34, 35, 36, 
                                      37, 38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))
      
    })
  }) 
  
  output$seasonal_fam_effort_graph <- renderPlot({
    
    if(input$select_fam == "All"){
      ggplot(tins_nethours_all(), aes(x = week, y = count)) +
        geom_col(fill = "steelblue2",
                 width = 0.75) +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Total count",
             fill = "Species") +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(31.5, 42.5),
                           breaks = c(32, 33, 34, 35, 36, 37, 
                                      38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))}
    
    else if(input$select_spec == "All"){
      ggplot(seasonal_fam(), aes(x = week, y = count)) +
        geom_col(aes(fill = spec_label),
                 width = 0.75) +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Total count",
             fill = "Species") +
        scale_fill_manual(values = mycolors) +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(31.5, 42.5),
                           breaks = c(32, 33, 34, 35, 36, 37, 
                                      38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))}
    else({
      ggplot(seasonal_spec(), aes(x = week, y = count)) +
        geom_col(aes(fill = spec_label),
                 width = 0.75) +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Total count",
             fill = "Species") +
        scale_fill_manual(values = mycolors) +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(31.5, 42.5),
                           breaks = c(32, 33, 34, 35, 36, 37, 
                                      38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))
    })
  })
  
  
  ########## Total #########
  
  observeEvent(total_fam(),
               {
                 updateSelectInput(
                   session, 
                   input = "select_spec_total",
                   choices = c("All", total_fam()$spec_label))
               })
  
  output$total_fam_hour_graph <- renderPlot({
    
    if(input$select_fam_total == "All"){
      ggplot(total_all(), aes(x = year, y = avg_yearly_hour)) + 
        geom_jitter(color = "steelblue2",
                    alpha = 0.7,
                    width = 0.2,
                    size = 2) +
        geom_smooth(size = 0.5,
                    color = "gray75",
                    se = FALSE) +
        theme_minimal() + 
        labs(x = "Year",
             y = "Average count per net hour per species") +
        scale_x_continuous(limits = c(2009.5, 2019.5),
                           breaks = seq(2010, 2019, by = 1),
                           minor_breaks = NULL)
    }
    else if(input$select_spec_total == "All"){
      ggplot(total_fam_avg(), aes(x = year, y = avg_yearly_hour)) + 
        geom_jitter(aes(color = spec_label),
                    alpha = 0.85,
                    width = 0.2,
                    size = 2) +
        geom_smooth(size = 0.5,
                    color = "gray75",
                    se = FALSE) +
        theme_minimal() + 
        labs(x = "Year",
             y = "Average count per net hour",
             color = "Species") +
        scale_color_manual(values = mycolors) +
        scale_x_continuous(limits = c(2009.5, 2019.5),
                           breaks = seq(2010, 2019, by = 1),
                           minor_breaks = NULL) 
    }
    else({
      ggplot(total_spec_avg(), aes(x = year, y = avg_yearly_hour)) + 
        geom_jitter(aes(color = spec_label),
                    size = 2,
                    width = 0.2) +
        geom_smooth(size = 0.5,
                    color = "gray75",
                    se = FALSE) +
        theme_minimal() + 
        labs(x = "Year",
             y = "Average count per net hour",
             color = "Species") +
        scale_color_manual(values = "steelblue2") +
        scale_x_continuous(limits = c(2009.5, 2019.5),
                           breaks = seq(2010, 2019, by = 1),
                           minor_breaks = NULL)
      
    })
  }) 
  
  output$total_fam_effort_graph <- renderPlot({
    
    if(input$select_fam_total == "All"){
      ggplot(tins_nethours_all(), aes(x = year, y = count)) +
        geom_col(fill = "steelblue2",
                 width = 0.75) +
        theme_minimal() + 
        labs(x = "Year",
             y = "Total count",
             fill = "Species") +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(2009.5, 2019.5),
                           breaks = seq(2010, 2019, by = 1),
                           minor_breaks = NULL) }
    
    else if(input$select_spec_total == "All"){
      ggplot(total_fam(), aes(x = year, y = count)) +
        geom_col(aes(fill = spec_label),
                 width = 0.75) +
        theme_minimal() + 
        labs(x = "Year",
             y = "Total count",
             fill = "Species") +
        scale_fill_manual(values = mycolors) +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(2009.5, 2019.5),
                           breaks = seq(2010, 2019, by = 1),
                           minor_breaks = NULL) }
    else({
      ggplot(total_spec(), aes(x = year, y = count)) +
        geom_col(aes(fill = spec_label),
                 width = 0.75) +
        theme_minimal() + 
        labs(x = "Year",
             y = "Total count",
             fill = "Species") +
        scale_fill_manual(values = "steelblue2") +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(2009.5, 2019.5),
                           breaks = seq(2010, 2019, by = 1),
                           minor_breaks = NULL)
    })
  })
  
  ##### Recoveries
  
  output$tmap = renderLeaflet({
    
    if(input$select_rec == "All"){
      species_tmap <- 
        tm_basemap("CartoDB.VoyagerNoLabels") +
        tm_shape(sf) +
        tm_dots("spec_label",
                title = "Species",
                id = "band_rec",
                palette = "Set2",
                border.col = "white",
                size = 0.125) }
    else({
      species_tmap <- 
        tm_basemap("CartoDB.VoyagerNoLabels") +
        tm_shape(tmap_sf()) +
        tm_dots("spec_label",
                id = NULL,
                col = "indianred1",
                border.col = "white",
                size = 0.125) + 
        tm_text("band_rec",
                col = "gray15",
                xmod = -4.5,
                ymod = 0.9) +
        tm_shape(tmap_sf()) +
        tm_text("year",
                col = "gray15",
                xmod = -4.5,
                ymod = -0.9)
      
    })
    
    tmap_mode("view")
    
    tmap_leaflet(species_tmap)
    
  })
  
  ##### Common species
  
  
  output$spec_image = renderImage({
    filename <- normalizePath(file.path('./images',
                                        paste(input$select_top_10, '.jpg', sep = '')))
    list(src = filename,
         width = "90%")},
    deleteFile = FALSE
    
  )
  
  output$spec_name <- renderText({
    spec_top_10_chosen()$spec_label
  })
  
  output$no_banded <- renderText({
    paste("TINS has banded", spec_top_10_chosen()$count, "individuals!")
  })
  
  output$spec_family <- renderText({
    paste("Family", unique(spec_top_10_fam()$family_label))
  })
  
  output$spec_habitat <- renderText({
    if(spec_top_10_chosen()$spec_label == "Yellow-rumped Warbler")
    {"Habitat: Forests"}
    else if(spec_top_10_chosen()$spec_label == "Dark-eyed Junco")
    {"Habitat: Forests"}
    else if(spec_top_10_chosen()$spec_label == "Orange-crowned Warbler")
    {"Habitat: Forests"}
    else if(spec_top_10_chosen()$spec_label == "Song Sparrow")
    {"Habitat: Open woodlands"}
    else if(spec_top_10_chosen()$spec_label == "White-crowned Sparrow")
    {"Habitat: Scrub"}
    else if(spec_top_10_chosen()$spec_label == "Mountain Chickadee")
    {"Habitat: Forests"}
    else if(spec_top_10_chosen()$spec_label == "Yellow Warbler")
    {"Habitat: Open Woodlands"}
    else if(spec_top_10_chosen()$spec_label == "Wilson's Warbler")
    {"Habitat: Scrub"}
    else if(spec_top_10_chosen()$spec_label == "Lincoln's Sparrow")
    {"Habitat: Scrub"}
    else if(spec_top_10_chosen()$spec_label == "Ruby-crowned Kinglet")
    {"Habitat: Forests"}
  })
  
  output$spec_food <- renderText({
    if(spec_top_10_chosen()$spec_label == "Yellow-rumped Warbler")
    {"Food: Insects"}
    else if(spec_top_10_chosen()$spec_label == "Dark-eyed Junco")
    {"Food: Seeds"}
    else if(spec_top_10_chosen()$spec_label == "Orange-crowned Warbler")
    {"Food: Insects"}
    else if(spec_top_10_chosen()$spec_label == "Song Sparrow")
    {"Food: Insects"}
    else if(spec_top_10_chosen()$spec_label == "White-crowned Sparrow")
    {"Food: Insects"}
    else if(spec_top_10_chosen()$spec_label == "Mountain Chickadee")
    {"Food: Insects"}
    else if(spec_top_10_chosen()$spec_label == "Yellow Warbler")
    {"Food: Insects"}
    else if(spec_top_10_chosen()$spec_label == "Wilson's Warbler")
    {"Food: Insects"}
    else if(spec_top_10_chosen()$spec_label == "Lincoln's Sparrow")
    {"Food: Insects"}
    else if(spec_top_10_chosen()$spec_label == "Ruby-crowned Kinglet")
    {"Food: Insects"}
  })
  
  output$spec_nest <- renderText({
    if(spec_top_10_chosen()$spec_label == "Yellow-rumped Warbler")
    {"Nesting: Trees"}
    else if(spec_top_10_chosen()$spec_label == "Dark-eyed Junco")
    {"Nesting: Ground"}
    else if(spec_top_10_chosen()$spec_label == "Orange-crowned Warbler")
    {"Nesting: Ground"}
    else if(spec_top_10_chosen()$spec_label == "Song Sparrow")
    {"Nesting: Shrub"}
    else if(spec_top_10_chosen()$spec_label == "White-crowned Sparrow")
    {"Nesting: Ground"}
    else if(spec_top_10_chosen()$spec_label == "Mountain Chickadee")
    {"Nesting: Cavity"}
    else if(spec_top_10_chosen()$spec_label == "Yellow Warbler")
    {"Nesting: Shrub"}
    else if(spec_top_10_chosen()$spec_label == "Wilson's Warbler")
    {"Nesting: Ground"}
    else if(spec_top_10_chosen()$spec_label == "Lincoln's Sparrow")
    {"Nesting: Ground"}
    else if(spec_top_10_chosen()$spec_label == "Ruby-crowned Kinglet")
    {"Nesting: Trees"}
  })
  
  output$spec_behavior <- renderText({
    if(spec_top_10_chosen()$spec_label == "Yellow-rumped Warbler")
    {"Behavior: Foliage Gleaner"}
    else if(spec_top_10_chosen()$spec_label == "Dark-eyed Junco")
    {"Behavior: Ground Forager"}
    else if(spec_top_10_chosen()$spec_label == "Orange-crowned Warbler")
    {"Behavior: Foliage Gleaner"}
    else if(spec_top_10_chosen()$spec_label == "Song Sparrow")
    {"Behavior: Ground Forager"}
    else if(spec_top_10_chosen()$spec_label == "White-crowned Sparrow")
    {"Behavior: Ground Forager"}
    else if(spec_top_10_chosen()$spec_label == "Mountain Chickadee")
    {"Behavior: Foliage Gleaner"}
    else if(spec_top_10_chosen()$spec_label == "Yellow Warbler")
    {"Behavior: Foliage Gleaner"}
    else if(spec_top_10_chosen()$spec_label == "Wilson's Warbler")
    {"Behavior: Foliage Gleaner"}
    else if(spec_top_10_chosen()$spec_label == "Lincoln's Sparrow")
    {"Behavior: Ground Forager"}
    else if(spec_top_10_chosen()$spec_label == "Ruby-crowned Kinglet")
    {"Behavior: Foliage Gleaner"}
  })
  
  output$photo_credit <- renderText({
    if(spec_top_10_chosen()$spec_label == "Yellow Warbler")
    {"Photo Credit: Joshua Galicki/Audubon Photography Awards"}
    else if(spec_top_10_chosen()$spec_label == "Orange-crowned Warbler")
    {"Photo Credit: Devin Grady/Audubon Photography Awards"}
    else("Photo Credit: Cameryn Brock")
  })
  
}


shinyApp(ui = ui, server = server)

