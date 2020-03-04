###
# Attach packages
###

library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(janitor)
library(lubridate)
library(paletteer)
library(tsibble)


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


integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

###
### Shiny App 
###

# User interface
  
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("TINS Long-Term Bird Monitoring"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "select_fam",
        label = "First: Choose a family",
        choices = c("All", unique(tins_nethours$family_label))), 
      selectInput(
        inputId = "select_spec",
        label = "Second: Choose a species",
        choices = NULL
        )),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Bird Count",
                 br(),
                 p("Family seasonal (2010-2019)"), 
                 plotOutput(outputId = "seasonal_fam_hour_graph"), 
                 br(), 
                 p("Species seasonal (2010-2019)"),
                 plotOutput(outputId = "seasonal_fam_effort_graph")),
        tabPanel("Recaptures"),
        tabPanel("Species")))))



# Server 

server <- function(input, output, session) {
  
  seasonal_fam <- reactive({
    tins_nethours %>% 
      filter(family_label == input$select_fam)
  })
  
  seasonal_spec <- reactive({
    tins_nethours %>% 
      filter(spec_label == input$select_spec)
  })
  
  tins_nethours_all <- reactive({
    tins_nethours})
  
  observeEvent(seasonal_fam(),
               {
                 updateSelectInput(
                   session, 
                   input = "select_spec",
                   choices = c("All", seasonal_fam()$spec_label))
               })

  output$seasonal_fam_hour_graph <- renderPlot({
   
     if(input$select_fam == "All"){
      ggplot(tins_nethours_all(), aes(x = week, y = count_per_hour)) + 
        geom_jitter(color = "steelblue2",
                    alpha = 0.75,
                    size = 2) +
        geom_smooth(size = 0.5,
                    color = "gray75",
                    se = FALSE) +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Count per net hour",
             color = "Species") +
        scale_x_continuous(limits = c(32, 42),
                           breaks = c(32, 33, 34, 35, 36, 
                                      37, 38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))}
    
    else if(input$select_spec == "All"){
    ggplot(seasonal_fam(), aes(x = week, y = count_per_hour)) + 
        geom_jitter(aes(color = spec_label),
                  alpha = 0.75,
                  size = 2) +
        geom_smooth(size = 0.5,
                  color = "gray75",
                  se = FALSE) +
        theme_minimal() + 
        labs(x = "Week (all years)",
           y = "Count per net hour",
           color = "Species") +
        scale_x_continuous(limits = c(32, 42),
                           breaks = c(32, 33, 34, 35, 36, 
                                      37, 38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                    "9/13", "9/20", "9/27", "10/4", "10/11",
                                    "10/18"))}
    else({
      ggplot(seasonal_spec(), aes(x = week, y = count_per_hour)) + 
        geom_jitter(color = "steelblue2",
                    alpha = 0.75,
                    size = 2) +
        geom_smooth(size = 0.5,
                    color = "gray75",
                    se = FALSE) +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Count per net hour",
             color = "Species") +
        scale_x_continuous(limits = c(32, 42),
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
        geom_col(fill = "steelblue2") +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Total count",
             fill = "Species") +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(32, 42),
                           breaks = c(32, 33, 34, 35, 36, 37, 
                                      38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))}
    
    else if(input$select_spec == "All"){
      ggplot(seasonal_fam(), aes(x = week, y = count)) +
        geom_col(aes(fill = spec_label)) +
        theme_minimal() + 
        labs(x = "Week (all years)",
           y = "Total count",
           fill = "Species") +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(32, 42),
                         breaks = c(32, 33, 34, 35, 36, 37, 
                                    38, 39, 40, 41, 42),
                         labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                    "9/13", "9/20", "9/27", "10/4", "10/11",
                                    "10/18"))}
    else({
      ggplot(seasonal_spec(), aes(x = week, y = count)) +
        geom_col(fill = "steelblue2") +
        theme_minimal() + 
        labs(x = "Week (all years)",
             y = "Total count",
             fill = "Species") +
        scale_y_continuous(breaks = integer_breaks()) +
        scale_x_continuous(limits = c(32, 42),
                           breaks = c(32, 33, 34, 35, 36, 37, 
                                      38, 39, 40, 41, 42),
                           labels = c("8/9", "8/16", "8/23", "8/30", "9/6", 
                                      "9/13", "9/20", "9/27", "10/4", "10/11",
                                      "10/18"))
  })
})
}

shinyApp(ui = ui, server = server)




###
### Notes
###

# Want the family graph to show up, then want select input to be species from that family, then want that species to be highlight (specific color) and geom_smooth to show up for just that species as well 
# Change below graph to be bar graph of total count (instead of count/hour). Then with that one the proportion would change color when you select a single species
# Problem is that for bar graph want selected species to be at the bottom so you can actually understand the amount? 

## Allison
# How to make selectId reactive to first selectId... or plotly?
# updateSelectInput? 
# renderUI makes it so something will show up when you select something
# Should I do two geom smooths on top of each other and reactive if else statement? 
# Bar graph? Want selected sp at bottom so you can understand count? Or? Maybe unstack when you choose a species...? 
# Changing geom_smooth based on second inputId vs none?
# Dropdown options based on another widget - Shiny thread
# selectizeInput
# gghighlight() 
# ifelse to show entirely different graph with highlights
