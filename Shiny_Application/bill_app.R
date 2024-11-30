library(maps)
library(ggplot2)
library("tidyverse")
library("leaflet")
library("crosstalk")
library("DT")
library("dplyr")
library("lubridate")
library("htmltools")

reef <- read.csv("Reef_Check_with_cortad_variables_with_annual_rate_of_SST_change.csv") # read reef data
world_map <- map_data("world") # read map data

reef$lat_group <- cut(reef$Latitude.Degrees, breaks = seq(-90, 90, 5))
reef_summary <- aggregate(Average_bleaching ~ lat_group, data = reef, FUN = length)
reef_summary$Number_bleaching <- reef_summary$Average_bleaching
reef_summary$Bleaching_proportion <- reef_summary$Number_bleaching / sum(reef_summary$Number_bleaching)
reef_summary <- subset(reef_summary, select = -c(Average_bleaching))

reef_year <- aggregate(Average_bleaching ~ Year, data = reef, FUN = length)
reef_year$Bleaching_numbers <- reef_year$Average_bleaching
reef_year <- reef_year %>% select(-(Average_bleaching))

reef <- left_join(reef, reef_year, by = "Year")
reef$Year_data <- paste(reef$Year, reef$Bleaching_numbers, sep = " Bleaching Numbers: ")

reef$Ocean <- ifelse(reef$Ocean != "", reef$Ocean, "other")  

mycols <- c("#fa6a77", "#bf1567")

pal <- colorNumeric(
  palette = mycols,
  domain = reef$Average_bleaching
)

sd <- SharedData$new(reef)
year_filter_up <- filter_slider("Year", "", sd, column=~Year, step=1)

map <- leaflet(sd, width = "100%", height = "500px") %>% addTiles() %>% 
  addProviderTiles(providers$Esri.WorldTerrain, group = "World Imagery") %>%
  addProviderTiles("OpenTopoMap", group = 'Terrain') %>%
  addCircleMarkers(lat = ~Latitude.Degrees, 
                   lng = ~Longitude.Degrees,
                   popup = ~Average_bleaching, 
                   color = ~pal(Average_bleaching),
                   radius = ~Average_bleaching/5.5, stroke = F) %>%
  addScaleBar('bottomright') %>%
  addLegend("bottomleft", pal = pal, values = ~Average_bleaching,
            title = "Average Bleaching",
            opacity = 1) %>%
  addLayersControl(
    baseGroups = c("World Imagery",
                   "Toner Lite",
                   "Terrain"))

year_filter_down <- filter_checkbox("year", "Year", sd, group = ~Year_data, 
                                    inline = FALSE)

lat_filter <- filter_checkbox("lat_group", "Latitude Field", sd, group = ~lat_group, 
                              inline = TRUE)

ocean_filter <- filter_checkbox("Ocean", "Ocean", sd, group = ~Ocean, 
                                inline = TRUE)

country_filter <- filter_checkbox("Country", "Country", sd, group = ~Country, 
                                  inline = FALSE, columns = 3)




library(shiny)
library(base64enc)

#Encode local images to base 64 first
logo1 <- base64enc::dataURI(file = "logo.png", mime = "image/png")
logo2 <- base64enc::dataURI(file = "logo with text.png", mime = "image/png")
logo3 <- base64enc::dataURI(file = "logo3.png", mime = "image/png")
logo4 <- base64enc::dataURI(file = "logo4.png", mime = "image/png")

ui <- fluidPage(
  # Set app title
  tags$head(
    # Add custom style for title font
    tags$style(
      HTML("
        .title {
          font-family: Tahoma;
          left: 40px;
        }
        #tabset {
          position: fixed;
          top: 20px;
          left: 400px;
          right: 0px;
        }
        .panel-contents {
          margin-top: -610px;
          margin-left: 400px;
        }
        .panel-contents-title {
          margin-top: -610px;
          margin-left: 400px;
          font-size: 28px;
          font-family: Tahoma;
          font_weight: Bold;
        }
        .panel-contents-plot {
          margin-top: 0px;
          margin-left: 400px;
        }
        #workplace {
          position: absolute;
          margin-top: 10px;
          width: 300px;
          margin-left: 15px;
          background-color: transparent;
        }
         #transparent-block {
          background-color: rgba(255, 138, 0, 0.2);
          width: 320px;
          height: 500px;
          border-radius: 10px;
          margin-left: 5px;
        }
      ")
    )
  ),
  titlePanel(
    # Set title text and apply custom style
    tags$h1("Reef 6: Orange Soda", class = "title"),
    windowTitle = "3888 Group ASM"
  ),
  # Set app logo and position it on the top right corner
  tags$head(
    tags$style(
      HTML("
        #logo4 {
          float: left;
          margin-top: -5px;
          margin-left: 40px;
        }
      ")
    )
  ),
  # Add app logo and text placeholder
  tags$div(
    tags$img(src = logo4, id = "logo4", alt = "Orange Soda", height = "120px", width = "240px"),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),tags$br(),
    tags$div(
      "Workplace text goes here: la ala la la sld lsal asl dlas ldaslfdsalflfla. asd l adsla af las fa ",
      id = "workplace"
    ),
    tags$div(id = "transparent-block")
  ),
  
  # Add panel with tabs for panel selection
  tabsetPanel(
    id = "tabset",
    tabPanel("Home", 
             # Add contents for panel 1
             tags$div(
               class = "panel-contents-title",
               "How does excessive nutrient concentration and sediment transport affect coral reef health?"
             ),
             tags$div(class = "panel-contents-plot",
                      tagList(
                        tags$h4('Global Coral Bleaching Situation 1998 - 2017'),
                        year_filter_up,
                        map,
                        bscols(widths = c(4,8),
                               list(year_filter_down,
                                    ocean_filter,
                                    lat_filter),
                               country_filter)
                      )
             )  
    ),
    tabPanel("Current", 
             # Add contents for panel 2
             tags$div(
               class = "panel-contents",
               "Panel two contents"
             )
    ),
    tabPanel("Sediment", 
             # Add contents for panel 3
             tags$div(
               class = "panel-contents",
               "Panel three contents"
             )
    ),
    tabPanel("Nutrients", 
             # Add contents for panel 4
             tags$div(
               class = "panel-contents",
               "Panel 4 contents"
             )
    ),
    tabPanel("Recommendation", 
             # Add contents for panel 5
             tags$div(
               class = "panel-contents",
               "Panel 5 contents"
             )
    )
  ),
  
  windowTitle = "3888 Reef 6"
)

server <- function(input, output) {
  # Define server-side code
}

shinyApp(ui = ui, server = server)