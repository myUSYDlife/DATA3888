library(maps)
library(ggplot2)
library("tidyverse")
library("leaflet")
library("crosstalk")
library("DT")
library("dplyr")
library("lubridate")
library("htmltools")
library("ggplot2")
library("dplyr")
library("ozmaps")
library("sf")
library("sf")
library(gganimate)
library(gifski)
library(shinyjs)
library(gridExtra)

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


#Current Visualisations
#2020 1H
reef20201H <- read.csv("Reef-2020-1H.csv")
reef20201H <- reef20201H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))


#2020 2H
reef20202H <- read.csv("Reef-2020-2H.csv")

reef20202H <- reef20202H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

#2019
reef2019 <- read.csv("Reef-2019.csv")
reef2019 <- reef2019 %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

#2021 1H
reef20211H <- read.csv("Reef-2021-1H.csv")
reef20211H <- reef20211H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

#2021 2H
reef20212H <- read.csv("Reef-2021-2H.csv")
reef20212H <- reef20212H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

total <- rbind(reef20202H,reef20201H,reef2019,reef20212H,reef20211H)
total

total$dx <- sin((total$current_dir)/360*pi*2)*total$current_speed
total$dy <- cos((total$current_dir)/360*pi*2)*total$current_speed

oz_states <- ozmaps::ozmap_states %>% filter(NAME == "Queensland")

current_uv_scalar <- 0.5

#-----------------



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
               class = "panel-contents-title",
               "Visualising movement of currents"
             ),
             tags$div(class = "panel-contents-plot",
                      tagList(
                        tags$h4('Currents in Hitchinbrook'),
                        imageOutput("plot1")
                      )
             )  
             
    ),
    tabPanel("Sediment", 
             # Add contents for panel 3
             tags$div(
               class = "panel-contents",
               "Panel three contents"
             )
    ),
    tabPanel("Recommendation", 
             # Add contents for panel 5
             tags$div(
               class = "panel-contents-title",
               "Calculator"
             ),
             tags$div(class = "panel-contents-plot",
                      tagList(
                        tags$h4('Equation: 0.2345 * a - 0.8972 * b + 0.8923 * x + 0.2394 * y = z'),
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("a_input", "Drag a:", min = 142.93, max = 152.8238, value = 147.0411494),
                            sliderInput("b_input", "Drag b:", min = -22.4433, max = -9.2331, value = -16.4285154),
                            sliderInput("x_input", "Drag x:", min = -100, max = 100, value = 32),
                            sliderInput("y_input", "Drag y:", min = -100, max = 100, value = 54)
                          ),
                          # Output to display the result and map
                          mainPanel(
                            # Display the equation
                            verbatimTextOutput("equation_output"),
                            
                            # Display the input values and the calculated result
                            verbatimTextOutput("result_output"),
                            
                            # Display the map
                            leafletOutput("map_output", width = "100%", height = 600)
                          )
                        )
                      )
             )  
    )
  ),
  
  windowTitle = "3888 Reef 6"
)

server <- function(input, output) {
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p = ggplot() +
      geom_segment(x = total$longitude, y = total$latitude, data = total, aes(xend = longitude + dx * current_uv_scalar, yend = latitude + dy * current_uv_scalar),
                   arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.7)  + 
      geom_sf(data = oz_states, mapping = aes(fill = NAME), show.legend = FALSE)  +
      coord_sf(xlim = c(145, 148),ylim=c(-18.5,-17)) + ggtitle("Current movement") + xlab("longitude") + ylab("latitude")+ 
      transition_states(
        total$time,
        transition_length = 5,
        state_length = 5
      )  +
      enter_fade() + 
      exit_shrink() +
      ease_aes('sine-in-out') +
      labs(subtitle = 'Date: {closest_state}')
    
    
    anim_save("outfile.gif", animate(p, duration = 10, fps = 10, width = 1400, height = 865, renderer = gifski_renderer(), res = 100, type = "cairo")) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         #width = 1000,
         #height = 800,
         #alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  calculate_z <- function(a, b, x, y) {
    z <- 0.2345 * a - 0.8972 * b + 0.8923 * x + 0.2394 * y
    return(z)
  }
  
  # Equation display
  output$equation_output <- renderPrint({
    paste("Equation: 0.2345 *", input$a_input, "- 0.8972 *", input$b_input, "+ 0.8923 *", input$x_input, "+ 0.2394 *", input$y_input, "=", calculate_z(input$a_input, input$b_input, input$x_input, input$y_input))
  })
  
  # Result calculation
  output$result_output <- renderPrint({
    z <- calculate_z(input$a_input, input$b_input, input$x_input, input$y_input)
    paste("a =", input$a_input, "| b =", input$b_input, "| x =", input$x_input, "| y =", input$y_input, "| z =", z)
  })
  
  # Map
  output$map_output <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      fitBounds(lng1 = 142.93, lat1 = -22.4433, lng2 = 152.8238, lat2 = -9.2331) %>%
      addCircleMarkers(
        lng = input$a_input,
        lat = input$b_input,
        weight = 1,
        radius = sqrt(abs(calculate_z(input$a_input, input$b_input, input$x_input, input$y_input))) * 2,
        fill = TRUE,
        fillOpacity = 0.6,
        fillColor = "#ff0000",
        color = "#ffffff",
        label = as.character(calculate_z(input$a_input, input$b_input, input$x_input, input$y_input))
      )
  })
}

shinyApp(ui = ui, server = server)
