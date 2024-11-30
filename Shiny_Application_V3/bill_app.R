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

library(reticulate)
source_python("result.py")

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
               "Risk Calculator"
             ),
             tags$div(class = "panel-contents-plot",
                      tagList(
                        tags$h4('Random Forest Model Prediction'),
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("tss_input", "tss:", min = 0, max = 0.001, value = 0.000357),
                            sliderInput("DIP_input", "DIP:", min = 0, max = 0.5, value = 0.091478),
                            sliderInput("DIN_input", "DIN:", min = 0, max = 0.5, value = 0.064695),
                            sliderInput("PH_input", "PH:", min = 0, max = 15, value = 4.023255),
                            sliderInput("Turbidity_input", "Turbidity:", min = 0, max = 10, value = 1.23298889 ),
                            sliderInput("temp_input", "temp:", min = 0, max = 100, value = 18.031408),
                            sliderInput("Oxygen_input", "Oxygen:", min = 0, max = 50000, value = 10000.291),
                            sliderInput("current_speed_input", "current_speed:", min = 0, max = 0.5, value = 0.011175),
                            sliderInput("TN_input", "TN:", min = 0, max = 5, value = 0.89485),
                            sliderInput("TP_input", "TP:", min = -0, max = 0.5, value = 0.030444)
                          ),
                          # Output to display the result and map
                          mainPanel(
                            # Display the equation
                            verbatimTextOutput("equation_output"),
                            
                            # Display the input values and the calculated result
                            verbatimTextOutput("result_output"),
                            
                            # Display the plot
                            htmlOutput("result_percentage")
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
  
  calculate_z <- function(tss, DIP, DIN, PH, Turbidity, temp, Oxygen, current_speed, TN, TP) {
    z <- macroalgae_result(tss, DIP, DIN, PH, Turbidity, temp, Oxygen, current_speed, TN, TP)
    return(z)
  }
  
  # Equation display
  output$equation_output <- renderPrint({
    paste(calculate_z(input$tss_input, input$DIP_input, input$DIN_input, 
                      input$PH_input, input$Turbidity_input, input$temp_input, 
                      input$Oxygen_input, input$current_speed_input, input$TN_input, input$TP_input))
  })
  
  # Result calculation
  output$result_output <- renderPrint({
    z <- calculate_z(input$tss_input, input$DIP_input, input$DIN_input, 
                     input$PH_input, input$Turbidity_input, input$temp_input, 
                     input$Oxygen_input, input$current_speed_input, input$TN_input, input$TP_input)
    paste("tss =", input$tss_input, "| DIP =", input$DIP_input, "| DIN =", input$DIN_input, 
          "| PH =", input$PH_input, "Turbidity =", input$Turbidity_input, 
          "| temp =", input$temp_input, "| Oxygen =", input$Oxygen_input, 
          "| current_speed =", input$current_speed_input, "| TN =", input$TN_input, 
          "| TP =", input$TP_input, "| z =", z)
  })
  
  # result
  output$result_percentage <- renderUI({
    z <- calculate_z(input$tss_input, input$DIP_input, input$DIN_input, 
                     input$PH_input, input$Turbidity_input, input$temp_input, 
                     input$Oxygen_input, input$current_speed_input, input$TN_input, input$TP_input)
    
    result <- z * 100
    
    if (result <= 1) {
      color <- "forestgreen"  # Set the color to green for values less than or equal to 1
        risk_text <- "Low Risk"
    } else {
      color <- "darkred"  # Set the color to red for values greater than 1
        risk_text <- "High Risk"
    }
    
    # Create a div with the specified color, font size, and result
    div(
      style = paste0("color:", color, "; font-size: 100px;"),
      HTML(paste0(risk_text, "<br>", formatC(result, format = "f", digits = 2), "%"))
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
