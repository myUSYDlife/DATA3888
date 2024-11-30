#Updated Bill V

library("tidyverse")
library("leaflet")
library("crosstalk")
library("DT")
library("lubridate")
library("htmltools")
library("ggplot2")
library("dplyr")
library("ozmaps")
library("sf")
library("gganimate")
library("gifski")
library("shinyjs")
library("gridExtra")
library("reticulate")
py_install(c('pandas', 'joblib','scikit-learn'))
source_python("result.py")

summary <- "The Great Barrier Reef is under threat by an increase in diffuse pollutants which affect the ecosystem health of corals, seagrasses and mangroves. An increase in nutrients, sedimentation and turbidity has decreased coral reef health by reducing coral recruitment and diversity. Resulting in ideal environments for organisms that compete with corals, such as crown-of-thorn starfish and macroalgae. 

This research aims to create a better understanding of the link between increased pollutants and the phase shift from coral cover to macroalgae cover. 

This will be done through a comparison of two water catchments within the GBR, the Herbert Catchment in the Wet Tropics, which is a high-risk water quality catchment, and the Normanby Catchment in Cape York, which is a low-risk water catchment. 

Through temporal analysis of nutrient and sediment loads entering the ocean from these two catchments, the research aims to show how increased agricultural land use has a negative impact on coral reef health in the GBR. This information is aimed towards the Queensland Government Department of Agriculture and Fisheries, to help them implement and review coral reef management practices. 

The recommendations from this research are; to improve land use practices to reduce the level of pollutants entering waterways and to conserve areas of the reef that are most affected by pollutants. This includes improving wastewater treatment, reducing agricultural runoff, and implementing sustainable fishing practices. To do this there needs to be a better understanding of the effects of contaminants in the short-term versus the long-term, a better understanding of pollutant transport processes, and large-scale, long-term, whole-reef studies."

#Current Visualisations - Herbert
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
reef_data <- rbind(reef20202H,reef20201H,reef2019,reef20212H,reef20211H)

total$dx <- sin((total$current_dir)/360*pi*2)*total$current_speed
total$dy <- cos((total$current_dir)/360*pi*2)*total$current_speed

oz_states <- ozmaps::ozmap_states %>% filter(NAME == "Queensland")

current_uv_scalar <- 0.5

#Normanby Current Visualisation

#2022 2H
cleanreef20222H <- read.csv("Clean Reef 2022 2H.csv")
cleanreef20222H <- cleanreef20222H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

#2020 1H
cleanreef20201H <- read.csv("Clean Reef 2020 1H.csv")
cleanreef20201H <- cleanreef20201H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

#2020 2H
cleanreef20202H <- read.csv("Clean Reef 2020 2H.csv")
cleanreef20202H <- cleanreef20202H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

#2021 1H
cleanreef20211H <- read.csv("Clean Reef 2021 1H.csv")
cleanreef20211H <- cleanreef20211H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

#2021 2H
cleanreef20212H <- read.csv("Clean Reef 2021 2H.csv")
cleanreef20212H <- cleanreef20212H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

#2021 2H
cleanreef20221H <- read.csv("Clean Reef 2022 1H.csv")
cleanreef20221H <- cleanreef20221H %>% 
  group_by(longitude, latitude) %>%
  summarise(across(starts_with('current'), mean),time=max(time))

cleantotal <- rbind(cleanreef20222H,cleanreef20201H,cleanreef20202H,cleanreef20211H,cleanreef20212H,cleanreef20221H)

cleantotal$dx <- sin((cleantotal$current_dir)/360*pi*2)*cleantotal$current_speed
cleantotal$dy <- cos((cleantotal$current_dir)/360*pi*2)*cleantotal$current_speed

#Nutrient Vis
#Herbert DF
myvars <- c("time", "DIP", "DIN","TN","TP","MA_N_gr","tss","longitude","latitude")

#Read in CSV and subset the dataframe
reef20201H <- read.csv("Reef-2020-1H.csv")
reef20201H <- reef20201H[myvars]

reef20202H <- read.csv("Reef-2020-2H.csv")
reef20202H <- reef20202H[myvars]

reef20211H <- read.csv("Reef-2021-1H.csv")
reef20211H <- reef20211H[myvars]

reef20212H <- read.csv("Reef-2021-2H.csv")
reef20212H <- reef20212H[myvars]

reef20221H <- read.csv("Reef-2022-1H.csv")
reef20221H <- reef20221H[myvars]

reef20222H <- read.csv("Reef-2022-2H.csv")
reef20222H <- reef20222H[myvars]

df <- rbind(reef20201H,reef20202H,reef20211H,reef20212H,reef20221H,reef20222H)

df$DIP <- log(df$DIP)
df$DIN <- log(df$DIN)

df$TN <- log(df$TN)
df$TP <- log(df$TP)

df$tsslog <- log(df$tss)


df$MA_N_grlog <- (df$MA_N_gr + 0.1)
df$MA_N_grlog <- log(df$MA_N_grlog)

#Normanby DF
#Read in CSV and subset the dataframe
cleanreef20201H <- read.csv("Clean Reef 2020 1H.csv")
cleanreef20201H <- cleanreef20201H[myvars]

cleanreef20202H <- read.csv("Clean Reef 2020 2H.csv")
cleanreef20202H <- cleanreef20202H[myvars]

cleanreef20211H <- read.csv("Clean Reef 2021 1H.csv")
cleanreef20211H <- cleanreef20211H[myvars]

cleanreef20212H <- read.csv("Clean Reef 2021 2H.csv")
cleanreef20212H <- cleanreef20212H[myvars]

cleanreef20221H <- read.csv("Clean Reef 2022 1H.csv")
cleanreef20221H <- cleanreef20221H[myvars]

cleanreef20222H <- read.csv("Clean Reef 2022 2H.csv")
cleanreef20222H <- cleanreef20222H[myvars]

cleandf <- rbind(cleanreef20201H,cleanreef20202H,cleanreef20211H,cleanreef20212H,cleanreef20221H,cleanreef20222H)

cleandf$DIP <- log(cleandf$DIP)
cleandf$DIN <- log(cleandf$DIN)

cleandf$TN <- log(cleandf$TN)
cleandf$TP <- log(cleandf$TP)

cleandf$tsslog <- log(cleandf$tss)


cleandf$MA_N_grlog <- (cleandf$MA_N_gr + 0.1)
cleandf$MA_N_grlog <- log(cleandf$MA_N_grlog)

#get year
df$year <- substr(df$time, start = 1, stop = 4)
avg <- df %>% 
  group_by(year) %>%
  summarise(across(starts_with('MA'), mean)) 

cleandf$year <- substr(cleandf$time, start = 1, stop = 4)
cleanavg <- cleandf %>% 
  group_by(year) %>%
  summarise(across(starts_with('MA'), mean)) 

#Get the season
df <- df %>%
  mutate(result=case_when(
    substr(df$time, start = 6, stop = 7)== "12"  ~ "Summer",
    substr(df$time, start = 6, stop = 7)== "01"  ~ "Summer",
    substr(df$time, start = 6, stop = 7)== "02"  ~ "Summer",
    substr(df$time, start = 6, stop = 7)== "03"  ~ "Autumn",
    substr(df$time, start = 6, stop = 7)== "04"  ~ "Autumn",
    substr(df$time, start = 6, stop = 7)== "05"  ~ "Autumn",
    substr(df$time, start = 6, stop = 7)== "06"  ~ "Winter",
    substr(df$time, start = 6, stop = 7)== "07"  ~ "Winter",
    substr(df$time, start = 6, stop = 7)== "08"  ~ "Winter",
    TRUE ~ "Spring"
  ))

cleandf <- cleandf %>%
  mutate(result=case_when(
    substr(cleandf$time, start = 6, stop = 7)== "12"  ~ "Summer",
    substr(cleandf$time, start = 6, stop = 7)== "01"  ~ "Summer",
    substr(cleandf$time, start = 6, stop = 7)== "02"  ~ "Summer",
    substr(cleandf$time, start = 6, stop = 7)== "03"  ~ "Autumn",
    substr(cleandf$time, start = 6, stop = 7)== "04"  ~ "Autumn",
    substr(cleandf$time, start = 6, stop = 7)== "05"  ~ "Autumn",
    substr(cleandf$time, start = 6, stop = 7)== "06"  ~ "Winter",
    substr(cleandf$time, start = 6, stop = 7)== "07"  ~ "Winter",
    substr(cleandf$time, start = 6, stop = 7)== "08"  ~ "Winter",
    TRUE ~ "Spring"
  ))

#Rename result column
colnames(df)[colnames(df) == "result"] = "season"
colnames(cleandf)[colnames(cleandf) == "result"] = "season"

#Importance graph
# Read in the data and concatenate into one dataframe
data1 <- read.csv("Reef-2020-1H.csv")
data2 <- read.csv("Reef-2020-2H.csv")
data3 <- read.csv("Reef-2021-1H.csv")
data4 <- read.csv("Reef-2021-2H.csv")
data5 <- read.csv("Reef-2022-1H.csv")
data6 <- read.csv("Reef-2022-2H.csv")
reef_data <- rbind(data1, data2, data3, data4, data5, data6)

keep_cols <- c("time", "latitude", "longitude", "tss", "DIP", 
               "DIN", "PH", "Turbidity", "temp", "Oxygen", 
               "current_speed", "TN", "TP", "MA_N_gr")

reef_data_new <- reef_data[, keep_cols]

reef_data_new <- reef_data_new %>% 
  mutate(time = as.Date(as.character(time), '%Y-%m-%d'))

key_cols <- c("Total Suspended Solids", "Dissolved Inorganic Phosphorous", "Dissolved Inorganic Nitrogen", "PH", "Turbidity", 
              "Temperature", "Oxygen", "Current Speed", "Total Nitrogen", "Total Phosphorous")

key_importances <- c(0.26181013, 0.09272011, 0.07871058, 0.07615656, 0.06671054, 
                     0.12121891, 0.10227495, 0.07321423, 0.08244683, 0.04473716)

# Format the key_importances vector as percentages
key_importances_formatted <- sprintf("%.0f%%", key_importances * 100)

# Create a data frame with the key columns and importances
df_importance <- data.frame(key_cols, key_importances, key_importances_formatted)
#-----------------

library(shiny)
library(base64enc)

#Encode local images to base 64 first
logo4 <- base64enc::dataURI(file = "logo4.png", mime = "image/png")
qld <- base64enc::dataURI(file = "qld.png", mime = "image/png")
qld_os <- base64enc::dataURI(file = "qld_os.png", mime = "image/png")
catchments <- base64enc::dataURI(file = "new.png", mime = "image/png")
calculator <- base64enc::dataURI(file = "calculator.png", mime = "image/png")

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
        #place {
          position: fixed;
          top: 20px;
          left: 400px;
          right: 0px;
        }
        .panel-contents {
        }
        .panel-contents-title {
          font-size: 28px;
          font-family: Tahoma;
          font_weight: Bold;
        }
        .panel-contents-plot {
        }
        .panel-contents-plot-left0 {
          margin-top: 0px;
          margin-left: 400px;
          margin-right:100px;
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
         #explanation-block {
          background-color: rgba(255, 138, 0, 0.2);
          width: 350px;
          height:100px;
          border-radius: 10px;
          margin-left: 5px;
          margin-top: 180px;
          padding-left:10px;
          padding-right:10px;
          padding-top:10px;
          padding-bottom:10px;
         }
         #explanation-block-2 {
          background-color: rgba(255, 138, 0, 0.2);
          width: 350px;
          height:400px;
          border-radius: 10px;
          margin-left: 0px;
          margin-top: 22px;
          padding-left:10px;
          padding-right:10px;
          padding-top:10px;
          padding-bottom:10px;
         }
         #explanation-block-3 {
          background-color: rgba(255, 138, 0, 0.2);
          width: 1600px;
          border-radius: 10px;
          margin-left: 100px;
          margin-top: 35px;
          padding-left:10px;
          padding-right:10px;
          padding-top:10px;
          padding-bottom:10px;
         }
         #explanation-block-4 {
          background-color: rgba(255, 138, 0, 0.2);
          width: 500px;
          height: 100px;
          border-radius: 10px;
          margin-left: 0px;
          margin-top: 35px;
          padding-left:10px;
          padding-right:10px;
          padding-top:10px;
          padding-bottom:10px;
         }
         #explanation-block-5 {
          background-color: rgba(255, 138, 0, 0.2);
          width: 700px;
          height: 375px;
          border-radius: 10px;
          margin-left: 20px;
          margin-top: 10px;
          padding-left:10px;
          padding-right:10px;
          padding-top:10px;
         }
         #explanation-block-ma {
          background-color: rgba(255, 138, 0, 0.2);
          width: 350px;
          height:335px;
          border-radius: 10px;
          margin-left: 0px;
          margin-top: 35px;
          padding-left:10px;
          padding-right:10px;
          padding-top:10px;
          padding-bottom:10px;
         }
      ")
    )
  ),
  titlePanel(
    windowTitle = "Reef 6: Orange Soda",
    # Set title text and apply custom style
    tags$h1("Reef 6: Orange Soda", class = "title", width="100%")
   #windowTitle = "Reef 6: Orange Soda"
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
  #tags$div(
  #tags$img(src = qld, id = "logo4", alt = "Queensland Goverment", height = "155px", width = "240px")#,
  #tags$br(),
  #tags$br(),
  #tags$br(),
  #tags$br(),
  #tags$br(),
  #tags$br(),
  #tags$br(),tags$br(),
  #tags$div(
  #"Insert Summary/Brief Overview of project",
  #id = "workplace"
  #),
  #tags$div(id = "transparent-block")
  #),
  
  # Add panel with tabs for panel selection
  tabsetPanel(
    id = "tabset",
    tabPanel("Home", 
             # Add contents for panel 1
             tags$div(
               class = "panel-contents-title",
               "How does an increase in nutrient concentrations and sediment transported by currents affect macroalgal growth rates,",
               tags$br(),
               "ultimately impacting coral reef health?"
             ),
             tags$h4('Introduction & Stakeholder'),
             
             fluidRow(
               column(5, 
                      tags$div(
                        "The Great Barrier Reef is under threat by an increase in diffuse pollutants which affect the ecosystem health of corals, seagrasses and mangroves. An increase in nutrients, sedimentation and turbidity has decreased coral reef health by reducing coral recruitment and diversity. Resulting in ideal environments for organisms that compete with corals, such as crown-of-thorn starfish and macroalgae. This research aims to create a better understanding of the link between increased pollutants and the phase shift from coral cover to macroalgae cover. This will be done through a comparison of two water catchments within the GBR, the Herbert Catchment in the Wet Tropics, which is a high-risk water quality catchment, and the Normanby Catchment in Cape York, which is a low-risk water catchment. Through temporal analysis of nutrient and sediment loads entering the ocean from these two catchments, the research aims to show how increased agricultural land use has a negative impact on coral reef health in the GBR.",
                        tags$br(),
                        tags$br(),
                        "This information is aimed towards the Queensland Government Department of Agriculture and Fisheries, to help them implement and review coral reef management practices. The recommendations from this research are; to improve land use practices to reduce the level of pollutants entering waterways and to conserve areas of the reef that are most affected by pollutants. This includes improving wastewater treatment, reducing agricultural runoff, and implementing sustainable fishing practices. To do this there needs to be a better understanding of the effects of contaminants in the short-term versus the long-term, a better understanding of pollutant transport processes, and large-scale, long-term, whole-reef studies.",
                        tags$br(),
                        tags$br(),
                        tags$img(src = qld_os, width = "700px", height = "191px"),
                        id = "explanation-block-5"
                      )
                      
               
               ),
               column(4,
                      # Content 1
                      tags$img(src = catchments, id = "catchments", alt = "Herbert and Normanby Catchment", height = "500px", width = "700px"),
                      tags$figcaption("Global near-surface currents map of Queensland. The arrows indicate current direction as an annual mean, whilst the blue dots indicate the location of reefs. The green marker represents the Normanby Catchment site while the red marker indicates the Herbert Catchment site. Source: ArcGIS and student’s own work.",
                                      style = "width:700px; font-size:12px;"),
                      tags$div(
                        class = "panel-contents-plot",
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br()
                      )
               ),
               
               
               fluidRow(
                 column(6,
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        div(
                          p("sss", class = "custom-text"),
                          style="display:flex;",
                          tags$h4('Basic Information Plots')
                        ),
                        # Content 2
                        div(
                          p("sss", class = "custom-text"),
                          style="display:flex;",
                          selectInput("macroalgaecatchment", "Select Catchment To View", choices = c("Herbert Catchment","Normanby Catchment"))
                        ),
                        
                        div(style="display:flex;",
                            tags$div(
                              plotOutput("MAoveryears", width="600px", height="500px"),
                              tags$figcaption("Model output determining the average annual macroalgal growth rate, with a temporal scale of four years. Using the dropdown
box allows for both the Herbert Catchment and Normanby Catchment areas to be observed. Source: data from the Australian
Institution of Marine Science, model output by students' own work.", style = "width:600px; font-size:12px; margin-left:10px; margin-right:10px;"),
                              
                            ),
                            tags$div(
                              "For the Normanby catchment, it is evident that there are insignificant changes in macroalgae growth when compared to the Herbert catchment.",
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              id = "explanation-block-ma"
                            )
                        )
                 ),
                 column(6,
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        # Content 3
                        div(style="display:flex;",
                            tags$div(
                              plotOutput("importance_plot", width="600px", height="500px"),
                              tags$figcaption("Bar graph indicating the proportion of variables in their contribution to macroalgal growth rate. This was calculated through a random forest
model by students, using data sourced from the Australian Institution of Marine Science.", style = "width:600px; font-size:12px; margin-left:10px; margin-right:10px;"),
                            ),
                            tags$div(
                              "Sediment has the greatest impact on microalgae growth, followed by...",
                              tags$br(),
                              "Interestingly, the nutrients do not have as great of an impact, which aligns with research i.e., ",
                              tags$br(),
                              "• Sediments and storm events cause the event",
                              tags$br(),
                              "• Nutrients maintain the growth ",
                              tags$br(),
                              "• Changes in algal, coral, and fish assemblages along water quality gradients on the inshore Great Barrier Reef - Fabricius et al (2005)",
                              tags$br(),
                              "• Coral-algal phase shifts on coral reefs: ecological and environmental aspects - McManus et al (2004)",
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              id = "explanation-block-2"
                            )
                        )
                 )
                 
               ),
               
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               
               # Content 4
               tags$head(
                 tags$style(HTML("
                        .custom-text {
                        color: white;
                        }
                         "))
               ),
               div(
                 p("sss", class = "custom-text"),
                 style="display:flex;",
                 selectInput("yeartimealgae", "Select Year To View", choices = unique(df$year)),
                 selectInput("homecatchment", "Select Catchment To View", choices = c("Herbert Catchment","Normanby Catchment"))
               ),
               
               div(style="display:flex;",
                   tags$div(
                     plotOutput("algaeGrowth", width="1000px", height="500px"),
                     tags$figcaption("Map displaying the macroalgal growth rate across each catchment (by using the dropdown menu). The map uses both a color ramp and size gradient to show the differing macroalgal growth rate. Both were used so that the increased growth rate is strongly indicated in a cluster, where coral reefs exist. Source: data from the Australian Institution of Marine Science, model output by students' own work.", style = "width:1000px; font-size:12px; margin-left:10px; margin-right:10px;"),
                   ),
                   tags$div(
                     "Extent of measured macroalgal growth across each catchment, see its isolated around the reef area spatially.",
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     id = "explanation-block"
                   )
               ),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br()
               
               
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
                        fluidRow(
                          column(6,
                                 imageOutput("plot1"),
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 tags$figcaption("Animation of currents in the Herbert Catchment area.",
                                                 tags$br(),
                                                 "Source: current direction data sourced from the Australian Institution of Marine Science, animation is student’s own work."),
                                 
                                 tags$br()
                          ),
                          column(6,
                                 imageOutput("plot2"),
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 tags$figcaption("Animation of currents in the Normanby Catchment area.",
                                                 tags$br(),
                                                 "Source: current direction data sourced from the Australian Institution of Marine Science, animation is student’s own work."),
                                 tags$br()
                          )
                        ),
                        
                        
                        tags$br(),
                        tags$div(
                          "• Current movement is a few km out from the coastline - doesn’t indicate the northward current that occurs along coastline at Herbert Catchment - important to note because this plays a detrimental role in settling more sediment on reef as it moves up north and then swept with the EAC where it ultimately settles on reef.",
                          tags$br(),
                          "• Along the coastline the currents move northward due to southeasterly winds along the east coastline of australia",
                          tags$br(),
                          "• Sediment transport goes up initially then is brought down from currents to eventually settle on the reef",
                          id = "explanation-block-3"
                        )
                      )
             )  
             
    ),
    tabPanel("Nutrients", 
             # Add contents for panel 3
             tags$div(
               class = "panel-contents-title",
               "The Effects of Nutrient Run-Off on Macroalgal Growth"
             ),
             
             tags$div(class = "panel-contents-plot",
                      tags$br(),
                      tagList(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("yeartime", "Select Year To View", choices = unique(df$year)), 
                            selectInput("nutriseason", "Select Season To View", c("All Seasons","Spring","Summer","Autumn","Winter")),  
                            selectInput("nutricatchment", "Select Catchment To View", c("Herbert Catchment","Normanby Catchment")),  
                            radioButtons("data", "Select Nutrient:", choices = c('Dissolved Inorganic Phosphorous','Dissolved Inorganic Nitrogen','Total Phosphate','Total Nitrogen')),
                            verbatimTextOutput("nutrientCorr"),
                            tags$div(
                              "Text Insert Here",
                              tags$br(),
                              id = "explanation-block-4"
                            )
                          ),
                          mainPanel(
                            plotOutput("nutrivis",width="80%",height="500px"),
                            tags$figcaption("Dotplot that shows the correlation between different nutrients and their effect on macroalgal growth rate in the different catchments. The data is split into seasons (as seen in the dropdown menu) to allow for interpretation in relation to seasonal farming trends. Source: data is sourced from the Australian Institution of Marine Science, model output (macroalgal growth rate) is student’s own work."),
                            tags$br(),
                            plotOutput("nutrivismap",width="80%",height="500px"),
                            tags$figcaption("Map displaying the concentration of various nutrients (able to be selected from the dropdown menu, along with the season and year) across each catchment. The map uses both a colour ramp and size gradient to show the differing concentrations. Source: data from the Australian Institution of Marine Science, model output by students' own work."),
                            tags$br()
                          )
                        )
                      )        
                      
             ),
    ),
    tabPanel("Sediment", 
             # Add contents for panel 4
             tags$div(
               class = "panel-contents-title",
               "The Effects of Sediment on Macroalgal Growth",
             ),
             
             
             tags$div(class = "panel-contents-plot",
                      
                      tags$br(),
                      tagList(
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("yeartimetss", "Select Year To View", choices = unique(df$year)),
                            selectInput("tssseason", "Select Season To View", c("All Seasons","Spring","Summer","Autumn","Winter")),
                            selectInput("tsscatchment", "Select Catchment To View", c("Herbert Catchment","Normanby Catchment")),  
                            verbatimTextOutput("tssCorr"),
                            tags$div(
                              "Text Insert Here",
                              tags$br(),
                              id = "explanation-block-4"
                            )
                          ),
                          mainPanel(
                            plotOutput("tssvis",width="80%",height="500px"),
                            tags$figcaption("Dotplot that shows the correlation between total suspended solids and their effect on macroalgal growth rate in the different catchments. The data is split into seasons (as seen in the dropdown menu) to allow for interpretation in relation to seasonal farming trends and/or storm activity. Source: data is sourced from the Australian Institution of Marine Science, model output (macroalgal growth rate) is student’s own work."),
                            tags$br(),
                            plotOutput("tssmapped",width="80%",height="500px"),
                            tags$figcaption("Map displaying the total suspended solids across each catchment. The map uses both a colour ramp and size gradient to show the differing concentrations and to emphasise clusters. Source: data from the Australian Institution of Marine Science, model output by students' own work."),
                            tags$br()
                          )
                        )
                      )        
             ),
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
                            fluidRow(
                              column(7, 
                                     sliderInput("tss_input", "Total Suspended Solids (kg m-3):", min = 5.220e-08, max = 4.390e-04, value = 1.045e-07),
                                     sliderInput("DIP_input", "Dissolved Inorganic Phosphorous (mg P m-3):", min = 0.001118, max = 0.702027, value = 0.004158),
                                     sliderInput("DIN_input", "Dissolved Inorganic Nitrogen (mg N m-3):", min = 0.03406, max = 31.59233, value = 0.72632),
                                     sliderInput("PH_input", "PH:", min = 7.997, max = 8.145, value = 8.066),
                                     sliderInput("Turbidity_input", "Turbidity:", min = 0.1361, max = 1.3665, value = 0.1700),
                                     tags$div(
                                       tags$div(
                                         "Text Insert Here",
                                         tags$br(),
                                         id = "explanation-block-4"
                                       )
                                     )
                              ),
                              column(5,
                                     sliderInput("temp_input", "Temperature:", min = 22.53, max = 33.05, value = 28.03),
                                     sliderInput("Oxygen_input", "Oxygen:", min = 6368, max = 7127, value = 6675),
                                     sliderInput("current_speed_input", "Current Speed:", min = 0.006858, max = 0.813074, value = 0.227600),
                                     sliderInput("TN_input", "Total Nitrogen:", min = 43.11, max = 111.37, value = 47.91),
                                     sliderInput("TP_input", "Total Phosphate:", min = 3.460, max = 9.509, value = 4.021)
                              )
                            )
                          ),
                          # Output to display the result and map
                          mainPanel(
                            # Display the equation
                            #verbatimTextOutput("equation_output"),
                            
                            # Display the input values and the calculated result
                            #verbatimTextOutput("result_output"),
                            
                            # Display the plot
                            htmlOutput("result_percentage"),
                            tags$img(src = calculator, width = "1156px", height = "627px")
                            
                          )
                        )
                      )
             )  
    )
  ),
  
  windowTitle = "3888 Reef 6"
)

server <- function(input, output) {
  #Reactive values
  values <- reactiveValues()
  values$tssmaplongmax <- 148
  values$tssmaplongmin <- 145
  values$tssmaplatmax <- -17
  values$tssmaplatmin <- -18.5
  values$nutrimaplongmax <- 148
  values$nutrimaplongmin <- 145
  values$nutrimaplatmax <- -17
  values$nutrimaplatmin <- -18.5
  values$nutrientTitle <- "Dissolved Inorganic Phosphorous"
  
  output$plot1 <- renderImage({
    #Current Herbert Animation File
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p = ggplot() +
      geom_segment(x = total$longitude, y = total$latitude, data = total, aes(xend = longitude + dx * current_uv_scalar, yend = latitude + dy * current_uv_scalar),
                   arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.7)  + 
      geom_sf(data = oz_states, mapping = aes(fill = NAME), show.legend = FALSE)  +
      coord_sf(xlim = c(145, 148),ylim=c(-18.5,-17)) + ggtitle("Herbert Current movement") + xlab("Longitude") + ylab("Latitude")+ 
      transition_states(
        total$time,
        transition_length = 5,
        state_length = 5
      )  +
      enter_fade() + 
      exit_shrink() +
      ease_aes('sine-in-out') +
      labs(subtitle = 'Date: {closest_state}')
    
    
    anim_save("outfile.gif", animate(p, duration = 8, fps = 10, width = 800, height = 465, renderer = gifski_renderer(), res = 100, type = "cairo")) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         #width = 1000,
         #height = 800,
         #alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  #Current Normanby Visualisation
  output$plot2 <- renderImage({
    #Current Herbert Animation File
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p = ggplot() +
      geom_segment(x = cleantotal$longitude, y = cleantotal$latitude, data = cleantotal, aes(xend = longitude + dx * current_uv_scalar, yend = latitude + dy * current_uv_scalar),
                   arrow = arrow(angle = 15, length = unit(0.02, "inches"), type = "closed"), alpha = 0.7)  + 
      geom_sf(data = oz_states, mapping = aes(fill = NAME), show.legend = FALSE)  +
      coord_sf(xlim = c(145, 146.5),ylim=c(-15.6,-14.8)) + ggtitle("Normanby Current movement") + xlab("Longitude") + ylab("Latitude")+ 
      transition_states(
        cleantotal$time,
        transition_length = 5,
        state_length = 5
      )  +
      enter_fade() + 
      exit_shrink() +
      ease_aes('sine-in-out') +
      labs(subtitle = 'Date: {closest_state}')
    
    
    anim_save("outfile.gif", animate(p, duration = 8, fps = 10, width = 800, height = 465, renderer = gifski_renderer(), res = 100, type = "cairo")) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         #width = 1000,
         #height = 800,
         #alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  
  #Nutrient Visualisation
  df_subset <- reactive({
    if(input$nutricatchment == "Herbert Catchment"){
      a <- subset(df, year == input$yeartime)
      if(input$nutriseason != "All Seasons"){
        a <- subset(a, season == input$nutriseason)
      }
      return(a)
    }
    else if(input$nutricatchment == "Normanby Catchment"){
      
      a <- subset(cleandf, year == input$yeartime)
      if(input$nutriseason != "All Seasons"){
        a <- subset(a, season == input$nutriseason)
      }
      return(a)
    }
  })
  
  #Update nutrient name based on nutri chosen
  observeEvent(input$data, {
    #Dissolved Inorganic Phosphorous','Dissolved Inorganic Nitrogen','Total Phosphate','Total Nitrogen
    if(input$data == "Dissolved Inorganic Phosphorous"){
      values$nutrientTitle <<- "DIP"
    }
    else if(input$data == "Dissolved Inorganic Nitrogen"){
      values$nutrientTitle <<- "DIN"
    }
    else if(input$data == "Total Nitrogen"){
      values$nutrientTitle <<- "TN"
    }
    else{
      values$nutrientTitle <<- "TP"
    }
  })
  
  output$nutrivis <- renderPlot({
    ggplot(data = df_subset(), aes(x= df_subset()[[values$nutrientTitle]], y= MA_N_gr)) +
      geom_point(size=2, shape=23) + geom_smooth(method=lm) + 
      ggtitle(paste0(input$data," vs Macroalgal Growth in ","\n" ,input$nutricatchment ," ", input$yeartime," ",input$nutriseason)) + xlab(paste0(input$data," Levels")) + ylab("Macroalgal Growth Levels")
  })
  
  #Update variables based on nutri catchment chosen
  observeEvent(input$nutricatchment, {
    if(input$nutricatchment == "Herbert Catchment"){
      values$nutrimaplongmax <<- 148
      values$nutrimaplongmin <<- 145
      values$nutrimaplatmax <<- -17
      values$nutrimaplatmin <<- -18.5
    }
    else{
      values$nutrimaplongmax <<- 145
      values$nutrimaplongmin <<- 146.5
      values$nutrimaplatmax <<- -15.6
      values$nutrimaplatmin <<- -14.8
    }
  })
  
  #Update variables based on nutri catchment chosen
  observeEvent(input$tsscatchment, {
    if(input$tsscatchment == "Herbert Catchment"){
      values$tssmaplongmax <<- 148
      values$tssmaplongmin <<- 145
      values$tssmaplatmax <<- -17
      values$tssmaplatmin <<- -18.5
    }
    else{
      values$tssmaplongmax <<- 145
      values$tssmaplongmin <<- 146.5
      values$tssmaplatmax <<- -15.6
      values$tssmaplatmin <<- -14.8
    }
  })
  
  #Update variables based on nutri catchment chosen
  observeEvent(input$homecatchment, {
    if(input$homecatchment == "Herbert Catchment"){
      homemaplongmax <<- 148
      homemaplongmin <<- 145
      homemaplatmax <<- -17
      homemaplatmin <<- -18.5
    }
    else{
      homemaplongmax <<- 145
      homemaplongmin <<- 146.5
      homemaplatmax <<- -15.6
      homemaplatmin <<- -14.8
    }
  })
  
  
  #Nutrient Map Vis
  output$nutrivismap <- renderPlot({
    ggplot() +
      geom_sf(data = oz_states, mapping = aes(fill = NAME), show.legend = FALSE)+
      coord_sf(xlim = c(values$nutrimaplongmin, values$nutrimaplongmax),ylim=c(values$nutrimaplatmin,values$nutrimaplatmax)) + 
      geom_point(data = df_subset(), alpha = 0.2, aes(y = latitude,x = longitude, size = df_subset()[[values$nutrientTitle]], color = df_subset()[[values$nutrientTitle]])) + 
      ggtitle(paste0(input$data," in ","\n" ,input$nutricatchment ," ",input$yeartime," ",input$nutriseason)) +
      xlab("Longitude") + ylab("Latitude") +labs(colour=values$nutrientTitle, size=values$nutrientTitle)
  })
  
  #Render Correlation Values
  ## Nutrients
  output$nutrientCorr <- renderPrint({
    val = ""
    if(input$data == "Dissolved Inorganic Nitrogen"){
      val = "DIN"
    }
    else if(input$data == "Total Phosphate"){
      val = "TP"
    }
    else if(input$data == "Total Nitrogen"){
      val = "TN"
    }
    else{
      val = "DIP"
    }
    paste0("Correlation Value: ", round(corr <- cor(df_subset()[[val]], df_subset()$MA_N_gr),4))
  })
  
  #Sediment
  output$tssCorr <- renderPrint({
    paste0("Correlation Value: ", round(corr <- cor(df_subset_tss()$tss, df_subset_tss()$MA_N_gr),4))
  })
  
  #TSS vis
  df_subset_tss <- reactive({
    if(input$tsscatchment == "Herbert Catchment"){
      a <- subset(df, year == input$yeartimetss)
      if(input$tssseason != "All Seasons"){
        a <- subset(a, season == input$tssseason)
      }
      return(a)
    }
    else if(input$tsscatchment == "Normanby Catchment"){
      a <- subset(cleandf, year == input$yeartimetss)
      if(input$tssseason != "All Seasons"){
        a <- subset(a, season == input$tssseason)
      }
      return(a)
    }
    
  })
  
  output$tssvis <- renderPlot({
    ggplot(df_subset_tss(), aes(x=tsslog, y= MA_N_gr)) +
      geom_point(size=2, shape=23) +
      geom_smooth(method=lm) + ggtitle(paste0("Total Suspended Sediment vs Macroalgal Growth in ","\n",input$tsscatchment ," ",input$yeartimetss," ",input$tssseason)) + 
      xlab("Total Suspended Solids (kg m-3)") + 
      ylab("Macroalgal Growth Rate")
  })
  
  #MA_Gr vis
  df_subset_alg <- reactive({
    if(input$homecatchment == "Herbert Catchment"){
      a <- subset(df, year == input$yeartimealgae)
      return(a)
    }
    else if(input$homecatchment == "Normanby Catchment"){
      a <- subset(cleandf, year == input$yeartimealgae)
      return(a)
    }
    
  })
  
  #Show the Algae growth
  output$algaeGrowth <- renderPlot({
    ggplot() +
      geom_sf(data = oz_states, mapping = aes(fill = NAME), show.legend = FALSE)+
      coord_sf(xlim = c(homemaplongmin, homemaplongmax),ylim=c(homemaplatmin,homemaplatmax)) + geom_point(data = df_subset_alg(), alpha = 0.2, aes(y = latitude, x = longitude, size = MA_N_gr, color = MA_N_gr)) + 
      ggtitle(paste0("Macroalgal Growth Across ",input$homecatchment," " ,input$yeartimealgae)) + 
      xlab("Longitude") + ylab("Latitude") +labs(colour="Macroalgal Growth Rate", size="Macroalgal Growth Rate")
  })
  
  #tss across the reef
  output$tssmapped <- renderPlot({
    ggplot() +
      geom_sf(data = oz_states, mapping = aes(fill = NAME), show.legend = FALSE)+
      coord_sf(xlim = c(values$tssmaplongmax, values$tssmaplongmin),ylim=c(values$tssmaplatmax,values$tssmaplatmin)) + geom_point(data = df_subset_tss(), alpha = 0.2, aes(y = latitude,x = longitude, size = tss, color = tss)) + 
      ggtitle(paste0("Total Suspended Solids Across ","\n" ,input$tsscatchment," ",input$yeartimetss," ",input$tssseason)) +
      xlab("Longitude") + ylab("Latitude") + labs(colour = "tss (kg m-3)", size = "tss (kg m-3)")
  })
  
  Maoveryears <- reactive({
    if(input$macroalgaecatchment == "Herbert Catchment"){
      return(avg)
    }
    if(input$macroalgaecatchment == "Normanby Catchment"){
      return(cleanavg)
    }
  })
  
  #MA_gr over the years for home page
  output$MAoveryears <- renderPlot({
    ggplot(data=Maoveryears(), aes(x=year, y=MA_N_gr, group=1)) +
      geom_line()+
      geom_point() + ggtitle(paste0("Average Macroalgal Growth Between","\n","2020 and 2023 ", input$macroalgaecatchment)) + 
      ylab("Average Macroalgal Growth") + xlab("Year")
    
  })
  
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
    
    if (result <= 0.001425) {
      color <- "#23ba30"  # Set the color to green for values less than or equal to 1
        risk_text <- "Lower than the GBR Data's Mean Risk: 0.1425%"
    } else {
      color <- "#e64640"  # Set the color to red for values greater than 1
        risk_text <- "Higher than the GBR Data's Mean Risk: 0.1425%"
    }
    
    # Create a div with the specified color, font size, and result
    div(
      style = paste0("color:", color, "; font-size: 30px;"),
      #style = paste0("font-size: 30px;"),
      #HTML(paste0(risk_text, "<br>", formatC(result, format = "f", digits = 2), "%"))
      HTML(paste0(risk_text, "<br> <br>", "Macroalgal Growth: ", "<br>", formatC(result, format = "f", digits = 4), "%"))
    )
  })
  
  #importance plot
  output$importance_plot <- renderPlot({
    # Create a bar plot with labels
    ggplot(df_importance, aes(x = reorder(key_cols, +key_importances), y = key_importances)) +
      geom_bar(stat = "identity", fill = "#fa6a77", alpha = 0.6) +
      coord_flip() +
      xlab("Effective Variables") +
      ylab("Feature Importances") +
      ggtitle("Variables that Affect Macroalgal Growth ") +
      geom_text(aes(label = key_importances_formatted), hjust = 1.5) +
      theme(axis.text.y = element_text(angle = 45, hjust = 1))
    #axis.title.x = element_text(size = 12, face = "bold"),
    #axis.title.y = element_text(size = 12, face = "bold"),
    #plot.title = element_text(size = 15, face = "bold"))
  })
  
}

shinyApp(ui = ui, server = server)
