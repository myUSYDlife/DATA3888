library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)


#Encode local images to base 64 first
qld <-base64enc::dataURI(file = "qld.png", mime = "image/png")
catchment <-base64enc::dataURI(file = "catchment.png", mime = "image/png")

ui <- fluidPage(
  titlePanel(
    "How does excessive nutrient concentration and sediment transport affect coral reef health?
"
  ),
  navbarPage(
    "Reef 6",
    tabPanel(
      ("Overview"),
      
      fluidRow(
        column(
          br(),
          tags$img(
            src = catchment,
            width = "400",
            height = "350",
            p(em("Figure 1: Catchment regions and the respective locations where the data for this research was sourced "),style =
                "font-family: times")
          ),
          br(),
          
          width = 2
        ),
        column(
          br(),
          p("Insert Text Here",
            style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
          br(),
          
          p("Insert Text Here",
            style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
          
          width = 8
        ),
        column(
          br(),
          tags$img(
            src = qld,
            width = "281",
            height = "180"
          ),
          br(),
          br(),
          
          width = 2
        )
      ),
      
      hr(),
      
      hr(),
      p(em("Project by"), br("Reef 6"), style =
          "text-align:center; font-family: times")
    ),
    tabPanel(
      "Currents",
      
      fluidRow(
        column(width = 2),
        column(h4(
          p("The interaction between currents and the transport of nutrients and sediment throughout the Great Barrier Reef 
", style = "color:black;text-align:center")
        ),
        width = 8, style = "background-color:lavender;border-radius: 10px")
      ),
      br(),
      fluidRow(
        column(
          width = 2,
          align = "center"
        ),
        column(
          p(
            "Insert Text Here",
            style = "color:black;text-align:center"
          ),
          width = 8,
          style = "background-color:lavender;border-radius: 10px"
        )
      ),
      br(),
      fluidRow(
        column(width = 2),
        column(
          p(
            "Insert Text Here",
            style = "color:black;text-align:center"
          ),
          width = 8,
          style = "background-color:papayawhip;border-radius: 10px"
        )
      ),
      hr(),
    
      br(),
    ),
    tabPanel(
      "Sediment",
      
      fluidRow(
        column(width = 2),
        column(h4(
          p("The effect of sediment transport on coral reef health", style = "color:black;text-align:center")
        ),
        width = 8, style = "background-color:lavender;border-radius: 10px")
      ),
      br(),
      fluidRow(
        column(
          width = 2,
          align = "center"
        ),
        column(
          p(
            "Insert Text Here",
            style = "color:black;text-align:center"
          ),
          width = 8,
          style = "background-color:lavender;border-radius: 10px"
        )
      ),
      br(),
      fluidRow(
        column(width = 2),
        column(
          p(
            "Insert Text Here",
            style = "color:black;text-align:center"
          ),
          width = 8,
          style = "background-color:papayawhip; border-radius: 10px"
        )
      ),
      hr(),
    ),

    tabPanel(
      "Nutrients",
      
      fluidRow(
        column(width = 2),
        column(h4(
          p("The effect of excessive nutrients on coral reef health", style = "color:black;text-align:center")
        ),
        width = 8, style = "background-color:lavender;border-radius: 10px")
      ),
      br(),
      
      fluidRow(
        column(
          width = 2,
          align = "center"
        ),
        column(
          p(
            "Insert Text Here",
            style = "color:black;text-align:center"
          ),
          width = 8,
          style = "background-color:lavender;border-radius: 10px"
        )
      ),
      br(),
      fluidRow(
        column(width = 2),
        column(
          p("Insert Text Here"),
          width = 8,
          style = "background-color:papayawhip; border-radius: 10px;text-align:center"
        )
      ),
      hr(),
    ),
    tabPanel(
      "Recommendations",
      
      fluidRow(
        column(width = 2),
        column(h4(
          p("How can we reduce these effects?", style = "color:black;text-align:center")
        ),
        width = 8, style = "background-color:lavender;border-radius: 10px")
      ),
      br(),
      
      fluidRow(
        column(
          width = 2,
          align = "center"
        ),
        column(
          p(
            "Insert Text Here",
            style = "color:black;text-align:center"
          ),
         
          
          width = 8,
          style = "background-color:lavender;border-radius: 10px"
        )
      ),
      br(),
      fluidRow(
        column(width = 2),
        column(
          p(
            "Insert Text Here",
            style = "color:black;text-align:center"
          ),
          width = 8,
          style = "background-color:papayawhip;border-radius: 10px"
        )
      ),
      
      hr(),
      
    )
    
  )
  
  
)

server <- function(input, output, session) {

}
shinyApp(ui, server)