library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "Civic Data Hackathon: Predicting road decay",
  titleWidth = '100%'
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("busmap", height = '800')
           )
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
                h4(
                 paste("Future prediction")
               ),
               p(
                 paste("Using data from previous years,",
                       "it is possible to predict future road decay.",
                       "The goal of this applicatoin is to help public works to",
                       "predict future demands and budget accordingly.",
                       "Once a historical dataset of potholes is added to the model,",
                       "the hope is that information from pothole complaints will improve",
                       "the quality of the predictions even further."
                 )
               ),
               sliderInput("year",
                           "Year",
                           min = 2016,
                           max = 2025,
                           value = 2016)
           )
    ),
    column(width = 3,
           box(width = NULL, solidHeader = TRUE,
               h3(
                 "About"
               ),
               p(
                 "Daniel E. Acuna, iSchool, Syracuse University"
               ),
               a("deacuna@syr.edu")
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)