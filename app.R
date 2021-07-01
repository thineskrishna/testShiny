#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(shiny)
library (DT)

PatientProject <- read_csv("PatientProject.csv")
ParticipatingSites <- read_csv("ParticipatingSites.csv")
Leaderboard <- read_csv("LeaderBoard.csv")

ui <- fluidPage(
    titlePanel("RESECT Data Summary Portal"),

    tabsetPanel(

      tabPanel("Participant List",
               DT::dataTableOutput("participation")
      ),

        tabPanel ("Cases Entered",
                  sidebarLayout(
                      sidebarPanel(
                          numericInput("num",
                                       label = h3("Input Data Access Group Number"),
                                       value = ""),

                      ),
                      mainPanel(
                          htmlOutput("sum"),
                          htmlOutput("site"),
                          htmlOutput("cases"),
                          hr(),
                          h3("Sites with the highest cases entered"),
                          tableOutput("leaderboard")
                      )

                  )
        )


         )
)

server <- function(input, output) {

   output$sum <- renderText({
      (paste("<h3>Total number of cases entered in RESECT:<b>", length(PatientProject$record_id), "</b></h3>"))
   })


    output$site<- renderText({
        row<- min(which(PatientProject$redcap_data_access_group==input$num))
        hospital<-PatientProject$Hospital[row]
        country<-PatientProject$City[row]
        Site<- paste(hospital,country, sep= ", ")
        "<br></br>"
        (paste("<h3>Institution:<b>", Site, "</b></h3>"))
    })

    output$cases <- renderText({
        CentreEntries<-length(which(PatientProject$redcap_data_access_group==input$num))
        "<br></br>"
        paste("<h3>Cases entered by institution:<b>", CentreEntries)
    })


    output$participation <- DT::renderDataTable({
        ParticipatingSites
    })

    output$leaderboard <- renderTable({
        Leaderboard
    }, digits = 0)

}
# Run the application
shinyApp(ui = ui, server = server)
