#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)

# Define UI for application that draws a graph
ui <- fluidPage(
   
   # Application title
   titlePanel("Precipitation - Brighton Station"),
   
   # Sidebar with a slider input for number of years 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Years",
                     "Number of Years:",
                     min = 1987,
                     max = 2017,
                     value = 2016),
      #Create an input area for date ranges
      dateRangeInput("dates", label = h3("Date Range")),
      
         hr(),
      
      #Create text outputs for average, minumum and maximum values
      fluidRow(column(12, verbatimTextOutput("value_1"))),
      fluidRow(column(12, verbatimTextOutput("value"))),
      fluidRow(column(12, verbatimTextOutput("value_2"))),
      fluidRow(column(12, verbatimTextOutput("value_3"))),
      fluidRow(column(12, verbatimTextOutput("value_4"))),
      fluidRow(column(12, verbatimTextOutput("value_5")))   
      ),
      
      # Show a plot of the entire range of values and of the user selected range
      mainPanel(
        plotOutput("snotelPlot"),
        plotOutput("snotelplot")
        #textOutput("snotelplot")
      )
   )
)

# Define server logic required to draw summary statistics and two plots
server <- function(input, output) {
   output$value_1 <- renderText('Average')
   output$value <- renderText({mean(newdata$snotelPrecip)}) 
   output$value_2 <- renderText('Minimum')
   output$value_3 <- renderText({min(newdata$snotelPrecip)})
   output$value_4 <- renderText('Maximum')
   output$value_5 <- renderText({max(snotelPrecip)})
   
#Create individual vectors for Brighton data values
snoteldate <- snoteldata$Date[1:11231]
snotelSWE <- snoteldata$AccumSWE[1:11231]
snotelPrecip <- snoteldata$AccumPrecip[1:11231]

#Create a data frame combining all Brighton data values vectors
snoteldataframe <-  data.frame(snotelPrecip, snoteldate, snotelSWE)

#QA/QC for values below zero
newdata <- snoteldataframe[ which( snoteldataframe$snotelPrecip > 0), ]

#Render a plot for the entire data range
output$snotelPlot <- renderPlot({
  
  ggplot(data=newdata, aes(x=snoteldate))+
    geom_line(aes(y=snotelPrecip),stat="identity", colour = 'red')+
    xlab("Year")+ylab("Precipitation")+
    geom_line(aes(y=snotelSWE), stat="identity", colour = 'blue')+
    labs(title = "Precipitation & SWE")
    
    
  
})


#Render Text for the input data range
output$snotelplot <- renderText({
  paste("Viewing climate data between", input$dates[1],
    'and',input$dates[2])
})
#Render a plot for the input data range
output$snotelplot <- renderPlot({
  plotdata <- subset(newdata,
                            snoteldate >= input$dates[1] &
                            snoteldate<= input$dates[2])

  ggplot(data=plotdata, aes(x=plotdata$snoteldate, y=plotdata$snotelPrecip))+
    geom_line(stat="identity", colour = 'red')+
    geom_line(aes(x=plotdata$snoteldate, y=plotdata$snotelSWE), stat="identity", colour = 'blue')+
    xlab("Year")+ylab("Precipitation")
  
})
}


# Run the application 
shinyApp(ui = ui, server = server)

