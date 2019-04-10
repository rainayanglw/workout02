library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Raina Yang Workout2: Saving-Investing Modalities"),
   fluidRow(
     column(2, 
     
         # slider inputs 
         sliderInput("InitialAmount",
                     "Initial Amount:",
                     min = 0,
                     max = 100000,
                     value = 1000, step = 500),
         sliderInput("AnnualContribution",
                     "Annual Contribution:",
                     min = 0,
                     max = 50000,
                     value = 2000, step = 500)),
     column(2,
         sliderInput("ReturnRate",
                     "Return Rate: (in %)",
                     min = 0,
                     max = 20,
                     value = 5, step = 0.1),
         sliderInput("GrowthRate",
                     "Growth Rate: (in %)",
                     min = 0,
                     max = 20,
                     value = 2, step = 0.1)),
     
     column(2,
         sliderInput("Years",
                     "Years:",
                     min = 0,
                     max = 50,
                     value = 20, step = 1),
         selectizeInput("Facet",
                        "Facet?",
                        choices = c("Yes", "No")))
    ),
   
    h4("Timelines"),
      # Show the Timelines Plot
      plotOutput("TimelinesPlot"),
   h4("Balances"),
   tableOutput("dataframe")
  
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   data <- reactive({
     future_value <- function(amount, rate, years) {
       x = amount*(1+rate)^years
       return(x)
     }
     
     annuity <- function(contrib, rate, years) {
       x = contrib*(((1+rate)^years-1)/rate)
       return(x)
     }
     
     growing_annuity <- function(contrib, rate, growth, years) {
       x = contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
       return(x)
     }
     
     no_contrib <- c()
     fixed_contrib <- c()
     growing_contrib <- c()
     
     for (i in 0:input$Years){
       no_contrib[i+1] = future_value(input$InitialAmount,input$ReturnRate / 100, i)
     }
     
     for (i in 0:input$Years){
       fixed_contrib[i+1] = future_value(input$InitialAmount,input$ReturnRate / 100, i) + annuity(input$AnnualContribution, input$ReturnRate / 100, i)
     }
     
     for (i in 0:input$Years){
       growing_contrib[i+1] = future_value(input$InitialAmount,input$ReturnRate / 100, i) + growing_annuity(input$AnnualContribution, input$ReturnRate / 100, input$GrowthRate / 100, i)
     }
     year = c(0:input$Years)
     modalities <- data.frame("year" = year, "no_contrib" = no_contrib ,"fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib)
     
   })
  
   output$TimelinesPlot <- renderPlot({
      # generate plot based on inputs from ui.R
     
     
     library(ggplot2)
     if (input$Facet == "Yes"){
       library(reshape2)
       d <- melt(data(), id = 1:3, measure = 2:4)
       library(ggplot2)
       graph_modes <- ggplot(data = d, aes(x = year, y = value, color = variable)) + 
         ggtitle("Three modes of investing") +
         geom_line() +
         labs(x = "Time", y = "Growth") + 
         facet_wrap(~variable)
     } else{
     graph_modes <- ggplot(data = data(), aes(x = year)) + 
       ggtitle("Three modes of investing") +
       geom_line(aes(y = no_contrib, color = "no_contrib")) +
       geom_line(aes(y = fixed_contrib, color = "fixed_contrib")) +
       geom_line(aes(y = growing_contrib, color = "growing_contrib")) +
       labs(x = "Time", y = "Growth") 
     }
     graph_modes
   })
   
   
   
   output$dataframe <- renderTable({
     # generate plot based on inputs from ui.R
     data()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

