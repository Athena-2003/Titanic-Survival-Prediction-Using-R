# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "Titanic Survival Probability",
                  tabPanel("Probability",
                           sidebarPanel(
                             tags$h3("Enter: "),
                             selectInput("sex", label = "Gender",
                                         choices = list("Female" = "f", "Male" = "m")),
                             selectInput("pclass", label = "Passenger Class",
                                         choices = list("1st Class" = 1, "2nd Class" = 2, "3rd Class" = 3)),
                             actionButton("submitbutton", 
                                          "Submit", 
                                          class = "btn btn-primary")
                             #textInput("sex", "Gender:", ""),
                             #textInput("pclass", "Passenger Class:", ""),
                             
                           ), 
                           mainPanel(
                             h1("Titanic"),
                             
                             h4("The probability of your survival aboard the Titanic is: "),
                             verbatimTextOutput("txtout"),
                             
                           ) 
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Graphs",
                           mainPanel(
                             plotOutput(outputId = "ggplot1")
                           ),
                          
                  
                   tabPanel("Embarkment Point",
                             mainPanel(
                            plotOutput(outputId = "ggplot2")
                           ),
                   tabPanel("Class and Sex",
                             mainPanel(
                             plotOutput(outputId = "ggplot3")
                            ),                           
                  tabPanel("Class, Sex and Age",
                           mainPanel(
                           plotOutput(outputId = "ggplot4")
                         ),                           
                                     
                ) # navbarPage
       # fluidPage
      )
   )
  )
)
)


# Define server function  
server <- function(input, output) {
probability <- reactive({
  
  if( input$sex == "f" && input$pclass == 1) {
    prob = nrow(f1cs) / nrow (female1)
  }else if( input$sex == "f" && input$pclass == 2) {
    prob = nrow(f2cs) / nrow (female2)
  }else if(  input$sex == "f" && input$pclass == 3) {
    prob = nrow(f3cs) / nrow (female3)
  }else if(  input$sex == "m" && input$pclass == 1) {
    prob = nrow(m1cs) / nrow (male1)
  }else if(  input$sex == "m" && input$pclass == 2) {
    prob = nrow(m2cs) / nrow (male2)
  }else if(  input$sex == "m" && input$pclass == 3) {
    prob = nrow(m3cs) / nrow (male3)
  }
    paste(prob*100, "%")
})
  output$txtout <- renderText({
    if (input$submitbutton>0) { 
      isolate(probability()) 
    } 
  })
  output$ggplot1 <- renderPlot ({
    ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
      theme_bw() +
      geom_bar() +
      scale_fill_discrete(name = "Status",labels = c("Deceased", "Survived"))+
      labs(y = "Passenger Count",
           title = "Titanic Survival Rates by Class")
  })
  output$ggplot2 <-  renderPlot ({
    ggplot(titanic, aes(x = Embarked, fill = Survived)) + 
      theme_bw() +
      geom_bar() +
      coord_polar("y") +
      scale_fill_discrete(name = "Status",labels = c("Deceased", "Survived"))+
      labs(y = "Passenger Count",
           title = "Titanic Survival Rates by Embarked point")
  })
  output$ggplot3 <-  renderPlot ({
    ggplot(titanic, aes(x = Sex, fill = Survived)) + 
      theme_bw() +
      facet_wrap(~ Pclass) +
      geom_bar() +
      scale_fill_discrete(name = "Status",labels = c("Deceased", "Survived"))+
      labs(y = "Passenger Count",
           title = "Titanic Survival Rates by Class and Sex")
  })
  output$ggplot4 <-  renderPlot ({
    ggplot(titanic, aes(x = Age, fill = Survived)) +
      theme_bw() +
      facet_wrap(Sex ~ Pclass) +
      geom_density(alpha = 0.5) +
      scale_fill_discrete(name = "Status",labels = c("Deceased", "Survived"))+
      labs(y = "Age",
           x = "Survived",
           title = "Titanic Survival Rates by Age, Class and Sex")
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)