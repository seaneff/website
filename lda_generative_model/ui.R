library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Generative Model:\nLatent Dirichlet Allocation"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      width = 3,
    
      
      sliderInput("topic_count", 
                  "Number of Topics:", 
                  value = 3,
                  min = 3, 
                  max = 10),
      
      sliderInput("eta", 
                  "η:", 
                  value = .05,
                  min = .0001, 
                  max = 1),
      
      sliderInput("word_count", 
                  "Length of Vocabulary (V):", 
                  value = 20,
                  min = 6, 
                  max = 30),
      br(),
            
        sliderInput("a1", 
                    HTML(paste("𝝰", tags$sub(1), sep = "")), 
                    value = 0.05,
                    min = .01, 
                    max = 5),
      
      
      conditionalPanel(
        condition = "input.topic_count > 1",
        sliderInput("a2", 
                    HTML(paste("𝝰", tags$sub(2), sep = "")), 
                    value = 0.05,
                    min = .01, 
                    max = 5)
      ),
      
      conditionalPanel(
        condition = "input.topic_count > 2",
        sliderInput("a3", 
                   HTML(paste("𝝰", tags$sub(3), sep = "")), 
                    value = 0.05,
                    min = .01, 
                    max = 5)
      ),
      
      conditionalPanel(
        condition = "input.topic_count > 3",
        sliderInput("a4", 
                    HTML(paste("𝝰", tags$sub(4), sep = "")), 
                    value = 0.05,
                    min = .01, 
                    max = 5)
      ),
      
      conditionalPanel(
        condition = "input.topic_count > 4",
        sliderInput("a5", 
                    HTML(paste("𝝰", tags$sub(5), sep = "")), 
                    value = 0.05,
                    min = .01, 
                    max = 5)
      ),
      
      conditionalPanel(
        condition = "input.topic_count > 5",
        sliderInput("a6", 
                    HTML(paste("𝝰", tags$sub(6), sep = "")), 
                    value = 1,
                    min = .01, 
                    max = 5)
      ),
      
      conditionalPanel(
        condition = "input.topic_count > 6",
        sliderInput("a7", 
                    HTML(paste("𝝰", tags$sub(7), sep = "")), 
                    value = 1,
                    min = .01, 
                    max = 5)
      ),
      
      conditionalPanel(
        condition = "input.topic_count > 7",
        sliderInput("a8", 
                    HTML(paste("𝝰", tags$sub(8), sep = "")), 
                    value = 1,
                    min = .01, 
                    max = 5)
      ),
      
      conditionalPanel(
        condition = "input.topic_count > 8",
        sliderInput("a9", 
                    HTML(paste("𝝰", tags$sub(9), sep = "")), 
                    value = 1,
                    min = .01, 
                    max = 5)
      ),
      
      conditionalPanel(
        condition = "input.topic_count > 9",
        sliderInput("a10", 
                    HTML(paste("𝝰", tags$sub(10), sep = "")), 
                    value = 1,
                    min = .01, 
                    max = 5))
      
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Beta/Theta", 
                           fluidRow(
                             column(width = 10,
                                    height = 6,
                                    plotOutput("beta_bar")),
                             column(width = 10, 
                                    height = 6,
                                    plotOutput("theta_bar")))),
                  tabPanel("Example: Document 1 // Topic 1",
                           fluidRow(
                             column(width = 5, 
                                    height = 3,
                                    plotOutput("document_topic_bar")),
                             column(width = 7, 
                                    height = 3,
                                    plotOutput("topic_word_bar"))
      )
    )
  )
))))