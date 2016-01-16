library(shiny)
library(RColorBrewer)
## Define function to draw from dirichlet distribution
## Reference: lca package written by Ed Curry
## https://cran.r-project.org/web/packages/LCA/LCA.pdf
## Reference: course notes by Michael Jordon (UC Berkeley)
## http://www.cs.berkeley.edu/~jordan/courses/260-spring10/lectures/lecture24.pdf

## The dirichlet distribution is the 
## multivariate generalization of the beta distribution.

## To generate random numbers from the dirichlet distribution,
## we first draw random variables that are gamma distributed.
## The Dirichlet distribution is the joint distribution of the independent
## gamma variables divided by their sum.

## n = number of random vectors to generate
## alpha = vector of shape parameters
rdirichlet <- function (n, alpha){
  l <- length(alpha)
  x <- matrix(rgamma(n = l * n, shape = alpha), ncol = l, byrow = TRUE)
  s <- apply(x, 1, sum)
  return(x/s)
}

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  ## alpha can either be symmetric, in which case it is a single
  ## variable already called alpha, or it can be not symmetric,
  ## in which case it is input as a1, a2, a3, a4, a5, etc seperately
  
  alpha <- reactive({ 
      na.omit(
         c(input$a1, 
          input$a2, 
          input$a3, 
          ifelse(input$topic_count > 3, input$a4, NA), 
          ifelse(input$topic_count > 4, input$a5, NA),
          ifelse(input$topic_count > 5, input$a6, NA), 
          ifelse(input$topic_count > 6, input$a7, NA), 
          ifelse(input$topic_count > 7, input$a8, NA), 
          ifelse(input$topic_count > 8, input$a9, NA), 
          ifelse(input$topic_count > 9, input$a10, NA)))        
})
  
  set.seed(1202)
  ## beta is the per-topic word distribution
  beta <- reactive({rdirichlet(input$topic_count, ## per topic, 
                     rep(input$eta, input$word_count))}) ## word distribution
  
  set.seed(0614)
  
  ## theta is the per-document topic distribution
  theta <- reactive({rdirichlet(5, ## per document
                                alpha())}) ## topic distribution
  
  output$beta_bar <- renderPlot({
                      barplot(t(beta()),
                      col = rainbow(input$word_count),
                      main = "Word Topic Matrix:\nβ", 
                      ylab = "P (word | topic)",
                      xlab = "",
                      names =  paste("Topic", 1:input$topic_count),
                      xlim = c(0,input$topic_count*1.75),
                      width = 1)
                      
  legend("bottomright", 
         legend = paste("word", 1:input$word_count),
         fill = rainbow(input$word_count), 
         cex = .9, ncol = 2) })
  
  output$theta_bar <- renderPlot({
    
    barplot(t(theta()),
            col = brewer.pal(input$topic_count, "Set3"),
            ylab = "P (topic | document)",
            main = "Document Topic Matrix:\nθ",
            xlim = c(0, 7.5),
            names = paste("Document", 1:5))
    
    legend("topright", 
           legend = paste("Topic", 1:input$topic_count),
           fill = brewer.pal(input$topic_count, "Set3"), 
           cex = 1, ncol = 1)
               
})

output$topic_word_bar <- renderPlot({
  
  barplot(beta()[1,], 
          col = "dark blue",
          main = "Distribution of Topic 1\nOver Vocabulary",
          ylab = c("P (word | topic)"),
          names = paste("word", 1:input$word_count),
          las = 3,
          xlab = "",
          ylim = c(0, max(beta()[1,])*1.2))
    
})

output$document_topic_bar <- renderPlot({
  
  barplot(theta()[1,], 
          col = "dark blue",
          main = "Distribution of Document 1\nOver Topics",
          ylab = c("P (topic | document)"),
          names = paste("topic", 1:input$topic_count),
          las = 3,
          xlab = "",
          ylim = c(0, max(theta()[1,])*1.2))
  
})
})