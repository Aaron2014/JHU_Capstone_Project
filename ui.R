library(shiny)
shinyUI(fluidPage(
  titlePanel("Predict Next Word"),
  sidebarLayout(
    sidebarPanel(
      textInput("in_txt",label="Enter your phrase here", value=""),
      h4('Please use \'Submit\' to observe the predicted word.'),
      h4('(Sample Input:\'i want to\' or \'We are doing\')'),
      actionButton("Submit","Submit")
    ),
    mainPanel(
      h3("Next Word Prediction"),
      h4("Phrase entered:"),
      verbatimTextOutput("inputText"),
      h4('Next predicted words'),
      verbatimTextOutput("next_word"),
      h4("Probability"),
      verbatimTextOutput("prob")
    )
  )
))