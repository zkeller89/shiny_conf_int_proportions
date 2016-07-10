library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # CSS 
  tags$head(
    tags$style(HTML("#getSample{
                        background-color: #0099cc;
                        color:white;
                        padding: 15px 32px;
                        text-align: center;
                        font-size:16px;
                        font-weight:bold;
                        width: 100%;}
                     #getConfInt{
                        background-color: #660000;
                        color:white;
                        padding: 15px 32px;
                        text-align: center;
                        font-size:16px;
                        font-weight:bold;
                        width: 100%;}
                    "))
  ),
  
  # Application title
  titlePanel("Confidence Intervals - Proportions"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("true_p",
                  "True Proportion",
                  min = 0,
                  max = 1,
                  value = 0.5),
      strong("Current Sample Size: "), strong(textOutput("sample_size_text")),
      br(),
      actionButton(inputId = "add1",
                   label = "Add 1"),
      actionButton(inputId = "add25",
                   label = "Add 25"),
      actionButton(inputId = "add100",
                   label = "Add 100"),
      br(),br(),
      "Reset Sample Size: ",
      actionButton(inputId = "reset",
                   label = "Reset"),
      br(),br(),
      actionButton(inputId = "getSample",
                   label = "Get Sample"),
      br(),br(),
      sliderInput("conf_lvl",
                  "Confidence Level",
                  min = .9,
                  max = 1,
                  value = 0.95),
      br(),
      actionButton(inputId = "getConfInt",
                   label = "Get Confidence Interval")      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      withMathJax(),
      h4(uiOutput("p_hat_output")),
      h4(uiOutput("se_formula")),
      h4(uiOutput("z_star_output")),
      h4(uiOutput("moe_output")),
      h4(uiOutput("ci_output")),
      plotOutput("ciPlot_output")
    )
  )
)