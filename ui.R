library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  ##################################
  #
  # CSS
  #
  ##################################
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
                     #createConfInts{
                        background-color: #660000;
                        color:white;
                        padding: 15px 32px;
                        text-align: center;
                        font-size:16px;
                        font-weight:bold;
                        width: 100%;}
                     .nav{
                        margin-bottom:1em;
                     }
                     .irs-grid-text{
                        font-size:12px;
                     }
                     #cl_text{
                        font-size:16px;
                     }
                    "))
  ),
  
  

  tabsetPanel(
    
    #################################
    #
    # Confidence Interval Tab
    #
    #################################
    
    tabPanel(h4("Calculating a Confidence Interval ~ Proportions"),
             

      # Sidebar with a slider input for the number of bins
             sidebarLayout(
              sidebarPanel(
                sliderInput("true_p_ci",
                            "True Proportion",
                            min = 0.0001,
                            max = 0.9999,
                            value = 0.5,
                            sep=""),
                numericInput("sample_size",
                             "Enter Your Sample Size (>=10)",
                             value=10,
                             min=10,
                             max=5000,
                             step=1),
                br(),
                actionButton(inputId = "getSample",
                             label = "Get Sample"),
                br(),br(),
                sliderInput("conf_lvl_ci",
                            "Confidence Level",
                            min = .9,
                            max = .9999,
                            value = 0.95,
                            sep=""),
                br(),
                actionButton(inputId = "getConfInt",
                             label = "Get Confidence Interval")      
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                withMathJax(),
                h4(uiOutput("p_hat_output")),
                h4(uiOutput("se_formula")),
                uiOutput("break_1"),
                h4(uiOutput("z_star_output")),
                h4(uiOutput("moe_output")),
                h4(uiOutput("ci_output")),
                plotOutput("ciPlot_output")
              )
             )
    ),
    
    #################################
    #
    # Confidence Level Tab
    #
    #################################
    
    tabPanel(h4("Understanding the Confidence Level ~ Proportions"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("true_p_cl",
                             "True Proportion",
                             min = 0.0001,
                             max = .9999,
                             value = 0.5,
                             sep=""),
                 helpText("Note: We generally do not know the True Proportion but we get to set the value here to see how well the confidence interval estimation process works"),
                 numericInput("sample_size_cl",
                              "Enter Your Sample Size (>=10)",
                              value=25,
                              min=25,
                              max=5000,
                              step=1),
                 sliderInput("conf_lvl_cl",
                             "What Confidence Level do you want to use?",
                             min = .90,
                             max = 0.9999,
                             value = 0.95,
                             sep=""),
                 sliderInput("num_conf_ints_to_create",
                             "How Many Confidence Interval Should We Make?",
                             min = 10,
                             max = 200,
                             value = 100),
                 actionButton(inputId = "createConfInts",
                              label = "Create Confidence Intervals")
               ),
        mainPanel(
          plotOutput("c_int_plot"),
          htmlOutput("cl_text")
        )
      )
    )
  )
)