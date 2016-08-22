library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  ##################################
  #
  # CSS
  #
  ##################################
  tags$head(
    tags$style(HTML("#getSample, #getSample_mn{
                        background-color: #0099cc;
                        color:white;
                        padding: 15px 32px;
                        text-align: center;
                        font-size:16px;
                        font-weight:bold;
                        width: 100%;}
                     #getConfInt, #getConfInt_mn{
                        background-color: #660000;
                        color:white;
                        padding: 15px 32px;
                        text-align: center;
                        font-size:16px;
                        font-weight:bold;
                        width: 100%;}
                     #createConfInts, #createConfInts_mn{
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
                            min = 0.01,
                            max = 0.99,
                            value = 0.5,
                            sep="",
                            step=0.01),
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
                            max = .99,
                            value = 0.95,
                            sep="",
                            step=0.01),
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
                             min = 0.01,
                             max = .99,
                             value = 0.5,
                             sep="",
                             step=0.01),
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
                             max = 0.99,
                             value = 0.95,
                             sep="",
                             step=0.01),
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
    ),
    tabPanel(h4("Calculating Confidence Interval ~ Means"),
             sidebarLayout(
               sidebarPanel(selectInput("ci_mean_pop_dist",
                                        "Choose the population Distribution",
                                        choices = list("Normal" = 1, "Uniform" = 2),
                                        selected = 1),
                            conditionalPanel(condition = "input.ci_mean_pop_dist == 1",
                                             numericInput("mu_ci", 
                                                          "Enter Population Mean",
                                                          0),
                                             numericInput("sd_ci",
                                                          "Enter Population Standard Deviation",
                                                          1)),
                            conditionalPanel(condition = "input.ci_mean_pop_dist == 2",
                                             numericInput("a_ci",
                                                          "Enter Lower Bound for Distribution",
                                                          0),
                                             numericInput("b_ci",
                                                          "Enter Upper Bound for Distribution",
                                                          1)),
                            selectInput("sd_known_ci",
                                        strong("Do we know our Population Standard Deviation?"),
                                        choices = list("Yes" = 1, "No" = 0),
                                        selected = 0),
                            numericInput("sample_size_mn_ci",
                                         "Enter Your Sample Size (>=25)",
                                         35),
                            actionButton(inputId = "getSample_mn",
                                         label = "Get Sample"),
                            sliderInput("conf_lvl_mn_ci",
                                        "Confidence Level",
                                        min = .9,
                                        max = .99,
                                        value = 0.95,
                                        sep="",
                                        step=0.01),
                            actionButton(inputId = "getConfInt_mn",
                                         label = "Get Confidence Interval")  
               ),
               mainPanel(
                 helpText("Note: This exercise assumes no knowledge of the underlying population distribution (i.e., if it is normal or uniform). The only exception is regarding the population standard deviation in the event you selected 'Yes' in that respective dropdown."),
                 withMathJax(),
                 h4(uiOutput("x_bar_output")),
                 h4(uiOutput("samp_sd_output")),
                 h4(uiOutput("se_formula_mn")),
                 uiOutput("break_1_mn"),
                 h4(uiOutput("zt_star_output_mn")),
                 h4(uiOutput("moe_output_mn")),
                 h4(uiOutput("ci_output_mn")),
                 plotOutput("ciPlot_output_mn")
                 
               )
             )
    )
  )
)