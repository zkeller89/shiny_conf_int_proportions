library(shiny)

function(input, output) {
  
  # List of Reactive Values
  ## nsize, data, z*
  rv <- reactiveValues(nsize = 1,
                       data = NULL,
                       cf_flag = F)
  
  p_hat <- reactive({mean(rv$data)})
 
  #######################
  #  
  # Event Handling 
  #
  #######################

  observeEvent(input$add1, {
    rv$nsize <- as.integer(rv$nsize + 1)
  })
  observeEvent(input$add25, {
    rv$nsize <- as.integer(rv$nsize + 25)
  })
  observeEvent(input$add100, {
    rv$nsize <- as.integer(rv$nsize + 100)
  })
  observeEvent(input$reset, {
    as.integer(rv$nsize <- 1)
    rv$data <- NULL
    rv$cf_flag <- F
  })

  # Get our sample
  observeEvent(input$getSample, {
    rv$data <- rbinom(rv$nsize, 1, input$true_p)
    rv$cf_flag <- F
  })
  
  observeEvent(input$getConfInt, {
    rv$cf_flag <- T
  })
  
  #######################
  # 
  # eventReactive Objects 
  #
  #######################
 
      
  # Show p-hat after get sample
  p_hat <- eventReactive(input$getSample, {
    round(mean(rv$data), 4)
  })
  
  # z-start reactive object
  z_star <- eventReactive(input$getConfInt, {
    round(qnorm(input$conf_lvl), 4)
   })
  
  # se-phat calculation
  se_phat <- eventReactive(input$getSample, {
    sqrt( (p_hat() * (1 - p_hat())) / rv$nsize )
  })
  
  # confidence interval calculation
  ci <- eventReactive(input$getConfInt, {
    round(p_hat() + c(-1,1) * z_star() * se_phat(), 4)
  })
  
  #######################
  # 
  # Display Text 
  #
  #######################
  
  # text to display for standard error p-hat
  se_text <- eventReactive(input$getSample, {
    sprintf("Standard Error of \\(\\hat{p} = \\text{s.e.}(\\hat{p}) =  \\sqrt{\\frac{(%.04f)(1-%.04f)}{%.00f}} = %.04f\\)",
            p_hat(),
            p_hat(),
            rv$nsize,
            se_phat() )
  })
  
  # text to display for p-hat
  p_hat_text <- eventReactive(input$getSample, {
    sprintf("Sample Proportion \\((\\hat{p}): %.04f\\)",
            p_hat())
  })
  
  # text to display for z-start
  z_star_text <- eventReactive(input$getConfInt, {
    sprintf("\\(Z^*\\) Multiplier = %.04f",
            z_star())
  })
  
  # text for margin of error
  moe_text <- eventReactive(input$getConfInt, {
    sprintf("Margin of Error \\(= Z^* \\times \\text{s.e.}(\\hat{p}) = %.04f \\times %.04f = %.04f\\)",
            z_star(),
            se_phat(),
            z_star() * se_phat())
  })
  
  # text for confidence interval
  ci_text <- eventReactive(input$getConfInt, {
    sprintf("Our Interval: \\(\\hat{p}\\pm Z^* \\times \\text{s.e.}(\\hat{p}) = %.04f \\pm %.04f \\times %.04f = ( %.04f, %.04f) \\)",
            p_hat(),
            z_star(),
            se_phat(),
            ci()[1],
            ci()[2] )
  })
  
  #######################
  #
  # Create Output Objects 
  #
  ########################

  # output object to show n-size
  output$sample_size_text <- renderText(rv$nsize)
  
  # output object to show p-hat text
  output$p_hat_output <- renderUI({
    if (is.null(rv$data)){
      return()
    } else{
    withMathJax(p_hat_text())
    }
  })
  
  # output object to show z-star
  output$z_star_output <- renderUI({
    if (rv$cf_flag){
    withMathJax(z_star_text())
    } else{
      return()
    }
    })
  
  # output object to show std error formula
  output$se_formula <- renderUI({
    if (is.null(rv$data)){
      return()
    } else {
      withMathJax(se_text())
    } 
  })
  
  # output object to show margin of error
  output$moe_output <- renderUI({
    if (rv$cf_flag) {
      if (is.null(rv$data)){
        "Please Create a Sample"
      } else{
        withMathJax(moe_text())
      }
    } else {
      return()
    }
  })
  
  # output object to show confidence interval calculation
  output$ci_output <- renderUI({
    if(rv$cf_flag){
      if (is.null(rv$data)){
        return()
      } else {
      withMathJax(ci_text())
      }
    } else {
      return()
    }
  })
  
  # output plot to show our interval
  output$ciPlot_output <- renderPlot({
    if (rv$cf_flag){
      if (is.null(rv$data)){
        return()
      } else{
      ## define plot data
        xlim <- c(0,1)
        ylim <- c(-.1,0.1)
        px <- ci()
        py <- c(0,0)
        lx <- px
        ly <- 0.05
  
        ## create basic plot outline
        par(xaxs='i',yaxs='i')
        plot(NA,xlim=xlim,ylim=ylim,axes=F,ann=F)
        axis(1, pos = c(0,0))
  
        ## plot elements
        segments(px,py,lx,ly)
        segments(px[2], 0, px[1], 0, col="red", lwd=6)
        points(px,py,pch=21,xpd=T, cex= 2, bg="grey")
        text(lx,ly,px,pos=3)
      }
    } else {
      return()
    }
  })
  

}