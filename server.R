library(shiny)

function(input, output) {

  ####################################################################################
  #
  # This is for Confidence Interval UI
  #
  ####################################################################################
  
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

  
  # Get our sample
  observeEvent(input$getSample, {
    rv$data <- rbinom(input$sample_size, 1, input$true_p_ci)
    rv$cf_flag <- F
  })
  
  observeEvent(input$getConfInt, {
    rv$cf_flag <- T
  })
  
  
  #######################
  # 
  # eventReactive Objects that aren't text
  #
  #######################
 
      
  # Show p-hat after get sample
  p_hat <- eventReactive(input$getSample, {
    round(mean(rv$data), 4)
  })
  
  # z-start reactive object
  z_star <- eventReactive(input$getConfInt, {
    round(qnorm(1 - ((1 - input$conf_lvl_ci)/2)), 4)
   })
  
  # se-phat calculation
  se_phat <- eventReactive(input$getSample, {
    sqrt( (p_hat() * (1 - p_hat())) / input$sample_size )
  })
  
  # confidence interval calculation
  ci <- eventReactive(input$getConfInt, {
    round(p_hat() + c(-1,1) * z_star() * se_phat(), 4)
  })
  
  #######################
  # 
  # Display Text (event reactive objects that are text we'll display)
  #
  #######################
  
  # text to display for standard error p-hat
  se_text <- eventReactive(input$getSample, {
    sprintf("Standard Error of \\(\\hat{p} = \\text{s.e.}(\\hat{p}) =  \\sqrt{\\frac{(%.04f)(1-%.04f)}{%.00f}} = %.04f\\)",
            p_hat(),
            p_hat(),
            input$sample_size,
            se_phat() )
  })
  
  # text to display for p-hat
  p_hat_text <- eventReactive(input$getSample, {
    sprintf("Sample Proportion \\((\\hat{p}): %.04f\\)",
            p_hat())
  })
  
  # text to display for z-start
  z_star_text <- eventReactive(input$getConfInt, {
    sprintf("\\(z^*\\) Multiplier = %.04f",
            z_star())
  })
  
  # text for margin of error
  moe_text <- eventReactive(input$getConfInt, {
    sprintf("Margin of Error \\(= z^* \\times \\text{s.e.}(\\hat{p}) = %.04f \\times %.04f = %.04f\\)",
            z_star(),
            se_phat(),
            z_star() * se_phat())
  })
  
  # text for confidence interval
  ci_text <- eventReactive(input$getConfInt, {
    sprintf("Our Interval: \\(\\hat{p}\\pm z^* \\times \\text{s.e.}(\\hat{p}) \\Rightarrow %.04f \\pm %.04f \\times %.04f \\Rightarrow ( %.04f, %.04f) \\)",
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
  output$sample_size_text <- renderText(input$sample_size)
  
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
  
  # output object split between se-phat and z*/conf int
  output$break_1 <- renderUI({
    if (is.null(rv$data)){
      return()
    } else {
      HTML("<hr />")
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
        ylim <- c(-.1, 0.1)
        px <- c(ci(), isolate(input$true_p_ci))
        py <- c(0, 0, 0)
        lx <- px
        ly <- c(0.05, 0.05, 0.075)
  
        ## create basic plot outline
        par(xaxs='i',yaxs='i')
        plot(NA,xlim=xlim,ylim=ylim,axes=F,ann=F)
        axis(1, pos = c(0,0))
  
        ## plot elements
        segments(px,py,lx,ly)
        segments(px[2], 0, px[1], 0, col="red", lwd=6)
        points(px,py,pch=21,xpd=T, cex=2, bg="grey")
        points(isolate(input$true_p_ci), 0, pch=21, xpd=T, cex=2, bg="lightgreen")
        text(lx,ly,c(as.character(ci()), "True Proportion"),pos=3)
      }
    } else {
      return()
    }
  })

  ####################################################################################
  #
  # This is for Confidence Level UI
  #
  ####################################################################################
  
  # List of Reactive Values
  ## nsize, data, z*
  rv_cl <- reactiveValues(conf_ints_cl = NULL,
                          cf_flag_cl = F)
  
  #######################
  #  
  # Event Handling 
  #
  #######################  
  
  observeEvent(input$getConfInt, {
    rv_cl$cf_flag_cl <- T
  })
  
  # Get our data for confidence intervals
  observeEvent(input$createConfInts, {
    num_ints <- input$num_conf_ints_to_create
    sample_size <- input$sample_size_cl
    true_p <- input$true_p_cl
    c_level <- input$conf_lvl_cl
    
    cl_data <- matrix(nrow = num_ints, ncol=sample_size)
    
    for (i in 1:num_ints){
      temp.samp <- rbinom(sample_size, 1, true_p)
      cl_data[i,] <- temp.samp
    }
    
    phats <- rowMeans(cl_data)
    
    cis <- phats + matrix(rep(c(0,-1,1), each = num_ints), nrow = num_ints, ncol=3) * qnorm(1 - ((1 - c_level)/2)) * sqrt((phats * (1-phats)) / sample_size)
    cis <- data.frame(cis)
    names(cis) <- c("phat", "lower", "upper")
    cis$tp_flag <- true_p >= cis$lower & true_p <= cis$upper
    
    rv_cl$conf_ints_cl <- cis
    rv$cf_flag_cl <- T
  })
  
  
  #######################
  # 
  # eventReactive Objects 
  #
  #######################
  
  ci_in_out <- eventReactive(input$createConfInts, {
    in_out <- rv_cl$conf_ints_cl$tp_flag
    c(sum(in_out), sum(!in_out))
  })
  
  num_ints <- eventReactive(input$createConfInts, {
    input$num_conf_ints_to_create
  })
  
  #######################
  # 
  # Display Text 
  #
  #######################
  
  output$cl_text <- renderText({
    if (is.null(rv_cl$conf_ints_cl)){
      return()      
    } else {
      paste("We see that",
            strong(ci_in_out()[1]),
            "of our",
            strong(sum(ci_in_out())),
            "generated confidence intervals",
            strong(paste("(",round(ci_in_out()[1]/sum(ci_in_out()),4) * 100, "%)", sep="")),
            "did contain the true propportion of",
            strong(isolate(input$true_p_cl)),
            "while ",
            strong(ci_in_out()[2]),
            "did not. With a ",
            strong(paste(round(input$conf_lvl_cl,4) * 100, "%", sep="")),
            "confidence level, we would expect (in the long run) about",
            strong(paste(round(input$conf_lvl_cl,4) * 100, "%", sep="")),
            "of the intervals to contain the true proportion",
            sep=" ")
    }

  })
  
  #######################
  #
  # Create Output Objects 
  #
  ########################
  
  # ouput object for graphs
  output$c_int_plot <- renderPlot({
      if (is.null(rv_cl$conf_ints_cl)){
        return()
      } else{
      library(ggplot2)
      ggplot() + geom_errorbar(data=rv_cl$conf_ints_cl, mapping=aes(x = 1:num_ints(), ymin=lower, ymax=upper),
                               color= ifelse(rv_cl$conf_ints_cl$tp_flag, "blue", "red")) + 
        coord_cartesian(ylim = c(0,1)) +
        scale_x_discrete(breaks=NULL) +
        scale_y_continuous(breaks = seq(0,1,0.1), minor_break = NULL, limits = c(-.1,1.1)) + 
        coord_flip() +
        xlab("") +
        theme(legend.position="none") +
        geom_hline(aes(yintercept=isolate(input$true_p_cl)))
    }
  })

}