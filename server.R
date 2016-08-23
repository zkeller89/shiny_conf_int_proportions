library(shiny)

function(input, output) {

  ####################################################################################
  #
  # This is for Confidence Interval for proportions
  #
  ####################################################################################
  
  # List of Reactive Values
  ## nsize, data, z*
  rv <- reactiveValues(data = NULL,
                       cf_flag = F)
  
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
    our_ci <- round(p_hat() + c(-1,1) * z_star() * se_phat(), 4)
    if(our_ci[1] < 0){
      our_ci[1] = 0
    }
    if(our_ci[2] > 1){
      our_ci[2] = 1
    }
    our_ci
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
        px <- ci()
        py <- c(0, 0)
        lx <- c(px)
        ly <- c(0.05, 0.05)
        true_p <- isolate(input$true_p_ci)
        phat <- p_hat()
  
        ## create basic plot outline
        par(xaxs='i',yaxs='i')
        plot(NA,xlim=xlim,ylim=ylim,axes=F,ann=F)
        axis(1, pos = c(0,0))
  
        ## plot elements
        segments(px,py,px,ly)
        segments(px[2], 0, px[1], 0, col="red", lwd=6)
        segments(true_p, 0, true_p, .075)
        segments(phat, 0, phat, -.05)
        points(px,py,pch=21,xpd=T, cex=2, bg="grey")
        points(true_p, 0, pch=21, xpd=T, cex=2, bg="green")
        points(p_hat(), 0, pch = 21, xpd = T, cex = 2, bg = "blue")
        text(lx, ly, ci(), pos=3)
        text(true_p, .075, "True Proportion", pos=3)
        text(phat, -0.05, "p-hat", pos=1)
      }
    } else {
      return()
    }
  })

  ####################################################################################
  #
  # This is for Confidence Level
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
    
    cis <- phats + matrix(rep(c(0,-1,1), each = num_ints),
                          nrow = num_ints, ncol=3) * qnorm(1 - ((1 - c_level)/2)) * sqrt((phats * (1-phats)) / sample_size)
    cis <- data.frame(cis)
    names(cis) <- c("phat", "lower", "upper")
    cis$tp_flag <- true_p >= cis$lower & true_p <= cis$upper
    
    # Edit upper and lower bounds so they're between 0 and 1
    cis$lower[cis$lower < 0] = 0
    cis$upper[cis$upper > 1] = 1
    
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
            strong(paste(round(isolate(input$conf_lvl_cl),4) * 100, "%", sep="")),
            "confidence level, we would expect (in the long run) about",
            strong(paste(round(isolate(input$conf_lvl_cl),4) * 100, "%", sep="")),
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
  
  ####################################################################################
  #
  # This is for Confidence Interval for Population Means
  #
  ####################################################################################
  
  # First create our reactive values
  rv_mn_ci <- reactiveValues(data = NULL,
                       cf_flag = F)
  
  # Get our sample
  observeEvent(input$getSample_mn, {
    # Check if Normal (1) or Uniform (2), elseif for further distributions
    if (input$ci_mean_pop_dist == 1){
      rv_mn_ci$data <- rnorm(input$sample_size_mn_ci, input$mu_ci, input$sd_ci)      
    } else if (input$ci_mean_pop_dist == 2){
      rv_mn_ci$data <- runif(input$sample_size_mn_ci, input$a_ci, input$b_ci)
    }
    
    rv_mn_ci$cf_flag <- F
  })
  
  observeEvent(input$getConfInt_mn, {
    rv_mn_ci$cf_flag <- T
  })
  
  #######################
  # 
  # eventReactive Objects that aren't text
  #
  #######################
  
  # degrees of fredom
  df <- eventReactive(input$getSample_mn, {
    input$sample_size_mn_ci - 1
  })
  # get x-bar after get sample
  x_bar <- eventReactive(input$getSample_mn, {
    round(mean(rv_mn_ci$data), 4)
  })
  
  # Get Sample Standard deviation of data
  samp_sd <- eventReactive(input$getSample_mn, {
    round(sd(rv_mn_ci$data), 4)
  })
  
  # z-star reactive object (in case sigma known)
  z_star_mn <- eventReactive(input$getConfInt_mn, {
    round(qnorm(1 - ((1 - input$conf_lvl_mn_ci)/2)), 4)
  })
  
  # t-star (sigma unknown)
  t_star_mn <- eventReactive(input$getConfInt_mn, {
    round(qt(1 - ((1 - input$conf_lvl_mn_ci)/2), df()), 4)
  })
  
  # se-xbar calculation
  se_xbar_ci <- eventReactive(input$getSample_mn, {
    if (input$sd_known_ci == 1){
      if (input$ci_mean_pop_dist == 1){
        input$sd_ci / sqrt(input$sample_size_mn_ci)
      } else if (input$ci_mean_pop_dist == 2) {
        sqrt(abs(input$b_ci - input$a_ci)^2 / 12) / sqrt(input$sample_size_mn_ci)
      }
    } else{
        sd(rv_mn_ci$data)  / sqrt(input$sample_size_mn_ci)
    }
  })
  
  # confidence interval calculation
  ci_mn <- eventReactive(input$getConfInt_mn, {
    if (input$sd_known_ci == 1){
      our_ci <- round(x_bar() + c(-1,1) * z_star_mn() * se_xbar_ci(), 4)
      our_ci      
    } else {
      our_ci <- round(x_bar() + c(-1,1) * t_star_mn() * se_xbar_ci(), 4)
      our_ci
    }
  })
  
  #######################
  # 
  # Display Text (event reactive objects that are text we'll display) for population means
  #
  #######################
  
  # text to display for standard error p-hat
  se_text_mn <- eventReactive(input$getSample_mn, {
    sprintf("Standard Error of \\(\\bar{x} = \\text{s.e.}(\\bar{x}) =  \\frac{%.04f}{\\sqrt{%.00f}} = %.04f\\)",
            sd(rv_mn_ci$data),
            input$sample_size_mn_ci,
            se_xbar_ci())
  })
  
  # text to display for x-bar
  x_bar_text_mn <- eventReactive(input$getSample_mn, {
    sprintf("Sample Mean \\((\\bar{x}): %.04f\\)",
            x_bar())
  })
  
  # text to display for s (sample std dev)
  samp_sd_text_mn <- eventReactive(input$getSample_mn, {
    sprintf("Sample Standard Deviation,  s: %.04f ",
            samp_sd())
  })
  
  # text to display for z-start
  z_star_text_mn <- eventReactive(input$getConfInt_mn, {
    sprintf("\\(z^*\\) Multiplier = %.04f",
            z_star_mn())
  })
  
  # text to display for z-start
  t_star_text_mn <- eventReactive(input$getConfInt_mn, {
    sprintf("\\(t^*\\) Multiplier = %.04f",
            t_star_mn())
  })
  
  # text for margin of error
  moe_text_mn <- eventReactive(input$getConfInt_mn, {
    if (input$sd_known_ci == 1){
      sprintf("Margin of Error \\(= z^* \\times \\text{s.e.}(\\bar{x}) = %.04f \\times %.04f = %.04f\\)",
              z_star_mn(),
              se_xbar_ci(),
              z_star_mn() * se_xbar_ci())
    } else {
      sprintf("Margin of Error \\(= t^* \\times \\text{s.e.}(\\bar{x}) = %.04f \\times %.04f = %.04f\\)",
              t_star_mn(),
              se_xbar_ci(),
              t_star_mn() * se_xbar_ci())
    }
  })
  
  # text for confidence interval
  ci_text_mn <- eventReactive(input$getConfInt_mn, {
    if (input$sd_known_ci == 1){
      sprintf("Our Interval: \\(\\bar{x}\\pm z^* \\times \\text{s.e.}(\\bar{x}) \\Rightarrow %.04f \\pm %.04f \\times %.04f \\Rightarrow ( %.04f, %.04f) \\)",
              x_bar(),
              z_star_mn(),
              se_xbar_ci(),
              ci_mn()[1],
              ci_mn()[2] )
    } else {
      sprintf("Our Interval: \\(\\bar{x}\\pm t^* \\times \\text{s.e.}(\\bar{x}) \\Rightarrow %.04f \\pm %.04f \\times %.04f \\Rightarrow ( %.04f, %.04f) \\)",
              x_bar(),
              t_star_mn(),
              se_xbar_ci(),
              ci_mn()[1],
              ci_mn()[2] )
    }
  })
  
  #######################
  #
  # Create Output Objects
  #
  ########################
  
  # output object to show p-hat text
  output$x_bar_output <- renderUI({
    if (is.null(rv_mn_ci$data)){
      return()
    } else{
      withMathJax(x_bar_text_mn())
    }
  })
  
  output$samp_sd_output <- renderUI({
    if (is.null(rv_mn_ci$data)){
      return()
      } else {
        withMathJax(samp_sd_text_mn())
      }
  })
  
  # output object to show z-star
  output$zt_star_output_mn <- renderUI({
    if (rv_mn_ci$cf_flag){
      if (isolate(input$sd_known_ci) == 1){
        withMathJax(z_star_text_mn())
      } else {
        withMathJax(t_star_text_mn())
      }
    } else{
      return()
    }
  })
  
  # output object to show std error formula
  output$se_formula_mn <- renderUI({
    if (is.null(rv_mn_ci$data)){
      return()
    } else {
      withMathJax(se_text_mn())
    } 
  })
  
  # output object split between se-phat and z*/conf int
  output$break_1_mn <- renderUI({
    if (is.null(rv_mn_ci$data)){
      return()
    } else {
      HTML("<hr />")
    } 
  })
  
  # output object to show margin of error
  output$moe_output_mn <- renderUI({
    if (rv_mn_ci$cf_flag) {
      if (is.null(rv_mn_ci$data)){
        "Please Create a Sample"
      } else{
        withMathJax(moe_text_mn())
      }
    } else {
      return()
    }
  })
  
  # output object to show confidence interval calculation
  output$ci_output_mn <- renderUI({
    if(rv_mn_ci$cf_flag){
      if (is.null(rv_mn_ci$data)){
        return()
      } else {
        withMathJax(ci_text_mn())
      }
    } else {
      return()
    }
  })
   
  # output plot to show our interval
  output$ciPlot_output_mn <- renderPlot({
    if (rv_mn_ci$cf_flag){
      if (is.null(rv_mn_ci$data)){
        return()
      } else{
        ## define plot data
        len_ci <- abs(ci_mn()[2] - ci_mn()[1])
        xlim <- c(ci_mn()[1] - 0.25 * len_ci, ci_mn()[2] + 0.25 * len_ci)
        ylim <- c(-.1, 0.1)
        px <- ci_mn()
        py <- c(0, 0)
        lx <- c(px)
        ly <- c(0.05, 0.05)
        true_mn <- if (input$ci_mean_pop_dist == 1) {
          isolate(input$mu_ci)
        } else if (input$ci_mean_pop_dist == 2) {
          (isolate(input$a_ci) + isolate(input$b_ci)) / 2
        }
        
        xbar <- x_bar()
        
        ## create basic plot outline
        par(xaxs='i',yaxs='i')
        plot(NA,xlim=xlim,ylim=ylim,axes=F,ann=F)
        axis(1, at = xlim, pos = c(0,0))
        
        ## plot elements
        segments(px,py,px,ly)
        segments(px[2], 0, px[1], 0, col="red", lwd=6)
        segments(true_mn, 0, true_mn, .075)
        segments(xbar, 0, xbar, -.05)
        points(px,py,pch=21,xpd=T, cex=2, bg="grey")
        points(true_mn, 0, pch=21, xpd=T, cex=2, bg="green")
        points(xbar, 0, pch = 21, xpd = T, cex = 2, bg = "blue")
        text(lx, ly, ci_mn(), pos=3)
        text(true_mn, .075, "True Mean", pos=3)
        text(xbar, -0.05, "x-bar", pos=1)
      }
    } else {
      return()
    }
  })
}