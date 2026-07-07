## ---------------------------
##
## Script name: server.R
##
## Purpose of script: execute Macroviz R Shiny App in R environment
##
## Date Created: 2026-07-07
##
## Licensed under the MIT License. See LICENSE file in the project root for details.
## 
## ---------------------------
##
## Notes:  
##        
##
## ---------------------------



server <- function(input, output, session) {
  
  # ── Derived quantities ──────────────────────────────────────────────────────
  r_real    <- reactive({ input$i_set - input$pi_e }) #get real interest rate
  delta_G   <- reactive({ input$G - input$Tax}) #get Budget deficit
  
  # ── Core model ──────────────────────────────────────────────────────
  
  model <- reactive({
    r  <- r_real()
    Y  <- 100 + delta_G() * input$multiplier - input$b * (r+input$x) 
    delta_pi <- input$alpha * (Y - input$Yn) 
    
    # This is the y axis 
    Y_seq <- seq(60, 140, length.out = 300)
    
    # Get IS data to be returned (needed for plot)
    IS_df <- data.frame(
      Y = Y_seq,
      r = (100 + delta_G() * input$multiplier - Y_seq) / input$b - input$x
    )
    
    # Get PC data to be returned (needed for plot)
    PC_df <- data.frame(
      Y   = Y_seq,
      pi  = input$alpha * (Y_seq - input$Yn)
    )
    
    
    #we need the following data in order to place the IS label in the plot
    r_ceil <- 12
    Y_at_ceil <- 100 + delta_G() * input$multiplier - input$b * (r_ceil + input$x)
    
    
    #return
    list(
      Y      = Y, #equilibrium Y
      r      = r, #equilibrium r
      pi     = delta_pi + input$pi_e, #equilibrium inflation
      delta_pi = delta_pi, #equilibrium inflation due to output gap
      IS_df  = IS_df, # IS curve data
      PC_df  = PC_df, #PC curve data
      Y_at_ceil = Y_at_ceil #maximum Y (needed for placement of IS label)
    )
  })
  
  
  # ── Shared ggplot theme ──────────────────────────────────────────────────────
  plot_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.subtitle     = element_text(color = "gray55", size = 11),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "gray92"),
      plot.margin       = margin(8, 14, 2, 14)
    )
  
  # ── IS Plot ──────────────────────────────────────────────────────────────────
  output$plot_IS <- renderPlot({
    
    #get *reactive* model elements, make them inactive
    iso_model <- model()
    
    #make plot
    ggplot(iso_model$IS_df, aes(Y, r)) +
      geom_line(color = "#2980b9", linewidth = 1.4) + #IS curve
      geom_hline(yintercept = iso_model$r,
                 color = "#e74c3c", linewidth = 1) + #LM curve
      geom_vline(xintercept = input$Yn,
                 color = "gray70", linetype = "dotted", linewidth = 0.9) +
      geom_vline(xintercept = iso_model$Y,
                 color = "#f39c12", linetype = "dotted", linewidth = 1.2) +
      geom_point(aes(x = iso_model$Y, y = iso_model$r), #makes a white ring, needs a point value (at equilibrium)
                 color = "white", size = 5, shape = 21,
                 fill = "#f39c12", stroke = 1.5) +
      annotate("text", x = 136, y = iso_model$r + 0.55,
               label = "LM", color = "#e74c3c", size = 5.5, fontface = "bold") +
      annotate("text", x = iso_model$Y_at_ceil-2 , y = 11.5,
               label = "IS", color = "#2980b9", size = 5.5, fontface = "bold") +
      labs(x = NULL, y = "Real Rate  r (%)") +
      coord_cartesian(ylim = c(-3, 12), xlim = c(60, 140)) +
      plot_theme
  }, res = 110)
  
  # ── PC Plot ───────────────────────────────────────────────────────────────────
  output$plot_PC <- renderPlot({
    
    #get reactive model elements 
    iso_model <- model()
    
    #make plot
    ggplot(iso_model$PC_df, aes(Y, pi)) +
      geom_line(color = "#27ae60", linewidth = 1.4) +       #PC curve
      geom_hline(yintercept = 0,
                 color = "#000", linetype = "solid", linewidth = 0.5) +   #mark 0
      geom_vline(xintercept = input$Yn,
                 color = "gray70", linetype = "dotted", linewidth = 0.9) + #mark Yn
      geom_vline(xintercept = iso_model$Y,
                 color = "#f39c12", linetype = "dotted", linewidth = 1.2) + #mark Y
      geom_point(aes(x = iso_model$Y, y = iso_model$delta_pi),
                 color = "white", size = 5, shape = 21, fill = "#f39c12", stroke = 1.5) + #mark equilibrium
      annotate("text", x = input$Yn + 1, y = max(iso_model$pi, na.rm = TRUE) - 0.8,
               label = "Yₙ", color = "gray55", size = 3.8, hjust = 0) +
      annotate("text", x = 136, y = tail(iso_model$pi, 1) + 0.6,
               label = "PC", color = "#27ae60", size = 5.5, fontface = "bold") +
      labs(x = "Output  Y", y = "π-πᵉ (%)") +
      coord_cartesian(ylim = c(-6, 16), xlim = c(60, 140)) +
      plot_theme +
      theme(plot.margin = margin(2, 14, 8, 14))
  }, res = 110)
  
  # ── Value boxes ───────────────────────────────────────────────────────────────
  fmt <- function(x, suffix = "") paste0(round(x, 2), suffix)
  
  output$vb_Y     <- renderText({ fmt(model()$Y) })
  output$vb_gap   <- renderText({ fmt(model()$Y  - input$Yn) })
  output$vb_r     <- renderText({ fmt(model()$r,  " %") })
  output$vb_pi    <- renderText({ fmt(model()$pi, " %") })
  output$vb_pigap <- renderText({ fmt(model()$pi - input$pi_star, " %") })
}