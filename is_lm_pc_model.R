library(shiny)
library(ggplot2)
library(bslib)

#themee

app_theme <- bs_theme(
  version    = 5,
  bootswatch = "sandstone",
  base_font  = font_google("IBM Plex Sans"),
  code_font  = font_google("IBM Plex Mono")
)

#ui 

ui <- page_sidebar(
  title = div(
    style = "
      display: flex;
      align-items: center;
      justify-content: space-between;
      width: 100%;
    ",
    span("IS–LM-PC Model"),
    tags$img(
      src = "TUBAF_Logo_blau.png",
      style = "height: 42px; width: auto;"
    )
  ),
  
  theme = app_theme,
  window_title = "Macroviz IS-LM-PC",
  
#Sidebar
  sidebar = sidebar(
    width = 290,
    
    accordion(
      open = c("Parameters", "Policy & Shocks"),
      
      accordion_panel(
        "Policy & Shocks",
        icon = bsicons::bs_icon("lightning-charge"),
        
        sliderInput(
          "i_set",
          "Nominal Policy Rate (i in %)",
          min = -1,
          max = 12,
          value = 4,
          step = 0.25
        ),
        
        sliderInput(
          "G",
          "Government Spending (G)",
          min = 0,
          max = 20,
          value = 10,
          step = 1
        ),
        
        sliderInput(
          "Tax",
          "Tax Income (T)",
          min = 0,
          max = 20,
          value = 10,
          step = 1
        ),
        
        sliderInput(
          "pi_e",
          "Expected Inflation (πᵉ in %)",
          min = 0,
          max = 8,
          value = 2,
          step = 0.25
        ),
        
        sliderInput(
          "Yn",
          "Natural Output (Yₙ)",
          min = 80,
          max = 120,
          value = 100,
          step = 1
        ),
        
        sliderInput(
          "x",
          "Risk Premium (x, in %)",
          min = 0,
          max = 5,
          value = 0,
          step = 0.25
        )
      ),
      
      accordion_panel(
        "Advanced Parameters",
        icon = bsicons::bs_icon("sliders"),
        
        sliderInput(
          "b",
          "Interest Sensitivity of Investment (b₂)",
          min = 0.5,
          max = 5,
          value = 2,
          step = 0.25
        ),
        
        sliderInput(
          "alpha",
          "PC Slope (α)",
          min = 0.1,
          max = 1.5,
          value = 0.4,
          step = 0.05
        ),
        
        sliderInput(
          "pi_star",
          "Inflation Target π* (%)",
          min = 0,
          max = 4,
          value = 2,
          step = 0.5
        )
      )
    )
  ),
  
#Main cont

  layout_columns(
    col_widths = c(8, 4),
    
    # Left: plots
    card(
      full_screen = TRUE,
      card_header("Equilibrium"),
      plotOutput("plot_IS", height = "320px"),
      plotOutput("plot_PC", height = "320px")
    ),
    
    # Right: value boxes
    layout_columns(
      col_widths = 10,
      fill = FALSE,
      
      value_box(
        title = "Equilibrium Output Y",
        value = textOutput("vb_Y"),
        theme = "primary"
      ),
      
      value_box(
        title = "Output Gap Y − Yₙ",
        value = textOutput("vb_gap"),
        theme = value_box_theme(
          bg = "#fdedec",
          fg = "#2c3e50"
        )
      ),
      
      value_box(
        title = "Real Rate r = i − πᵉ",
        value = textOutput("vb_r"),
        theme = "secondary"
      ),
      
      value_box(
        title = "Inflation π",
        value = textOutput("vb_pi"),
        theme = value_box_theme(
          bg = "#fef9e7",
          fg = "#2c3e50"
        )
      ),
      
      value_box(
        title = "Inflation Gap π − target",
        value = textOutput("vb_pigap"),
        theme = value_box_theme(
          bg = "#e8f4f8",
          fg = "#2c3e50"
        )
      )
    )
  )
)

#server

server <- function(input, output, session) {
  
#Real policy rate as r = i - πᵉ
  
  r_real <- reactive({
    input$i_set - input$pi_e
  })
  
  
  model <- reactive({
    
    r <- r_real()
    
# Consumption as C = c0 + c1(Y - T)
    
    c0 <- 15
    c1 <- 0.60
    
# Invest as I = b0 + b1*Y - b2*(r + x)
    
    b0 <- 15
    b1 <- 0.10
    b2 <- input$b
    
    # ── IS equilibrium (refer chap 9 blanchard)────
    #
    # Goods-market equilibrium:
    #
    # Y = C + I + G
    #
    # Substituting:
    #
    # Y = c0 + c1(Y-T)
    #     + b0 + b1Y
    #     - b2(r+x)
    #     + G
    #
    # Solving for Y:
    #
    # Y =
    # [c0 + b0 + G - c1*T - b2(r+x)]
    # --------------------------------
    #       1 - c1 - b1
    
    Y <- (
      c0 +
        b0 +
        input$G -
        c1 * input$Tax -
        b2 * (r + input$x)
    ) / (1 - c1 - b1)
    
    # ── Phillips Curve ─
    #
    # π - πᵉ = α(Y - Yn)
    
    delta_pi <- input$alpha * (Y - input$Yn)
    
    inflation <- input$pi_e + delta_pi
    
# Data for plots 
    
    Y_seq <- seq(60, 140, length.out = 300)
    
    # IS curve:
    #
    # Rearranging the same IS equilibrium equation for r:
    #
    # r =
    # [c0 + b0 + G - c1*T - (1-c1-b1)Y] / b2 - x
    
    IS_df <- data.frame(
      Y = Y_seq,
      r = (
        c0 +
          b0 +
          input$G -
          c1 * input$Tax -
          (1 - c1 - b1) * Y_seq
      ) / b2 - input$x
    )
    
    # Phillips Curve: π - πᵉ = α(Y - Yn)
    
    PC_df <- data.frame(
      Y = Y_seq,
      pi = input$alpha * (Y_seq - input$Yn)
    )
    
    # Useing only for placing the IS label
    r_ceil <- 12
    
    Y_at_ceil <- (
      c0 +
        b0 +
        input$G -
        c1 * input$Tax -
        b2 * (r_ceil + input$x)
    ) / (1 - c1 - b1)
    
    # ── Returning the model values 
    
    list(
      Y = Y,
      r = r,
      pi = inflation,
      delta_pi = delta_pi,
      IS_df = IS_df,
      PC_df = PC_df,
      Y_at_ceil = Y_at_ceil
    )
  })
  
#Shared ggplot theme 
  
  plot_theme <- theme_minimal(base_size = 13) +
    theme(
      plot.subtitle = element_text(
        color = "gray55",
        size = 11
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "gray92"
      ),
      plot.margin = margin(
        8, 14, 2, 14
      )
    )
  
#IS–LM Plot
  
  output$plot_IS <- renderPlot({
    
    iso_model <- model()
    
    ggplot(
      iso_model$IS_df,
      aes(Y, r)
    ) +
      geom_line(
        color = "#2980b9",
        linewidth = 1.4
      ) +
      
      # LM curve
      geom_hline(
        yintercept = iso_model$r,
        color = "#e74c3c",
        linewidth = 1
      ) +
      
      # Natural output
      geom_vline(
        xintercept = input$Yn,
        color = "gray70",
        linetype = "dotted",
        linewidth = 0.9
      ) +
      
      # Equilibrium output
      geom_vline(
        xintercept = iso_model$Y,
        color = "#f39c12",
        linetype = "dotted",
        linewidth = 1.2
      ) +
      
      # Equilibrium point
      geom_point(
        aes(
          x = iso_model$Y,
          y = iso_model$r
        ),
        color = "white",
        size = 5,
        shape = 21,
        fill = "#f39c12",
        stroke = 1.5
      ) +
      
      # LM label
      annotate(
        "text",
        x = 136,
        y = iso_model$r + 0.55,
        label = "LM",
        color = "#e74c3c",
        size = 5.5,
        fontface = "bold"
      ) +
      
      # IS label
      annotate(
        "text",
        x = 66,
        y = 11,
        label = "IS",
        color = "#2980b9",
        size = 5.5,
        fontface = "bold"
      ) +
      
      labs(
        x = NULL,
        y = "Real Rate r (%)"
      ) +
      
      coord_cartesian(
        ylim = c(-3, 12),
        xlim = c(60, 140)
      ) +
      
      plot_theme
    
  }, res = 110)
  
#Phillips Curve Plot
  
  output$plot_PC <- renderPlot({
    
    iso_model <- model()
    
    ggplot(
      iso_model$PC_df,
      aes(Y, pi)
    ) +
      
      geom_line(
        color = "#27ae60",
        linewidth = 1.4
      ) +
      
      # Zero inflation-gap line
      geom_hline(
        yintercept = 0,
        color = "#000",
        linetype = "solid",
        linewidth = 0.5
      ) +
      
      # Natural output
      geom_vline(
        xintercept = input$Yn,
        color = "gray70",
        linetype = "dotted",
        linewidth = 0.9
      ) +
      
      # Equilibrium output
      geom_vline(
        xintercept = iso_model$Y,
        color = "#f39c12",
        linetype = "dotted",
        linewidth = 1.2
      ) +
      
      # Equilibrium point
      geom_point(
        aes(
          x = iso_model$Y,
          y = iso_model$delta_pi
        ),
        color = "white",
        size = 5,
        shape = 21,
        fill = "#f39c12",
        stroke = 1.5
      ) +
      
      # Natural output label
      annotate(
        "text",
        x = input$Yn + 1,
        y = 0.7,
        label = "Yₙ",
        color = "gray55",
        size = 3.8,
        hjust = 0
      ) +
      
      # PC label
      annotate(
        "text",
        x = 136,
        y = tail(iso_model$PC_df$pi, 1) + 0.6,
        label = "PC",
        color = "#27ae60",
        size = 5.5,
        fontface = "bold"
      ) +
      
      labs(
        x = "Output Y",
        y = "π − πᵉ (%)"
      ) +
      
      coord_cartesian(
        ylim = c(-6, 16),
        xlim = c(60, 140)
      ) +
      
      plot_theme +
      
      theme(
        plot.margin = margin(
          2, 14, 8, 14
        )
      )
    
  }, res = 110)
  
#Value boxes 
  
  fmt <- function(x, suffix = "") {
    paste0(round(x, 2), suffix)
  }
  
  output$vb_Y <- renderText({
    fmt(model()$Y)
  })
  
  output$vb_gap <- renderText({
    fmt(model()$Y - input$Yn)
  })
  
  output$vb_r <- renderText({
    fmt(model()$r, " %")
  })
  
  output$vb_pi <- renderText({
    fmt(model()$pi, " %")
  })
  
  output$vb_pigap <- renderText({
    fmt(model()$pi - input$pi_star, " %")
  })
}


shinyApp(ui, server)