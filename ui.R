## ---------------------------
##
## Script name: ui.R
##
## Purpose of script: specify UI for Macroviz R Shiny App
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


ui <- page_sidebar(
  title  = "IS–LM-PC Model",
  theme  = app_theme,
  window_title = "Macroviz IS-LM-PC",
  
  # ── Sidebar ────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 290,
    
    accordion(
      open = c("Parameters", "Policy & Shocks"),
      
      accordion_panel(
        "Policy & Shocks",
        icon = bsicons::bs_icon("lightning-charge"),
        sliderInput("i_set", "Nominal Policy Rate  (i in %)", -1, 12, 4, 0.25),
        sliderInput("G",     "Government Spending (G)",          0, 20, 10, 1),
        sliderInput("Tax",     "Tax income  (T)",          0, 20, 10, 1),
        sliderInput("pi_e",    "Expected Inflation (πᵉ in %)",   0,   8,   2,  0.25),
        sliderInput("Yn",      "Natural Output (Yₙ)",        80, 120, 100, 1),
        sliderInput("x",      "Uncertainty (x)",        0, 10, 0, 1),
        
      ), 
      
      accordion_panel(
        "Advanced Parameters",
        icon = bsicons::bs_icon("sliders"),
        sliderInput("b",     "IS interest sensitivity (b)",  0.5, 5,   2,   0.25),
        sliderInput("alpha", "PC slope (α)",                 0.1, 1.5, 0.4, 0.05),
        sliderInput("pi_star", "Inflation Target π* (%)",     0,   4,   2,  0.5),
        sliderInput("multiplier", "Fiscal Multiplier",     1,   3,   1.5,  0.05)
      )
      
    )
  ),
  
  # ── Main content ───────────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(8, 4),
    
    # Left: stacked plots
    card(
      full_screen = TRUE,
      card_header("Equilibrium"),
      plotOutput("plot_IS", height = "320px"),
      plotOutput("plot_PC", height = "320px")
    ),
    
    # Right: value boxes + table
    layout_columns(
      col_widths = 10,
      fill = FALSE,
      
      value_box(
        title    = "Output per capita  Y",
        value    = textOutput("vb_Y"),
        showcase = bsicons::bs_icon("boxes"),
        theme    = "primary"
      ),
      value_box(
        title    = "Output Gap  Y − Yₙ",
        value    = textOutput("vb_gap"),
        showcase = bsicons::bs_icon("thermometer-half"),
        theme    = value_box_theme(bg = "#fdedec", fg = "#2c3e50")
      ),
      value_box(
        title    = "Real Rate  r = i − πᵉ",
        value    = textOutput("vb_r"),
        showcase = bsicons::bs_icon("bank"),
        theme    = "secondary"
      ),
      value_box(
        title    = "Inflation  π",
        value    = textOutput("vb_pi"),
        showcase = bsicons::bs_icon("graph-up"),
        theme    = value_box_theme(bg = "#fef9e7", fg = "#2c3e50")
      ),
      value_box(
        title    = "Inflation Gap  π − target",
        value    = textOutput("vb_pigap"),
        showcase = bsicons::bs_icon("arrows-expand"),
        theme    = value_box_theme(bg = "#e8f4f8", fg = "#2c3e50")
      )
    )
  )
)

