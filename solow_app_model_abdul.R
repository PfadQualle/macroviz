library(shiny)
library(bslib)
library(ggplot2)

f_output <- function(k, alpha, A) {
  A * k^alpha
}

k_steady <- function(s, d, alpha, A) {
  (s * A / d)^(1 / (1 - alpha))
}

y_steady <- function(s, d, alpha, A) {
  k_ss <- k_steady(s, d, alpha, A)
  f_output(k_ss, alpha, A)
}

c_steady <- function(s, d, alpha, A) {
  (1 - s) * y_steady(s, d, alpha, A)
}

s_gold <- function(alpha) alpha

next_k <- function(k, s, d, alpha, A) {
  k + s * f_output(k, alpha, A) - d * k
}

path_df <- function(k0, s, d, alpha, A, n_periods = 100) {
  k <- numeric(n_periods + 1)
  k[1] <- k0
  for (i in 2:(n_periods + 1)) {
    k[i] <- next_k(k[i - 1], s, d, alpha, A)
  }
  y <- f_output(k, alpha, A)
  data.frame(period = 0:n_periods, capital = k, output = y, consumption = (1 - s) * y)
}

my_theme <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

solow_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    title = "The Solow Growth Model",
    theme = bs_theme(version = 5, preset = "flatly"),
    sidebar = sidebar(
      width = 320, title = "Parameters",
      sliderInput(ns("savings"), "Savings rate (s)", min = 0.05, max = 0.60, value = 0.25, step = 0.01),
      helpText("Share of output saved and invested each year. Higher s means more capital",
               "gets built up over time, but less output is left for consumption today."),
      
      sliderInput(ns("depreciation"), HTML("Depreciation rate (&delta;)"), min = 0.02, max = 0.15, value = 0.05, step = 0.005),
      helpText("Fraction of capital that wears out every year. Faster depreciation means",
               "more investment is needed just to keep capital per worker from falling."),
      
      sliderInput(ns("tech"), "Technology level (A)", min = 0.5, max = 3.0, value = 1.0, step = 0.1),
      helpText("Scales how much output comes out of a given amount of capital.",
               "A higher A raises output at every level of k, which shifts the whole steady state up."),
      
      hr(),
      tags$strong("Advanced"),
      sliderInput(ns("alpha"), HTML("Capital share (&alpha;)"), min = 0.10, max = 0.70, value = 0.33, step = 0.01),
      helpText("Exponent on capital in y = A k^alpha. Also happens to equal the golden-rule savings rate."),
      
      sliderInput(ns("k0"), HTML("Initial capital per worker (k&#8320;)"), min = 0.1, max = 30, value = 1, step = 0.1),
      helpText("Starting point for the convergence chart on the right.")
    ),
    
    layout_columns(
      col_widths = c(4, 8),
      layout_columns(
        col_widths = 12,
        value_box(title = "Steady-state capital per worker (k*)", value = textOutput(ns("box_k")), theme = "primary"),
        value_box(title = "Steady-state output per worker (y*)", value = textOutput(ns("box_y")), theme = "secondary"),
        value_box(title = "Steady-state consumption per worker (c*)", value = textOutput(ns("box_c")), theme = "success")
      ),
      card(
        full_screen = TRUE,
        card_header("Steady-state consumption and the savings rate"),
        plotOutput(ns("plot_c_vs_s"), height = "460px"),
        card_footer(textOutput(ns("golden_note")))
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("The Solow diagram: investment versus depreciation"),
           plotOutput(ns("plot_diagram"), height = "380px")),
      card(full_screen = TRUE, card_header("Convergence to the steady state"),
           plotOutput(ns("plot_path"), height = "380px"))
    )
  )
}

solow_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    kstar <- reactive(k_steady(input$savings, input$depreciation, input$alpha, input$tech))
    ystar <- reactive(f_output(kstar(), input$alpha, input$tech))
    cstar <- reactive((1 - input$savings) * ystar())
    gold  <- reactive(s_gold(input$alpha))
    
    fmt <- function(x) formatC(x, format = "f", digits = 2)
    
    output$box_k <- renderText(fmt(kstar()))
    output$box_y <- renderText(fmt(ystar()))
    output$box_c <- renderText(fmt(cstar()))
    
    output$golden_note <- renderText({
      g <- gold()
      s <- input$savings
      verdict <- if (abs(s - g) < 1e-8) {
        "exactly at the consumption-maximising rate"
      } else if (s < g) {
        "below it, so saving a bit more would actually raise consumption in the long run"
      } else {
        "above it, so saving less would raise long-run consumption"
      }
      paste0("Golden-rule savings rate: ", sprintf("%.2f", g), ". You're currently at ",
             sprintf("%.2f", s), ", which is ", verdict, ".")
    })
    
    output$plot_c_vs_s <- renderPlot({
      s_seq <- seq(0.01, 0.99, by = 0.005)
      cons  <- sapply(s_seq, function(s) c_steady(s, input$depreciation, input$alpha, input$tech))
      df <- data.frame(savings = s_seq, consumption = cons)
      
      g <- gold()
      c_at_gold <- c_steady(g, input$depreciation, input$alpha, input$tech)
      
      ggplot(df, aes(savings, consumption)) +
        geom_line(linewidth = 1.1, colour = "#2c7fb8") +
        geom_vline(xintercept = input$savings, linetype = "dotted", linewidth = 0.9, colour = "#31a354") +
        geom_vline(xintercept = g, linetype = "dashed", linewidth = 0.9, colour = "#d95f0e") +
        geom_point(data = data.frame(savings = input$savings, consumption = cstar()), size = 3.5, colour = "#31a354") +
        geom_point(data = data.frame(savings = g, consumption = c_at_gold), size = 3.5, colour = "#d95f0e") +
        annotate("text", x = input$savings, y = max(df$consumption) * 1.02,
                 label = paste0("your s = ", sprintf("%.2f", input$savings)), hjust = -0.05, colour = "#31a354", fontface = "bold") +
        annotate("text", x = g, y = c_at_gold,
                 label = paste0("golden rule s = ", sprintf("%.2f", g)), hjust = -0.08, vjust = -1.2, colour = "#d95f0e", fontface = "bold") +
        scale_x_continuous(limits = c(0, 1)) +
        labs(title = "Saving more doesn't raise consumption forever",
             subtitle = "Steady-state consumption per worker as a function of s, holding delta and A fixed",
             x = "Savings rate (s)", y = "Steady-state consumption per worker (c*)") +
        my_theme
    })
    
    output$plot_diagram <- renderPlot({
      kmax <- max(kstar() * 1.8, input$k0 * 1.2, 1)
      k_seq <- seq(0, kmax, length.out = 400)
      out <- f_output(k_seq, input$alpha, input$tech)
      inv <- input$savings * out
      dep <- input$depreciation * k_seq
      
      curves <- rbind(
        data.frame(capital = k_seq, value = out, series = "Output per worker  y = A k^alpha"),
        data.frame(capital = k_seq, value = inv, series = "Investment  s*f(k)"),
        data.frame(capital = k_seq, value = dep, series = "Depreciation  delta*k")
      )
      
      ggplot(curves, aes(capital, value, colour = series)) +
        geom_line(linewidth = 1.05) +
        geom_vline(xintercept = kstar(), linetype = "dotted", linewidth = 0.8, colour = "grey30") +
        geom_point(data = data.frame(capital = kstar(), value = input$savings * ystar(), series = "Investment  s*f(k)"),
                   size = 3.5, show.legend = FALSE) +
        annotate("text", x = kstar(), y = 0, label = paste0("k* = ", sprintf("%.2f", kstar())),
                 vjust = -0.5, hjust = -0.08, colour = "grey20", fontface = "bold") +
        scale_colour_manual(values = c(
          "Output per worker  y = A k^alpha" = "#54278f",
          "Investment  s*f(k)" = "#2c7fb8",
          "Depreciation  delta*k" = "#d95f0e"
        )) +
        labs(title = "The steady state is where s*f(k) crosses delta*k",
             x = "Capital per worker (k)", y = "Output, investment, depreciation per worker", colour = NULL) +
        my_theme
    })
    
    output$plot_path <- renderPlot({
      df <- path_df(input$k0, input$savings, input$depreciation, input$alpha, input$tech, n_periods = 150)
      
      long <- rbind(
        data.frame(period = df$period, value = df$capital, series = "Capital per worker (k)"),
        data.frame(period = df$period, value = df$output, series = "Output per worker (y)"),
        data.frame(period = df$period, value = df$consumption, series = "Consumption per worker (c)")
      )
      lvls <- c("Capital per worker (k)", "Output per worker (y)", "Consumption per worker (c)")
      long$series <- factor(long$series, levels = lvls)
      
      targets <- data.frame(series = factor(lvls, levels = lvls), value = c(kstar(), ystar(), cstar()))
      
      ggplot(long, aes(period, value, colour = series)) +
        geom_hline(data = targets, aes(yintercept = value, colour = series), linetype = "dashed", linewidth = 0.7, show.legend = FALSE) +
        geom_line(linewidth = 1.05) +
        scale_colour_manual(values = c(
          "Capital per worker (k)" = "#54278f",
          "Output per worker (y)" = "#2c7fb8",
          "Consumption per worker (c)" = "#31a354"
        )) +
        labs(title = "Adjustment towards the steady state",
             subtitle = "Dashed lines show the steady-state levels implied by the current parameters",
             x = "Years since the starting point", y = "Per worker", colour = NULL) +
        my_theme
    })
  })
}

ui <- solow_ui("solow")
server <- function(input, output, session) solow_server("solow")

shinyApp(ui, server)
