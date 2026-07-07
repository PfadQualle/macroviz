## ---------------------------
##
## Script name: app.R
##
## Purpose of script: execute Macroviz R Shiny App in R environment
##
## Date Created: 2026-07-07
##
## Licensed under the MIT License. See LICENSE file in the project root for details.
## 
## ---------------------------
##
## Notes:  This script requires that the modules "global.R", "ui.R", and "server.R" are loaded.
##        
##
## ---------------------------

# ── GLOBAL SETTINGS ────────────────────────────────────────────────────────────────────────
source("global.R")

# ── UI ────────────────────────────────────────────────────────────────────────
source("ui.R")

# ── SERVER ────────────────────────────────────────────────────────────────────
source("server.R")

# ── RUN ───────────────────────────────────────────────────────────────────────
shinyApp(ui, server)

# ── RUN in Showcase mode ──────────────────────────────────────────────────────
#shiny::runApp(display.mode="showcase")
