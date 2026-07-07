## ---------------------------
##
## Script name: global.R
##
## Purpose of script: add global themes etc. to Macroviz R Shiny App
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



library(shiny)
library(ggplot2)
library(gridExtra)
library(bslib)

# we add the manifest.json to host the code via Posit Connect
rsconnect::writeManifest()


# ── Theme ─────────────────────────────────────────────────────────────────────

# we implement a theme to make the app look much better

app_theme <- bs_theme(
  version    = 5,
  bootswatch = "sandstone",      #sets theme colours
  base_font  = font_google("IBM Plex Sans"),
  code_font  = font_google("IBM Plex Mono")
)
