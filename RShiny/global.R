# Introduction to Shiny
# Creating your first Shiny App
# 11/14/2019

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(plotly)

#You can do things globally in Shiny. These will be set once as the app is opened.
kg <- read.table("kg_ex.txt", header = TRUE, stringsAsFactors = FALSE)

x.low <- min(kg$PC1)
x.high <- max(kg$PC1)
y.low <- min(kg$PC2)
y.high <- max(kg$PC2)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n <- length(unique(kg$Population))
cols = gg_color_hue(n)
