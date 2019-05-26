#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinyalert)
load('migration_data.Rdata')
continent_choices = as.list(levels(as.factor(solde_migration_long$origine)))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyalert(),
  
  # Application title
  titlePanel(paste0("Données de migration pour la suisse par origine géographique de la nationalité entre ", min(years)," à ", max(years))),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("annees", label = h3("Selection des années"), min = min(years), 
                    max = max(years), value = c(min(years), max(years)), step = 1, sep = ""),
        checkboxGroupInput("origine", 
                         label = h3("Continent d'origine"), 
                         choices = list("Afrique" = 'Afrique', 
                                        "Amérique latine" = 'AmeriqueLatine', 
                                        "Amérique du Nord" = 'AmeriqueNord',
                                        "Asie" = 'Asie',
                                        "Europe"="Europe",
                                        "Océanie"="Oceanie",
                                        "Suisse"="Suisse"),
                         selected = c('Afrique','AmeriqueLatine','AmeriqueNord','Asie','Europe','Oceanie','Suisse')),
        selectInput('migrationType',
                    label = 'Type de migration',
                    choices = c('Solde migratoire','Immigration', 'Emigration'),
                    selected = 'Solde migratoire'),
      selectInput("Absolue ou Pourcentage", "Données en:", 
                  choices=list('En valeur absolue'='absolue', 'En pourcentage de la population Suisse'='pourcentage'),
                  selected = 'pourcentage'),
      selectInput('TypeGraphe',
                   label = 'Type de graphique',
                   choices = c('Surface','Bar plot'),
                   selected = 'Surface')
    ),
    
    
    mainPanel(
       plotOutput("migration_plot",height = '900px')
    )
  )
))
