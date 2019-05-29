#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(shinyalert)
library(viridis)
load('migration_data.Rdata')
# Define server logic required to draw a histogram


shinyServer(function(input, output) {
  my_mean = reactiveVal(0)
  my_wish = reactiveVal(0)
  observeEvent(TRUE,
               shinyalert(
                 title = "Information",
                 text = "Bienvenue sur l'application Uthink Migration.\nCette application vous permet de tester vos connaissances et de les confronter directement avec les données de l'OFS. Nous vous invitons à répondre aux deux questions suivantes sans chercher à trouver la \"bonne\" réponse. Il s'agit uniquement de votre intuition, il n'y a pas de bonne ou mauvaise réponse.",
                 closeOnEsc = FALSE,
                 closeOnClickOutside = FALSE,
                 html = FALSE,
                 type = "info",
                 showConfirmButton = TRUE,
                 showCancelButton = FALSE,
                 confirmButtonText = "OK",
                 confirmButtonCol = "#AEDEF4",
                 timer = 0,
                 imageUrl = "https://uthink.ch/logo.jpg",
                 imageWidth = 100,
                 imageHeight = 100,
                 animation = TRUE
               ))
  observeEvent(TRUE, {
      shinyalert(
    title = "Première Question",
    text = "Quel est, selon vous, la valeur du solde migratoire pour la Suisse entre 2002 et 2017?\n\nLe solde migratoire étant la différence entre l'immigration et l'émigration.\n\nNote: Les chiffres de la migration n'inclue pas les données des demandes d'asile.\n\ Pour vous aider, en 2017, la population Suisse se situait à 8.4 Mio et 1% est égal à 84\'000",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "input",
    inputType = "numeric",
    inputValue = "",
    inputPlaceholder = "En pourcentage de la population totale Suisse",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE, 
    callbackR = mean_callback
  )})
  observeEvent(TRUE, {
    shinyalert(
      title = "Deuxième Question",
      text = "A votre avis, quel devrait être le pourcentage du solde migratoire en Suisse pour les années à venir?",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "input",
      inputType = "numeric",
      inputValue = "",
      inputPlaceholder = "En pourcentage de la population totale Suisse",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE,
      callbackR = wish_callback
    )})
  mean_callback <- function(value) {
    my_mean(as.numeric(value))
  }
  wish_callback <- function(value) {
    my_wish(as.numeric(value))
  }
  url <- a("Lien github avec le code source et les données", href="https://github.com/uthink-git/Uthink_Migration")
  output$tab <- renderUI({
    tagList("", url)
  })
  
  output$migration_plot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    if (input$`Absolue ou Pourcentage` == 'pourcentage'){
      if(input$migrationType == 'Immigration'){
        base_data = immigration_long_pct
      } else if(input$migrationType == 'Emigration'){
        base_data = emigration_long_pct
      } else{
        base_data = solde_migration_long_pct
      }  
    }else{
      if(input$migrationType == 'Immigration'){
        base_data = immigration_long
      } else if(input$migrationType == 'Emigration'){
        base_data = emigration_long
      } else{
        base_data = solde_migration_long
      }
    }
    plot_data = base_data %>% filter(annee >= input$annees[1] & annee <= input$annees[2]) %>% filter(origine %in% input$origine)
    gg = ggplot(plot_data,aes(x = annee, y=nombre, fill = origine))
    if(input$TypeGraphe == 'Surface'){
    gg = gg + geom_area()
    } else {
      gg = gg + geom_bar(stat = 'identity')
    }
    gg = gg + scale_fill_viridis(discrete=TRUE) + theme_minimal() 
    if(input$migrationType != 'Immigration' & input$migrationType != 'Emigration' & input$`Absolue ou Pourcentage` == 'pourcentage'){
     gg = gg + geom_hline(aes(yintercept= my_mean(), linetype = "Votre moyenne prédite entre 2002 et 2017"), colour= 'red')
     gg = gg + geom_hline(aes(yintercept= my_wish(), linetype = "Votre souhait de solde migratoire"), colour= 'blue')
     gg = gg + scale_linetype_manual(name = "limit", values = c(2, 2), 
                            guide = guide_legend(override.aes = list(color = c("red", "blue"))))
     
    } else if(input$migrationType != 'Immigration' & input$migrationType != 'Emigration' & input$`Absolue ou Pourcentage` == 'absolue') {
      
      my_mean_absolute = my_mean() * (sum(population_tot_suisse[which(years %in% input$annees)])/length(input$annees)) / 100
      my_wish_absolute = my_wish() * (sum(population_tot_suisse[which(years %in% input$annees)])/length(input$annees)) / 100
      gg = gg + geom_hline(aes(yintercept= my_mean_absolute, linetype = "Votre moyenne prédite entre 2002 et 2017"), colour= 'red')
      gg = gg + geom_hline(aes(yintercept= my_wish_absolute, linetype = "Votre souhait de solde migratoire"), colour= 'blue')
      gg = gg + scale_linetype_manual(name = "limit", values = c(2, 2), 
                                      guide = guide_legend(override.aes = list(color = c("red", "blue"))))
    }
    
    gg = gg + scale_x_continuous(breaks = years, name = 'Année')
    if (input$`Absolue ou Pourcentage` == 'pourcentage'){
      gg = gg + scale_y_continuous(name = 'Pourcentage')
    } else {
      gg = gg + scale_y_continuous(name = 'Nombre')
    }
    
    gg
    
  })

})
