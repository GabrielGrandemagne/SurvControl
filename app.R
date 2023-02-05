#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(forcats)
library(survival)
library(survminer)
library(dplyr)
library(broom)

# Banco de dados Pós-tratamento
load("C:/Users/Gabriel-PC/Desktop/TCC_GERAL/df.rda")


df <- df %>%
  mutate(TOPOGRUP = as.factor(TOPOGRUP))

cox1 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+SEXO, data=df)

coxtable <- tidy(cox1)


df61 <- df %>%
  filter(TOPOGRUP %in% "C61")

df56 <- df %>%
  filter(TOPOGRUP %in% "C56")

df50 <- df %>%
  filter(TOPOGRUP %in% "C50")

#   ___  ______ _     _____ _____   ___ _____ _____ _   _  _____ 
#  / _ \ | ___ \ |   |_   _/  __ \ / _ \_   _|_   _| | | ||  _  |
# / /_\ \| |_/ / |     | | | /  \// /_\ \| |   | | | | | || | | |
# |  _  ||  __/| |     | | | |    |  _  || |   | | | | | || | | |
# | | | || |   | |_____| |_| \__/\| | | || |  _| |_\ \_/ /\ \_/ /
# \_| |_/\_|   \_____/\___/ \____/\_| |_/\_/  \___/ \___/  \___/ 


df <- as.data.frame(df)

#sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df)

cidlist <- c(levels(df$TOPOGRUP), "All")

covarlist <- c("SEXO", "ESTAGIO", "CATEATEND")

# APP
ui <- dashboardPage(
  dashboardHeader(title = "SurvControl"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Kaplan Meyer", tabName = "coxph", icon = icon("dashboard")),
      menuItem("Modelo de Cox", tabName = "extra", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "coxph",
              fluidRow(
                box(
                  title = "Curva de Sobrevivência", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("graficosurv")
                ),
                box(
                  title = "Cumulative Hazard Plot", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("graficohazard"),
                ),
                box(
                  title = "Grupos", width=3, status = "primary", solidHeader = TRUE,
                  selectInput(
                    "caixa",
                    label = "Escolha a Variável",
                    choices = covarlist
                  ),
                  selectInput(
                    "cid",
                    label = "Ecolha os CIDs",
                    choices = cidlist
                  )
                )
              )
      ),
      tabItem(
        tabName = "extra",
        fluidRow(
          box(
            title = "Modelo de Cox", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("coxstable")
          ),
          box(
            title = "Teste de Schoenfeld", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("coxplot")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  #  filtered_data <- reactive({
  #    df[df$TOPOGRUP %in% input$filterCID, ]
  #  })
  #  
  #  filtered_fit <- reactive({
  #    survfit(Surv(filtered_data()$tempo, filtered_data()$delta_t) ~ filtered_data()[[input$covar]])
  #  })
  #  
  #  # Plot the survival curve using ggsurvplot
  #  output$survival_plot <- renderPlot({
  #    ggsurvplot(filtered_fit(), data = filtered_data(), conf.int = TRUE,
  #               ggtheme = theme_bw())
  #  })
  
  # Tabela cox
  output$coxstable <- renderTable(coxtable)
  
  output$coxplot <- renderPlot({
    cox1 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+SEXO, data=df)
    cox1_test <- cox.zph(cox1)
    
    ggcoxzph(cox1_test)
    
  })
  
  
  #  if(input$caixa == "ESTAGIO") {
  #
  #    sfit <- survfit(Surv(filtered_data()$tempo, filtered_data()$delta_t)~filtered_data()$ESTAGIO, data = filtered_data()[[input$caixa]])
  #  }
  #  
  #  if(input$caixa == "SEXO") {
  #    
  #      sfit <- survfit(Surv(filtered_data()$tempo, filtered_data()$delta_t)~filtered_data()$SEXO, data = filtered_data())
  #  }
  #  
  #  if(input$caixa == "CATEATEND") {
  #
  #      sfit <- survfit(Surv(filtered_data()$tempo, filtered_data()$delta_t)~filtered_data()$CATEATEND, data = filtered_data())
  #  }
  
  # covarlist <- c("SEXO", "ESTAGIO", "CATEATEND")
  
  
  output$graficosurv <- renderPlot({
    
    if(input$cid == "C61") {
      
      df61 <- df %>%
        filter(TOPOGRUP %in% "C61")
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df61)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df61)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df61)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw()
      )
      
    }
    
    else if(input$cid == "C56") {
      
      df56 <- df %>%
        filter(TOPOGRUP %in% "C56")
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df56)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df56)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df56)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw()
      )
      
    }
    
    else if(input$cid == "C50") {
      
      df50 <- df %>%
        filter(TOPOGRUP %in% "C50")
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df50)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df50)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df50)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw()
      )
      
    } else {
      
      df <- df
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw()
      )
      
    }
    
  })
  
  output$graficohazard <- renderPlot({
    
    if(input$cid == "C61") {
      
      df61 <- df %>%
        filter(TOPOGRUP %in% "C61")
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df61)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df61)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df61)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw(),
                 risk.table.col = "strata",
                 fun = "cumhaz")
      
    }
    
    else if(input$cid == "C56") {
      
      df56 <- df %>%
        filter(TOPOGRUP %in% "C56")
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df56)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df56)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df56)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw(),
                 risk.table.col = "strata",
                 fun = "cumhaz")
      
    }
    
    else if(input$cid == "C50") {
      
      df50 <- df %>%
        filter(TOPOGRUP %in% "C50")
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df50)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df50)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df50)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw(),
                 risk.table.col = "strata",
                 fun = "cumhaz")
      
    } else {
      
      df <- df
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw(),
                 risk.table.col = "strata",
                 fun = "cumhaz")
      
    }
    
  })
  
}

shinyApp(ui, server)
