#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("C:/Users/Gabriel-PC/Desktop/progamation/Bolsa Silvana/PacoteAntigo_DepCens/DepCens-master/R/biblio_function_EMMC_Weibull.R")
source("C:/Users/Gabriel-PC/Desktop/progamation/Bolsa Silvana/PacoteAntigo_DepCens/DepCens-master/R/dependent_censoring.R")
source("C:/Users/Gabriel-PC/Desktop/progamation/Bolsa Silvana/PacoteAntigo_DepCens/DepCens-master/R/plot_dc.R")
source("C:/Users/Gabriel-PC/Desktop/progamation/Bolsa Silvana/PacoteAntigo_DepCens/DepCens-master/R/summary_dc.R")
source("C:/Users/Gabriel-PC/Desktop/progamation/Bolsa Silvana/PacoteAntigo_DepCens/DepCens-master/R/survivalplots.R")


library(Formula)
library(survival)
library(rootSolve)
library(dlm)
library(matrixStats)
library(stats)
library(graphics)

### === ###

library(shiny)
library(shinydashboard)
library(tidyverse)
library(forcats)
library(survival)
library(survminer)
library(dplyr)
library(broom)
library(DT)

# Banco de dados Pós-tratamento
load("C:/Users/Gabriel-PC/Desktop/TCC_GERAL/Projeto_R_tcc/df2.rda")

# Definindo o nome dos bancos a serem utilizados no aplicativo (utilizar REACT posteriormente)
df <- df2 %>%
  mutate(TOPOGRUP = as.factor(TOPOGRUP))

"C56 e C50" <- df5650 <- df %>%
  filter(TOPOGRUP %in% c("C50", "C56"))

C61 <- df61 <- df %>%
  filter(TOPOGRUP %in% "C61")

C56 <- df56 <- df %>%
  filter(TOPOGRUP %in% "C56")

C50 <- df50 <- df %>%
  filter(TOPOGRUP %in% "C50")

All <- df

# Modelos COX a serem utilizados no app

# All
cox1 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+SEXO+TRATAMENTO, data=df)
coxtable1 <- tidy(cox1)

# 2
cox2 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+SEXO+TRATAMENTO, data=df5650)
coxtable2 <- tidy(cox2)

# 50
cox3 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+SEXO+TRATAMENTO, data=C50)
coxtable3 <- tidy(cox3)

# 56
cox4 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+TRATAMENTO, data=df56)
coxtable4 <- tidy(cox4)

# 61
cox5 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+TRATAMENTO, data=df61)
coxtable5 <- tidy(cox5)

#   ___  ______ _     _____ _____   ___ _____ _____ _   _  _____ 
#  / _ \ | ___ \ |   |_   _/  __ \ / _ \_   _|_   _| | | ||  _  |
# / /_\ \| |_/ / |     | | | /  \// /_\ \| |   | | | | | || | | |
# |  _  ||  __/| |     | | | |    |  _  || |   | | | | | || | | |
# | | | || |   | |_____| |_| \__/\| | | || |  _| |_\ \_/ /\ \_/ /
# \_| |_/\_|   \_____/\___/ \____/\_| |_/\_/  \___/ \___/  \___/ 


df <- as.data.frame(df)

#sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df)

cidlist <- c(levels(df$TOPOGRUP), "C56 e C50", "All")

covarlist <- c("SEXO", "ESTAGIO", "CATEATEND", "ESCOLARI")

depcenslist <- c("Weibull", "MEP")

# APP
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "SurvControl"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Kaplan Meyer", tabName = "coxph", icon = icon("dashboard")),
      menuItem("Modelo de Cox", tabName = "extra", icon = icon("th")),
      menuItem("Depcens", tabName = "depcens", icon = icon("th")),
      selectInput(
        "cid",
        label = "Ecolha os CIDs",
        choices = cidlist
      )
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "coxph",
              fluidRow(
                box(
                  title = "Curva de Sobrevivência (Kaplan-Meier)", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("graficosurv")
                ),
                box(
                  title = "Função de Risco Acumulado (Kaplan-Meier)", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("graficohazard"),
                ),
                box(
                  selectInput(
                    "caixa",
                    label = "Escolha a variável a ser utilizada no Kapan-Meier",
                    choices = covarlist
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
      ),
      tabItem(tabName = "depcens",
              fluidRow(
                box(
                  title = "Output Depcens", width = 8, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  DTOutput("depcenstable")
                ),
                box( 
                  title = "Inputs Depcens", width = 4, status = "primary", solidHeader = TRUE,
                selectInput(
                  "dci",
                  label = "Weibull ou MEP",
                  choices = depcenslist
                ),
                selectInput(
                  "scen",
                  label = "T ou C",
                  choices = c("t", "c")
                )
                ),
                box(
                  title = "Plot Depcens", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("plotdep")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$depcenstable <- renderDT({
    
    if(input$dci == "MEP") {
    
    if(input$cid == "C56 e C50") {
      dat <- c5056novo_df
    }
    
    if(input$cid == "C50") {
      dat <- c50novo_df
    }
    
    if(input$cid == "C56") {
      dat <- c56novo_df
    }
      
    }
    
    if(input$dci == "Weibull") {
      
      if(input$cid == "C56 e C50") {
        dat <- c5056novo_dfWEIB
      }
      
      if(input$cid == "C50") {
        dat <- c50novo_dfWEIB
      }
      
      if(input$cid == "C56") {
        dat <- c56novo_dfWEIB
      }
      
    }
    
    dat <- format(dat, nsmall = 6)
    datatable(dat, rownames = TRUE)
  
  })
  
  output$plotdep <- renderPlot({ 
    
    if(input$dci == "MEP") {
      
      if(input$cid == "C56 e C50") {
        fit_escolhido <- c50c56novo_depcensMEP
      }
      
      if(input$cid == "C50") {
        fit_escolhido <- c50novo_depcens
      }
      
      if(input$cid == "C56") {
        fit_escolhido <- c56novo_depcens
      }
    }
    
    if(input$dci == "Weibull") {
      
      if(input$cid == "C56 e C50") {
        fit_escolhido <- c50c56novo_depcensWEIB
      }
      
      if(input$cid == "C50") {
        fit_escolhido <- c50novo_depcensWEIB
      }
      
      if(input$cid == "C56") {
        fit_escolhido <- c56novo_depcensWEIB
      }
    }
    
    if (input$scen == "t") {
      scenesc <- c("t")
    }
    
    if (input$scen == "c") {
      scenesc <- c("c")
    }
    
    plot_dc(fit_escolhido, scenario = scenesc)
    
    })
  
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
  output$coxplot <- renderPlot({
    
  if(input$cid == "All") {
    output$coxstable <- renderTable(coxtable1)
    
#      cox1 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+SEXO, data=df)
      cox_test <- cox.zph(cox1)
  }
  
  if(input$cid == "C56 e C50") {
    output$coxstable <- renderTable(coxtable2)
    
    cox_test <- cox.zph(cox2)
      

  }
  
  if(input$cid == "C50") {
    output$coxstable <- renderTable(coxtable3)
    
      cox_test <- cox.zph(cox3)
      

  }
  
  if(input$cid == "C56") {
    output$coxstable <- renderTable(coxtable4)
    
    cox_test <- cox.zph(cox4)
      

  }
  
  if(input$cid == "C61") {
    output$coxstable <- renderTable(coxtable5)
    
    cox_test <- cox.zph(cox5)

  }
    
    ggcoxzph(cox_test)
    
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
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df61)
        
      }
      
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
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df56)
        
      }
      
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
    
    else if(input$cid == "C56 e C50") {
      df5650 <- df %>%
        filter(TOPOGRUP %in% c("C50", "C56"))
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df5650)
        
      }
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df5650)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df5650)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df5650)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw()
      )
      
    }
    
    else if(input$cid == "C50") {
      
      df50 <- df %>%
        filter(TOPOGRUP %in% "C50")
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df50)
        
      }
      
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
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df)
        
      }
      
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
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df61)
        
      }
      
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
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df56)
        
      }
      
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
    
    else if(input$cid == "C56 e C50") {
      df5650 <- df %>%
        filter(TOPOGRUP %in% c("C50", "C56"))
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df5650)
        
      }
      
      if(input$caixa == "ESTAGIO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESTAGIO, data=df5650)
        
      }
      
      if(input$caixa == "SEXO") {
        
        sfit <- survfit(Surv(tempo, delta_t)~SEXO, data=df5650)
        
      }
      
      if(input$caixa == "CATEATEND") {
        
        sfit <- survfit(Surv(tempo, delta_t)~CATEATEND, data=df5650)
        
      }
      
      ggsurvplot(sfit, conf.int=TRUE, 
                 ggtheme = theme_bw(),
                 risk.table.col = "strata",
                 fun = "cumhaz")
      
    }
    
    else if(input$cid == "C50") {
      
      df50 <- df %>%
        filter(TOPOGRUP %in% "C50")
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df50)
        
      }
      
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
      
      if(input$caixa == "ESCOLARI") {
        
        sfit <- survfit(Surv(tempo, delta_t)~ESCOLARI, data=df)
        
      }
      
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
