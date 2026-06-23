if (!requireNamespace("DepCens", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("GabrielGrandemagne/DepCens")
}

library(DepCens)
library(Formula)
library(survival)
library(rootSolve)
library(dlm)
library(matrixStats)
library(stats)
library(graphics)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(forcats)
library(survminer)
library(dplyr)
library(broom)
library(DT)

load("df2.rda")

df <- df2 %>%
  mutate(TOPOGRUP = as.factor(TOPOGRUP))

df5650 <- df %>% filter(TOPOGRUP %in% c("C50", "C56"))
df61   <- df %>% filter(TOPOGRUP %in% "C61")
df56   <- df %>% filter(TOPOGRUP %in% "C56")
df50   <- df %>% filter(TOPOGRUP %in% "C50")

cox1 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+SEXO+TRATAMENTO, data=df)
coxtable1 <- tidy(cox1)

cox2 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+SEXO+TRATAMENTO, data=df5650)
coxtable2 <- tidy(cox2)

cox3 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+SEXO+TRATAMENTO, data=df50)
coxtable3 <- tidy(cox3)

cox4 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+TRATAMENTO, data=df56)
coxtable4 <- tidy(cox4)

cox5 <- coxph(Surv(tempo, delta_t)~CATEATEND+ESTAGIO+IDADE+TRATAMENTO, data=df61)
coxtable5 <- tidy(cox5)

# Load pre-computed DepCens models (run precompute.R once to generate this file)
depcens_loaded <- file.exists("depcens_models.rda")
if (depcens_loaded) load("depcens_models.rda")

df <- as.data.frame(df)

cidlist    <- c(levels(df$TOPOGRUP), "C56 e C50", "All")
covarlist  <- c("SEXO", "ESTAGIO", "CATEATEND", "ESCOLARI")
depcenslist <- c("Weibull", "MEP")
depcens_cids <- c("C50", "C56")

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "SurvControl"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Kaplan Meyer", tabName = "coxph",    icon = icon("dashboard")),
      menuItem("Modelo de Cox", tabName = "extra",   icon = icon("th")),
      menuItem("Depcens",       tabName = "depcens", icon = icon("th")),
      selectInput("cid", label = "Ecolha os CIDs", choices = cidlist)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "coxph",
        fluidRow(
          box(title = "Curva de Sobrevivência (Kaplan-Meier)", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, plotOutput("graficosurv")),
          box(title = "Função de Risco Acumulado (Kaplan-Meier)", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, plotOutput("graficohazard")),
          box(selectInput("caixa",
                label = "Escolha a variável a ser utilizada no Kapan-Meier",
                choices = covarlist))
        )
      ),
      tabItem(tabName = "extra",
        fluidRow(
          box(title = "Modelo de Cox", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, tableOutput("coxstable")),
          box(title = "Teste de Schoenfeld", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, uiOutput("coxplot_ui"))
        )
      ),
      tabItem(tabName = "depcens",
        fluidRow(
          box(title = "Output Depcens", width = 8, status = "primary",
              solidHeader = TRUE, collapsible = TRUE, DTOutput("depcenstable")),
          box(title = "Inputs Depcens", width = 4, status = "primary", solidHeader = TRUE,
            selectInput("dci",  label = "Weibull ou MEP", choices = depcenslist),
            selectInput("scen", label = "T ou C",         choices = c("t", "c"))
          ),
          box(title = "Plot Depcens", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, plotOutput("plotdep"))
        )
      )
    )
  )
)

server <- function(input, output) {

  output$depcenstable <- renderDT({
    if (!depcens_loaded)
      return(datatable(data.frame(Info = "Run precompute.R first to generate depcens_models.rda"), rownames = FALSE))
    if (!input$cid %in% depcens_cids)
      return(datatable(data.frame(Info = paste("DepCens only available for:", paste(depcens_cids, collapse = ", "))), rownames = FALSE))

    dat <- if (input$dci == "MEP") {
      if (input$cid == "C50") c50novo_df else c56novo_df
    } else {
      if (input$cid == "C50") c50novo_dfWEIB else c56novo_dfWEIB
    }
    datatable(format(dat, nsmall = 6), rownames = TRUE)
  })

  output$plotdep <- renderPlot({
    if (!depcens_loaded || !input$cid %in% depcens_cids) return(NULL)

    fit_escolhido <- if (input$dci == "MEP") {
      if (input$cid == "C50") c50novo_depcens else c56novo_depcens
    } else {
      if (input$cid == "C50") c50novo_depcensWEIB else c56novo_depcensWEIB
    }
    plot_dc(fit_escolhido, scenario = input$scen)
  })

  cox_test_r <- reactive({
    switch(input$cid,
      "All"        = cox.zph(cox1),
      "C56 e C50"  = cox.zph(cox2),
      "C50"        = cox.zph(cox3),
      "C56"        = cox.zph(cox4),
      "C61"        = cox.zph(cox5)
    )
  })

  output$coxstable <- renderTable({
    switch(input$cid,
      "All"        = coxtable1,
      "C56 e C50"  = coxtable2,
      "C50"        = coxtable3,
      "C56"        = coxtable4,
      "C61"        = coxtable5
    )
  })

  output$coxplot_ui <- renderUI({
    n_panels <- nrow(cox_test_r()$table) - 1  # subtract GLOBAL row
    plotOutput("coxplot", height = paste0(max(300, n_panels * 200), "px"))
  })

  output$coxplot <- renderPlot({
    ggcoxzph(cox_test_r())
  })

  get_data <- reactive({
    switch(input$cid,
      "C61"      = df %>% filter(TOPOGRUP %in% "C61"),
      "C56"      = df %>% filter(TOPOGRUP %in% "C56"),
      "C56 e C50"= df %>% filter(TOPOGRUP %in% c("C50", "C56")),
      "C50"      = df %>% filter(TOPOGRUP %in% "C50"),
      df
    )
  })

  make_sfit <- function(data, covar) {
    f <- as.formula(paste("Surv(tempo, delta_t) ~", covar))
    do.call(survfit, list(formula = f, data = data))
  }

  output$graficosurv <- renderPlot({
    dat <- get_data()
    sfit <- make_sfit(dat, input$caixa)
    ggsurvplot(sfit, data = dat, conf.int = TRUE, ggtheme = theme_bw())
  })

  output$graficohazard <- renderPlot({
    dat <- get_data()
    sfit <- make_sfit(dat, input$caixa)
    ggsurvplot(sfit, data = dat, conf.int = TRUE, ggtheme = theme_bw(),
               risk.table.col = "strata", fun = "cumhaz")
  })
}

shinyApp(ui, server)
