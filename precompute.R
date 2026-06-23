# Run this script once to fit the DepCens models and save them to depcens_models.rda.
# After running, commit depcens_models.rda so that app.R works for everyone who clones the repo.
# WARNING: fitting these models can take a long time.

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
library(dplyr)

load("df2.rda")

# Prepare subsets with factor re-coding used in the thesis models
df3 <- df2 %>%
  mutate(
    CATEATEND = as.factor(CATEATEND),
    SEXO      = as.factor(SEXO),
    ESCOLARI  = as.factor(ESCOLARI)
  )
levels(df3$CATEATEND) <- c("0", "1", "0")  # collapse Particular/Convênio → 0, SUS → 1
levels(df3$SEXO)      <- c("0", "1")

df56_ <- df3 %>%
  filter(TOPOGRUP %in% "C56") %>%
  mutate(ESTAGIOt = as.factor(ESTAGIO_num))
levels(df56_$ESTAGIOt) <- c("0", "1")

df50_ <- df3 %>%
  filter(TOPOGRUP %in% "C50")

# Sample cap — keeps fitting fast; raise if models fail to converge
SAMPLE_N <- 5000
set.seed(291440)
df56_ <- df56_ %>% slice_sample(n = min(nrow(df56_), SAMPLE_N))
df50_ <- df50_ %>% slice_sample(n = min(nrow(df50_), SAMPLE_N))

# --- Fit models ---

message("(1/4) Fitting C56 MEP model...")
delta_t_56 <- ifelse(df56_$ULTINFO == 3, 1, 0)
delta_c_56 <- ifelse(df56_$ULTINFO == 4, 1, 0)
c56novo_depcens <- dependent.censoring(
  formula = tempo ~ IDADE + ESTAGIOt + CATEATEND + ESCOLARI | IDADE + ESTAGIOt + CATEATEND + ESCOLARI,
  data = df56_, delta_t = delta_t_56, delta_c = delta_c_56,
  ident = df56_$CLINICA, dist = "mep"
)

message("(2/4) Fitting C56 Weibull model...")
c56novo_depcensWEIB <- dependent.censoring(
  formula = tempo ~ IDADE + ESTAGIOt + CATEATEND + ESCOLARI | IDADE + ESTAGIOt + CATEATEND + ESCOLARI,
  data = df56_, delta_t = delta_t_56, delta_c = delta_c_56,
  ident = df56_$CLINICA, dist = "weibull"
)

message("(3/4) Fitting C50 MEP model...")
delta_t_50 <- ifelse(df50_$ULTINFO == 3, 1, 0)
delta_c_50 <- ifelse(df50_$ULTINFO == 4, 1, 0)
c50novo_depcens <- dependent.censoring(
  formula = tempo ~ IDADE + ESTAGIO + CATEATEND + ESCOLARI + SEXO | IDADE + ESTAGIO + CATEATEND + ESCOLARI + SEXO,
  data = df50_, delta_t = delta_t_50, delta_c = delta_c_50,
  ident = df50_$CLINICA, dist = "mep"
)

message("(4/4) Fitting C50 Weibull model...")
c50novo_depcensWEIB <- dependent.censoring(
  formula = tempo ~ IDADE + ESTAGIO + CATEATEND + ESCOLARI + SEXO | IDADE + ESTAGIO + CATEATEND + ESCOLARI + SEXO,
  data = df50_, delta_t = delta_t_50, delta_c = delta_c_50,
  ident = df50_$CLINICA, dist = "weibull"
)

# --- Build summary dataframes ---

fit_to_df_mep <- function(object) {
  bmax <- object$bmax; p <- object$p; q <- object$q; pv <- object$pvalue
  rbind(
    data.frame(
      ESTIMATE = c(object[[1]][p+bmax+q+1],       object[[1]][p+bmax+q+bmax+2]),
      Stde     = c(object[[2]][p+bmax+q+1],       object[[2]][p+bmax+q+bmax+2]),
      CINF     = c(object[[1]][p+bmax+q+1]      - 1.96*object[[2]][p+bmax+q+1],
                   pmax(0, object[[1]][p+bmax+q+bmax+2] - 1.96*object[[2]][p+bmax+q+bmax+2])),
      CSUP     = c(object[[1]][p+bmax+q+1]      + 1.96*object[[2]][p+bmax+q+1],
                   object[[1]][p+bmax+q+bmax+2] + 1.96*object[[2]][p+bmax+q+bmax+2]),
      PVALUE   = c(pv[1], NA)
    ),
    data.frame(
      ESTIMATE = object[[1]][1:p],
      Stde     = object[[2]][1:p],
      CINF     = object[[1]][1:p] - 1.96*object[[2]][1:p],
      CSUP     = object[[1]][1:p] + 1.96*object[[2]][1:p],
      PVALUE   = pv[2:(p+1)]
    ),
    data.frame(
      ESTIMATE = object[[1]][p+bmax+(1:q)],
      Stde     = object[[2]][p+bmax+(1:q)],
      CINF     = object[[1]][p+bmax+(1:q)] - 1.96*object[[2]][p+bmax+(1:q)],
      CSUP     = object[[1]][p+bmax+(1:q)] + 1.96*object[[2]][p+bmax+(1:q)],
      PVALUE   = pv[1+p+(1:q)]
    )
  )
}

fit_to_df_weib <- function(object) {
  p <- object$p; q <- object$q; pv <- object$pvalue
  rbind(
    data.frame(
      ESTIMATE = c(object[[1]][5+p+q],  object[[1]][6+p+q]),
      Stde     = c(object[[2]][5+p+q],  object[[2]][6+p+q]),
      CINF     = c(object[[1]][5+p+q] - 1.96*object[[2]][5+p+q],
                   pmax(0, object[[1]][6+p+q] - 1.96*object[[2]][6+p+q])),
      CSUP     = c(object[[1]][5+p+q] + 1.96*object[[2]][5+p+q],
                   object[[1]][6+p+q] + 1.96*object[[2]][6+p+q]),
      PVALUE   = c(pv[1], NA)
    ),
    data.frame(
      ESTIMATE = object[[1]][3:(p+2)],
      Stde     = object[[2]][3:(p+2)],
      CINF     = object[[1]][3:(p+2)] - 1.96*object[[2]][3:(p+2)],
      CSUP     = object[[1]][3:(p+2)] + 1.96*object[[2]][3:(p+2)],
      PVALUE   = pv[2:(p+1)]
    ),
    data.frame(
      ESTIMATE = object[[1]][4+p+(1:q)],
      Stde     = object[[2]][4+p+(1:q)],
      CINF     = object[[1]][4+p+(1:q)] - 1.96*object[[2]][4+p+(1:q)],
      CSUP     = object[[1]][4+p+(1:q)] + 1.96*object[[2]][4+p+(1:q)],
      PVALUE   = pv[1+p+(1:q)]
    )
  )
}

c56novo_df     <- fit_to_df_mep(c56novo_depcens)
c56novo_dfWEIB <- fit_to_df_weib(c56novo_depcensWEIB)
c50novo_df     <- fit_to_df_mep(c50novo_depcens)
c50novo_dfWEIB <- fit_to_df_weib(c50novo_depcensWEIB)

save(
  c56novo_depcens,     c56novo_depcensWEIB,
  c50novo_depcens,     c50novo_depcensWEIB,
  c56novo_df,          c56novo_dfWEIB,
  c50novo_df,          c50novo_dfWEIB,
  file = "depcens_models.rda"
)

message("Done. Commit depcens_models.rda to the repo.")

local({
  e <- new.env()
  load("depcens_models.rda", envir = e)
  cat("Objects saved:\n")
  print(ls(e))
  cat("\nC50 MEP table (first 3 rows):\n")
  print(head(e$c50novo_df, 3))
  cat("\nC56 Weibull table (first 3 rows):\n")
  print(head(e$c56novo_dfWEIB, 3))
})
