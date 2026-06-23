# SurvControl

**An interactive control panel for survival analysis on real oncology data.**

🔗 **[Live demo](https://ggrandemagne.shinyapps.io/SurvControl2/)** &nbsp;·&nbsp; R / Shiny

SurvControl is a dashboard that lets users explore time-to-event (survival) data without writing code. It fits and compares several survival models, visualizes how covariates affect survival, and tests whether censoring is *dependent* on the event of interest, a case where standard models can mislead.

Built as my undergraduate thesis in Statistics at UFRGS (advisor: Profa. Dra. Silvana Schneider, 2023).

## What it does

- **Explore covariate effects** (age, sex, education, clinical stage, etc.) on survival across groups or individuals.
- **Fit and compare models:** Kaplan-Meier estimator, Cox regression, frailty models, and dependent-censoring models (Weibull and piecewise-exponential marginals, via the [DepCens](https://github.com/GabrielGrandemagne/DepCens) package).
- **Diagnose dependent censoring** — detect when failure time and censoring time are associated, and quantify why ignoring it biases results.
- **Visualize** survival and cumulative-hazard curves interactively.

## Data

Applied to anonymized hospital cancer-registry data (RHC) from the **Fundação Oncocentro de São Paulo (FOSP, 2022)**, with a comparative case study of **breast (C50)** and **ovarian (C56)** cancer patients. The analysis found a meaningful association between failure and censoring times — confirming the value of dependent-censoring models for this data.

## Getting Started

**Requirements:** R ≥ 4.0

```r
# 1. Install all dependencies (run once)
source("install.R")

# 2. Pre-compute the DepCens models (run once — takes a few minutes)
source("precompute.R")

# 3. Launch the app
shiny::runApp("app.R")
```

> Steps 1 and 2 only need to be run once after cloning. After that, only step 3 is needed.

## Tech

R · Shiny · survival · [DepCens](https://github.com/GabrielGrandemagne/DepCens)