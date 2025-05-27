
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{mlstartr}`

<!-- badges: start -->
<!-- badges: end -->

## Road map

### Objectif

Créer une application Shiny pour initier les utilisateurs au machine
learning via une interface interactive.

### Fonctionnalités principales

#### Tab 1 : Présentation de l’application

- [ ] Texte de présentation
- [ ] Illustrations
- [ ] Définitions liées au ML

#### Tab 2 : Choix du dataset

- [ ] Choix du dataset parmi une sélection (e.g. mtcars, iris, penguins)
- [ ] Sélection variable cible et prédicteurs

#### Tab 3 : Exploration des données

- [ ] Statistiques descriptives
- [ ] Visualisations

#### Tab 4 : Prétraitement

- [ ] Transformations (normalisation, factorisation)
- [ ] Séparation train/test

#### Tab 5 : Modélisation

- [ ] Choix du modèle (RF, etc.)
- [ ] Entraînement

#### Tab 6 : Évaluation et interprétation

- [ ] Prédiction sur test set
- [ ] Mesures de performance
- [ ] Interprétation

#### Tab 7 : Export

- [ ] Génération du code R correspondant

## Notes

- Verrouillage des onglets tant que l’étape précédente n’est pas validée

- Utilisation de `{tidymodels}`

## Installation

You can install the development version of `{mlstartr}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
mlstartr::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-05-27 16:55:08 BST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading mlstartr
#> 
#> Attaching package: 'shinydashboard'
#> 
#> 
#> The following object is masked from 'package:graphics':
#> 
#>     box
#> ── R CMD check results ──────────────────────────────── mlstartr 0.0.0.9000 ────
#> Duration: 4.3s
#> 
#> ❯ checking whether package ‘mlstartr’ can be installed ... ERROR
#>   See below...
#> 
#> ── Install failure ─────────────────────────────────────────────────────────────
#> 
#> * installing *source* package ‘mlstartr’ ...
#> ** this is package ‘mlstartr’ version ‘0.0.0.9000’
#> ** using staged installation
#> ** R
#> ** inst
#> ** byte-compile and prepare package for lazy loading
#> Error in library(shinydashboard) : 
#>   there is no package called ‘shinydashboard’
#> Error: unable to load R code in package ‘mlstartr’
#> Execution halted
#> ERROR: lazy loading failed for package ‘mlstartr’
#> * removing ‘/private/var/folders/df/r183kjzj43q4wbz7v4_s9hsm0000gn/T/RtmpZXtrgI/file7f37578ed973/mlstartr.Rcheck/mlstartr’
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error: Failure in `/private/var/folders/df/r183kjzj43q4wbz7v4_s9hsm0000gn/T/RtmpZXtrgI/R_LIBS7f37542a78bc/mlstartr/mlstartr-tests/testthat.Rout.fail`
#> `dashboardPage(dashboardHeader(title = "MLstartr"), dashboardSidebar(sidebarMenu(id = "tabs", 
#>     menuItem("Introduction", tabName = "intro"))), dashboardBody(tabItems(tabItem(tabName = "intro", 
#>     mod_intro_ui("intro_1")))))`: could not find function "dashboardPage"
#> Backtrace:
#>     ▆
#>  1. └─mlstartr:::app_ui() at test-golem-recommended.R:2:3
#>  2.   └─htmltools::tagList(...)
#>  3.     └─rlang::dots_list(...) at htmltools/R/tags.R:275:3
#> 
#> [ FAIL 1 | WARN 0 | SKIP 1 | PASS 88 ]
#> Error: Test failures
#> Execution halted
```
