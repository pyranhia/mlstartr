
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

- [x] Choix du dataset parmi une sélection (e.g. mtcars, iris, penguins)
- [ ] Pré-traiter les datasets pour avoir les bons types de variables
- [x] Sélection variable cible et prédicteurs
- [ ] Retirer la variable cible de la liste des prédicteurs possibles
- [ ] Bouton de validation de la configuration et déverouillage de
  l’onglet suivant

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

## Checklist de tests pour l’application

| Test                        | Fréquence        | Commande clé           |
|-----------------------------|------------------|------------------------|
| Vérification du package     | Régulièrement    | `devtools::check()`    |
| Lancement de l’app          | En continu       | `golem::run_dev()`     |
| Tests unitaires             | À chaque feature | `devtools::test()`     |
| Build et install du package | Avant release    | `devtools::build()`    |
| Docker / déploiement        | Avant push prod  | `docker build` / `run` |
| Tests UI / navigateurs      | Manuellement     | Test visuel            |

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
#> [1] "2025-06-09 10:17:30 BST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading mlstartr
#> ── R CMD check results ──────────────────────────────── mlstartr 0.0.0.9000 ────
#> Duration: 33.9s
#> 
#> ❯ checking code files for non-ASCII characters ... WARNING
#>   Found the following files with non-ASCII characters:
#>     R/app_ui.R
#>     R/mod_dataset.R
#>     R/mod_intro.R
#>     R/mod_validate_conf.R
#>     R/mod_variables.R
#>   Portable packages must use only ASCII characters in their R code and
#>   NAMESPACE directives, except perhaps in comments.
#>   Use \uxxxx escapes for other characters.
#>   Function ‘tools::showNonASCIIfile’ can help in finding non-ASCII
#>   characters in files.
#> 
#> ❯ checking dependencies in R code ... WARNING
#>   '::' or ':::' import not declared from: ‘shinyjs’
#> 
#> 0 errors ✔ | 2 warnings ✖ | 0 notes ✔
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> mlstartr Coverage: 79.08%
#> R/run_app.R: 0.00%
#> R/mod_variables.R: 47.76%
#> R/mod_validate_conf.R: 53.12%
#> R/mod_dataset.R: 79.31%
#> R/app_config.R: 100.00%
#> R/app_server.R: 100.00%
#> R/app_ui.R: 100.00%
#> R/golem_utils_server.R: 100.00%
#> R/golem_utils_ui.R: 100.00%
#> R/mod_intro.R: 100.00%
```
