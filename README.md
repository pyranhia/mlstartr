
# `{mlstartr}`

<!-- badges: start -->

<!-- badges: end -->

Une application Shiny pédagogique pour s’initier au machine learning
supervisé, pas à pas. Développée par [Pyranhia](https://pyranhia.eu).

## Présentation

`{mlstartr}` guide l’utilisateur à travers un pipeline complet de
machine learning supervisé :

1.  **Données** — choix du jeu de données et sélection des variables
2.  **Exploration** — visualisation et statistiques descriptives
3.  **Prétraitement** — séparation train/test et transformations
4.  **Modélisation** — entraînement d’un Random Forest
5.  **Évaluation** — métriques de performance sur le jeu de test
6.  **Export** — récupération du code R généré

Trois jeux de données sont disponibles : - **California Housing**
(régression) - **Titanic** (classification binaire) - **Penguins**
(classification multiclasse)

## Stack technique

- [`{golem}`](https://thinkr-open.github.io/golem/) — architecture du
  package
- [`{bslib}`](https://rstudio.github.io/bslib/) — interface utilisateur
- [`{tidymodels}`](https://www.tidymodels.org/) — pipeline ML
- [`{datapyranhia}`](https://github.com/pyranhia/datapyranhia) — jeux de
  données

## Installation

``` r
remotes::install_github("pyranhia/mlstartr")
```

## Lancement

``` r
mlstartr::run_app()
```

## Licence

Copyright (c) 2026 Thelma Panaïotis — [Pyranhia](https://pyranhia.eu)
