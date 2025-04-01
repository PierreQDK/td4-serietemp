# Analyse des séries temporelles univariées – Non-stationnarité, racines unitaires et PIB des États-Unis (TD4 – M1 ECAP)

## Présentation

Ce projet s’inscrit dans le cadre du TD4 de séries temporelles univariées du Master 1 ECAP (2024-2025) à l’IAE de Nantes. Il a pour objectif d’explorer les notions clés de non-stationnarité, de chocs transitoires ou permanents, de régressions fallacieuses et de racines unitaires via des simulations et analyses en R.

Nous avons mis en œuvre des approches graphiques, empiriques et économétriques afin d’illustrer les comportements distincts entre les processus stationnaires et non stationnaires, puis appliqué ces concepts à l’étude du PIB américain entre 1990 et 2023.

Les principaux axes étudiés sont :

- L’impact de chocs dans un AR(1) selon différentes valeurs de φ (0.5, 0.9, 1),
- La comparaison de processus **Trend Stationary (TS)** vs **Difference Stationary (DS)**,
- La mise en évidence des **régressions fallacieuses** entre séries non stationnaires,
- La simulation de la **distribution du test de Dickey-Fuller** via une méthode de Monte Carlo,
- Une **analyse complète du PIB des États-Unis**, avec ACF, PACF, tests ADF, KPSS et modélisation.

## Contenu du projet

**Simulation de chocs AR(1) :** Étude des effets transitoires et permanents selon φ.

**Comparaison TS vs DS :** Simulation graphique selon plusieurs lois normales (N(0, 1/4), N(0, 1/2), N(0, 1)).

**Régressions fallacieuses :** Analyse du taux de rejets erronés de H0 en régressant deux marches aléatoires.

**Distribution de Dickey-Fuller :** Estimation empirique des seuils critiques avec et sans constante.

**Étude empirique du PIB US :** Représentation des cycles, tests de racine unitaire (ADF) et test KPSS.

## Langage de programmation

**Langage :** R

**Packages :** ggplot2, dplyr, urca, tseries, lubridate, tibbletime, scales

## Auteurs

📌 Pierre QUINTIN DE KERCADIO & Rémi HOUSSAIS  

📅 Année universitaire 2024 - 2025  
