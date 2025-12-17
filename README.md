# Big Data Bowl — Defensive Anticipation Models

This repository contains the modeling and analysis used to generate defensive anticipation and individual impact metrics using NFL Big Data Bowl tracking data.

The raw tracking data, BDB supplementary file, and Sumer datasets must be downloaded and stored locally in order to reproduce results/rerun analysis.

## Data
This project requires the Big Data Bowl tracking dataset to be available locally.  
Weekly input and output data are also expected to be stored by week and are not tracked in this repository.

## Models

**Anticipation Models**  
`scripts/anticipation_models_clean.Rmd`

These models generate anticipation-related metrics during the ball-in-air window. The goal is to quantify how quickly and efficiently defenders react once the ball is thrown.

**Individual Models**  
`scripts/individual_models_clean.Rmd`

These models generate individual defensive impact metrics, specifically tackles (TKL) and passes defended (PD) probability gained from the start of the break to the end of the play.

## Analysis
`analysis/anticipation_models_analysis_clean.Rmd`  
`analysis/individual_models_analysis_clean.Rmd`  
`analysis/SchemeBreakdown_Clean.Rmd`

These files apply the trained models across the full dataset and produce aggregated results and visualizations.  
Once local data paths are set, these analysis files should be run first, as they assume model outputs already exist.

## Shiny App
`app_code.Rmd`  
`app.R`

These files handle data preparation & pre-loads GIFs to power the Shiny app used to explore anticipation and impact metrics interactively.
