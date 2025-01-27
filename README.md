## Analysing how Key Audio Features vary across different <br> categories of Valence (Mood) in the *Spotify Tracks Dataset*


This repository contains analysis of the *Spotify Tracks Dataset*, (available [here](https://raw.githubusercontent.com/robert-jacques/datavisRProject/refs/heads/main/data/data.csv) and directly from
[Kaggle](https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset)), which is a tabular dataset of music tracks that are available on the popular music streaming platform, Spotify.
Each of the 114,000 tracks in the dataset is associated with a range of audio features, each of which provides a measurement of a particular musical characteristic, across 12 attributes in total.

The analysis was written using R and RStudio, and the code is designed to be run in RStudio. The R code can be found [here](https://raw.githubusercontent.com/robert-jacques/introdsRProject/refs/heads/main/code.R).

The aim of the project was to determine whether audio features can be used to predict the mood (valence) of a track, and involved the following steps:

* Data collection (Spotify Tracks Dataset),
* Data cleaning and pre-processing,
* Exploratory Data Analysis (EDA) with visualisations, 
* Further pre-processing and EDA,
* Baseline multiple linear regression modelling,
* Principal Component Analysis (PCA),
* PCA-based multiple linear regression modelling,
* Model refinement and evaluation.

Both the baseline and PCA-based multiple linear regression models demonstrated a relationship between audio features and valence, but several factors limited their performance.
The baseline model identified audio features as significant predictors of valence, while the reduced dimensionality introduced by the PCA-based model decreased predictive accuracy.
Additionally, performance was impacted by the biases and data loss brought about by the data pre-processing steps which were required.
 
**The project’s findings suggest that audio features do play a role in mood (valence) prediction, but that refinement is needed to optimise data pre-processing and modelling. These adjustments should facilitate improved predictive accuracy.**
