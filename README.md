SeriousSearch v1.0.0
===================
*Serious Search Identification in User-generated data*
                                                            
* Copyright 2024-2026 <Anonymous submission>

SeriousSearch is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see licence.txt

* **Lab site:** Anonymous submission
* **GitHub repo:** Anonymous submission
* **Contact:** Anonymous submission

------------------------------------------------------------------------

This repository contains the R code used to process the data, generate all results, and produce the figures presented in the document *'**Who is Serious? A novel behavioral approach for identifying serious search on online real estate platforms**'*.  
A link to the document will be added if it is published.

The scripts provide a complete workflow for behavioral feature construction, analysis, and visualization.  
They can also be applied to custom datasets to identify user behaviors that may be interpreted as *serious search* within housing-related user‑generated data.


------------------------------------------------------------------------




If you use this source code, please cite article:

``` bibtex
```

------------------------------------------------------------------------

**Content**

- [Organization](#organization)
- [Installation](#installation)
- [Use](#use)
- [Dependencies](#dependencies)
- [References](#references)

------------------------------------------------------------------------

## Organization

- `in`: folder containing all the input data files.
  - `events.parquet`: dataset  (in [parquet](https://en.wikipedia.org/wiki/Apache_Parquet) format) of listings viewed by online searchers. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view);  `visitid` (view ID in the dataset);  `is_logged` (indication of wether the user was logged while viewing the listing).
  - `features.parquet`: dataset (in [parquet](https://en.wikipedia.org/wiki/Apache_Parquet) of every single listings' features. This table contains colums : `id_listing` (listing ID); `price` (property price); `area` (property area); `room_count` (property room count); `fct_room_count` (property room count set to factor variable); `sqm_price` (property square meter price); `item_type` (property type, wether a house or appartment);
  - `mail_phone.parquet`: a dataset (in [parquet](https://en.wikipedia.org/wiki/Apache_Parquet) format) of contact indicators. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view); `visitid` (view ID in the dataset); `event_action` (indication of the contact action a user made : displaying a phone number or submiting a mail form); `is_logged` (indication of wether the user was logged while viewing the listing).
  - `geom_sf_cities.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing cities' limits.
  - `geom_sf_departements.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing departements' limits. In the French context, departments correspond to the NUTS 3 level of the EU territorial classification system, which is an administrative unit with populations ranging from 150,000 to 800,000 inhabitants [[Eu'26](#references)].
- `out`: folder containing the outputs of the processing.
  - `pdf`: pdf files with the plots of the processing.
  - `Rdata`: Rdata files containing `R`objects such as dataframes.
  - `svg`: svg files with the plots of the processing.
  - `tex`: tex files (output in [Latex](https://fr.wikipedia.org/wiki/LaTeX) format) with the tables produced by the processing and provided in the document.
  - `parquet`: forlder containing output datasets (in [parquet](https://en.wikipedia.org/wiki/Apache_Parquet). For example, the output dataset of 'Serious and non serious users'.
  - `CSV`: forlder containing output datasets in [CSV](https://fr.wikipedia.org/wiki/Comma-separated_values) format.
- `src`: folder containing the source code.
  - `plot`: folder containing `R` scripts for plots.
    - `map_plot.R`: `R` script to plot Online search intensity by city.
    - `sample_search_cluster_plot.R`: `R` script to plot serious search graphics.
    - `save_tables.R`: `R` script to save tables in tex format.
    - `serious_search_pdf_plot.R`: `R`script to save graphics in pdf format.
    - `serious_searcher_svg_plots.R`: `R`script to save graphics in svg format.
  - `serious_search`: folder containing `R` scripts for the analysis of serious search.
    - `contact_indicators.R`: `R`script to compute contact indicators.
    - `revisit_contact_temporality.R`: `R`script to analyze behavioural temporality between Revist and Contact.
    - `revisit_contact_temporality.R`: `R`script to analyze the revisiting temporality.
    - `serious_search_through_revisit.R`: `R`script to compute listings' revisit indicators and search variability indicators.
    - `serious_search_through_revisit_analysis.R`: `R`script to analyze search engagement through listings' revisit indicators and search variability indicators.
    - `serious_search_through_revisit_sample_analysis.R` : `R`script to analyze search engagement through listings' revisit indicators and search variability indicators on a sample of the online searchers.
  - `create_directories.R` : `R` scripts to create directories.
  - `load_data.R`: `R` script to load data.
  - `packages_loading.R` : `R` script to install (if not ye installed) and load the packages used in this work, along with customer functions.
  - `processing_functions.R`: `R` scripts for data processing ad hoc functions.
  - `main.R`: the main `R` scripts to run the full processings.
  - `serious_search_through_revisit_along_with_contact_indicators.R`: Identify serious search with revisit and contact (phone number displaying and mail form submission) indicators.
  - `serious_search_through_revisit_without_contact_indicators.R`: Identify serious search with revisit only.
- `Serious_search_identification.Rproj`: `R` projet file to run the projet.

## Installation

### R and Packages
To run this program you need to install the `R` language environment and the required packages :
1. install the [`R` language](https://cran.r-project.org/)
2. Download this project from GitHub and unzip or clone it.
3. Run `packages_loading.R` to install the required packages (this step is included in `main.R`; a minimal Internet connection is needed).

### Data
You need to set up the files in folder `in`: `events.parquet`, `features.gpkg`, `mail_phone.parquet`, `geom_sf_cities.gpkg` and `geom_sf_departements.gpkg`. See the examples provided in folder `in`.


## Use

### To Replicate the Paper Experiments

#### Data Preparation
Unfortunately, we are not allowed to publish the data used in this paper. However, we provide a small fictional dataset designed to test the program and reproduce experiments similar to those presented in our paper (see folder `in`). 


#### Processing
Once the dataset is ready, do the following to apply the process described in the paper to these data: Run the `main.R` script using `source("main.R")`.

**Note** : [Rstudio](https://docs.posit.co/ide/user/)  user may first run `Serious_search_identification.Rproj` file to work under the project. For example, this will automatically set the working directory. But this is not necessary.


### To Apply the Paper Process to Custom Data

#### Data Preparation
First, you need to set files `events.parquet`, `features.gpkg`, `mail_phone.parquet` `geom_sf_cities.gpkg` and `geom_sf_departements.gpkg`:

* `events.parquet`: dataset  (in parquet format) of listings view by online searchers. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view);  `visitid` (view ID in the dataset);  `is_logged` (indication of wether the user was logged while viewing the listing).
* `features.gpkg`: dataset (in gpkg format) of every single listings' features. This table contains colums : `id_listing` (listing ID); `price` (property price); `area` (property area); `room_count` (property room count); `fct_room_count` (property room count set to factor variable); `sqm_price` (property square meter price); `item_type` (property type, wether a house or appartment);
* `mail_phone.parquet`: a dataset (in parquet format) of contact indicators. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view); `visitid` (view ID in the dataset); `event_action` (indication of the contact action a user made : displaying a phone number or submiting a mail form); `is_logged` (indication of wether the user was logged while viewing the listing).
* `geom_sf_cities.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing cities' limits. This table contains clomuns :
* `geom_sf_departements.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing departements' limits. This table contains clomuns :

#### Processing
  1. Copy your own `events.parquet`, `features.gpkg`, `geom_sf_cities.gpkg`, `geom_sf_departements.gpkg` and `mail_phone.parquet` in the `in` folder;
  2. Run the `main.R` script using `source("main.R")`. 

### To Perform Only the Serious Search Identification
These instructions are meant to perform a quick identification of serious search, without necessarily replicating all the experiments of the paper.
The procedure is slightly different depending on whether contact data are available to the user. Contact data refers to phone number displaying as well as mail form submission (cf. the paper for more detail).

##### Data Preparation
First, you need to set files `events.parquet`, `features.gpkg`, `mail_phone.parquet`, `geom_sf_cities.gpkg` and `geom_sf_departements.gpkg`:
  
* `events.parquet`: dataset  (in parquet format) of listings view by online searchers. Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view);  `visitid` (view ID in the dataset); `is_logged` (indication of wether the user was logged while viewing the listing).
* `features.gpkg`: dataset (in gpkg format) of every single listings' features. This table contains colums : `id_listing` (listing ID); `price` (property price); `area` (property area); `room_count` (property room count); `fct_room_count` (property room count set to factor variable); `sqm_price` (property square meter price); `item_type` (property type, wether a house or appartment);
* `mail_phone.parquet`: **only if the contact data are available.** This file contains contact indicators (in parquet format). Each line is a combination of a user with a listing. This table contains colums : `fullvisitorid` (user ID); `id_listing` (listing ID); `datetime` (date and time of view); `visitid` (view ID in the dataset); `event_action` (indication of the contact action a user made : displaying a phone number or submiting a mail form); `is_logged` (indication of wether the user was logged while viewing the listing).
* `geom_sf_cities.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing cities' limits. This table contains clomuns :
* `geom_sf_departements.gpkg`: a file (in [GeoPackage](https://en.wikipedia.org/wiki/GeoPackage) format) containing departements' limits. This table contains clomuns : 


#### Processing
1. Copy your own `events.parquet`, `features.gpkg`, `geom_sf_cities.gpkg`, `geom_sf_departements.gpkg`, and possibly `mail_phone.parquet` in the `in` folder;
2.
   -If contact indicators are available and want to incorporate them in the process, run the `serious_search_through_revisit_along_with_contact_indicators.R` script using `source("serious_search_through_revisit_along_with_contact_indicators.R")`.
  - If contact indicators are not available, run the `serious_search_through_revisit_without_contact_indicators.R` script using `source("serious_search_through_revisit_without_contact_indicators.R")`.
     
#### Output
The output of this processing is saved under `out/parquet/serious_search/serious_search_data.parquet` for [parquet](https://en.wikipedia.org/wiki/Apache_Parquet format and `out/CSV/serious_search/serious_search_data.CSV` for [CSV](https://fr.wikipedia.org/wiki/Comma-separated_values) format. Serious search is identified through the column `is_serious`, which is a logical variable.

## Dependencies


## References


* **[Eu'26]** **Eurostat** (2026).  
*NUTS — Nomenclature des unités territoriales statistiques: Principes.*  
URL: https://ec.europa.eu/eurostat/fr/web/nuts/principles

* **[Hu'20]** **Hurlbert, S. H.** (1971).  
*The nonconcept of species diversity.*  
Ecology, 52(4), 577–586.  
DOI: https://doi.org/10.2307/1934145

* **[Mc'77]** **McFadden, D.** (1977).  
*Quantitative Methods for Analyzing Travel Behaviour of Individuals.*  
Cowles Foundation Discussion Paper 707.  
URL: https://elischolar.library.yale.edu/cowles-discussion-paper-series/707


* **[Ok'24]** **Oksanen, J., et al.** (2024).  
*vegan: Community Ecology Package.*  
R package version 2.6-8.  
URL: https://CRAN.R-project.org/package=vegan

* **[Ma'04]** **Pagès, J.** (2004).  
*Analyse factorielle de données mixtes.*  
Revue de statistique appliquée, 52(4), 93–111.


* **[Pa'25]** **Pannu, A., & El-Saeiti, I. N.** (2025).  
*valuating predictive accuracy and model selection in logistic regression: a
statistical approach using sensitivity, specificity, and ROC analysis.*  
International Journal of Management, 16(1).  
DOI: https://doi.org/10.34218/IJM_16_01_002



* **[Ro'87]** **Rousseeuw, P. J.** (1987).  
*Silhouettes: a graphical aid to the interpretation and validation of cluster analysis.*  
Journal of Computational and Applied Mathematics, 53–65.  
DOI: https://doi.org/10.1016/0377-0427(87)90125-7

* **[Si'49]** **Simpson, E. H.** (1949).  
*Measurement of diversity.*  
Nature, 163(4148), 688.  
DOI: https://doi.org/10.1038/163688a0


* **[Zo'07]** **Zou, K. H., O’Malley, A. J., & Mauri, L.** (2007).  
*Receiver-operating characteristic analysis for evaluating diagnostic tests and predictive models.*  
Circulation, 115(5), 654–657.  
DOI: https://doi.org/10.1161/CIRCULATIONAHA.105.594929
