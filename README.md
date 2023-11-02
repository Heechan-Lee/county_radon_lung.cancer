# County-Level Lung Cancer Incidence and Environmental Exposures

## Overview

This repository contains the code used to evaluate the relationship between radon exposure, other environmental exposures (such as PM2.5), and lung cancer incidence at the county level. The research aims to explore the correlation between these exposures and lung cancer incidence rates.

The analysis involves the application of Poisson regression and Poisson random forest regression models. These models are implemented using R programming language and various libraries.

## Paper Information

- **Title:** Evaluating County-Level Lung Cancer Incidence from Environmental Radiation Exposure, PM2.5, and Other Exposures with Regression and Machine Learning Models
- **Description:** This paper investigates the relationship between radon exposure, PM2.5 pollution, and other environmental exposures with lung cancer incidence rates. The goal is to establish a connection between these exposures and the occurrence of lung cancer.
- **Link to Paper:** will be added

## Code Purpose and Features

The code in this repository serves the following purposes:

- Conduct Poisson regression and Poisson random forest regression analyses.
- Examine the correlation between radon exposure, PM2.5 pollution, and other exposures with lung cancer incidence.
- Utilize the `caret`, `rfCountData`, and `Metrics` libraries for implementing the analysis.

## Dependencies

- The code requires the R programming language.
- The following libraries are used:
  - `caret`: For training machine learning models.
  - `rfCountData` (Available at https://github.com/fpechon/rfCountData/): For handling random forest models in count data scenarios.
  - `Metrics`: For evaluating the performance of the models.

## Data Sources

- **RadNet - U.S. Environmental Protection Agency**
   - [RadNet](https://www.epa.gov/radnet/)

- **Radon (Radon Zone) - US Environmental Protection Agency (1993)**
   - [EPA’s map of radon zones national summary](https://www.epa.gov/radon/epa-map-radon-zones-and-supplemental-information)
- **Radon (median concentration) - National Environmental Public Health Tracking Network**
   - [Environmental Public Health Tracking Network](https://ephtracking.cdc.gov/DataExplorer/)

- **Air quality (PM 2.5, Ozone, Chemicals) - National Environmental Public Health Tracking Network**
   - [Environmental Public Health Tracking Network](https://ephtracking.cdc.gov/DataExplorer/)

- **Smoking - University of Wisconsin Population Health Institute (2022)**
   - [County Health Rankings & Roadmaps](https://www.countyhealthrankings.org)

- **Sociodemographic - National Cancer Institute, DCCPS, Surveillance Research Program (2022)**
   - [SEER*Stat Database: Incidence - SEER Research Data, 8 Registries, Nov 2021 Sub (1975-2019) - Linked To County Attributes - Time Dependent (1990-2019) Income/Rurality, 1969-2020 Counties](https://www.seer.cancer.gov)

- **Health Outcome (Lung cancer incidence) - National Cancer Institute, DCCPS, Surveillance Research Program (2022)**
   - [SEER*Stat Database: Incidence - SEER Research Data, 8 Registries, Nov 2021 Sub (1975-2019) - Linked To County Attributes - Time Dependent (1990-2019) Income/Rurality, 1969-2020 Counties](https://www.seer.cancer.gov)
 
   - Age Groups

     In this study, we divided the population into the following age groups, each spanning five years:

      - 30-34 years old
      - 35-39 years old
      - 40-44 years old
      - 45-49 years old
      - 50-54 years old
      - 55-59 years old
      - 60-64 years old
      - 65-69 years old
      - 70-74 years old
      - 75-79 years old
      - 80-84 years old

     These age groups were used to analyze lung and bronchus cancer incidences.

