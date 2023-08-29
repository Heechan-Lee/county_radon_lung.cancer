# County-Level Lung Cancer Incidence and Environmental Exposures

## Overview

This repository contains the code used to evaluate the relationship between radon exposure, other environmental exposures (such as PM2.5), and lung cancer incidence at the county level. The research aims to explore the correlation between these exposures and lung cancer incidence rates.

The analysis involves the application of Poisson regression and Poisson random forest regression models. These models are implemented using R programming language and various libraries.

## Paper Information

- **Title:** Evaluating County-Level Lung Cancer Incidence from Environmental Radiation Exposure, PM2.5, and Other Exposures with Regression and Machine Learning Models
- **Description:** This paper investigates the relationship between radon exposure, PM2.5 pollution, and other environmental exposures with lung cancer incidence rates. The goal is to establish a connection between these exposures and the occurrence of lung cancer.
- **Link to Paper:** [Insert Link to Paper Here]

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

## Instructions for Using the Code

1. Clone or download this repository to your local machine.
2. Ensure you have R installed on your system.
3. Download the `rfCountData` library from https://github.com/fpechon/rfCountData/.
4. Open the R script file named `lung_cancer_analysis.R`.
5. Install the required libraries if you haven't already:
   ```R
   install.packages("caret")
   install.packages("Metrics")
6. You also need library named rfCountData and please refer https://github.com/fpechon/rfCountData/blob/master/README.md to install the package.
