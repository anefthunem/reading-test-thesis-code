# Norwegian National Reading Test – Thesis Code (2022 & 2023)

This repository contains the full R code used in the analysis of 5th and 8th grade data from the **Norwegian National Reading Test**, conducted in 2022 and 2023. The scripts were written and used as part of a master's thesis submitted at University of Oslo.

## Repository Contents

- **Data 2022.R**  
  Contains the complete workflow for cleaning, processing, and analysing the 2022 dataset. Includes estimation of unidimensional and bifactor models using the Generalized Partial Credit Model (GPCM), classification of students into mastery levels, and model comparison metrics.

- **Data 2023.R**  
  Follows the same analytical structure for the 2023 dataset, ensuring comparability between years. Also includes bifactor modeling, classification, and model fit evaluations.

Each script is clearly commented and divided by year, grade level (5th and 8th), and modelling approach (unidimensional vs. bifactor). Outputs such as model objects, factor scores, and classification results are saved as `.RData` files for reproducibility.

## Purpose

This code was developed to ensure **transparency** and **replicability** in the thesis analyses. All steps described in the methodology section of the thesis are implemented here.

## How to Use

Open the `.R` files in RStudio or any R script editor. You may need to adjust file paths if running the scripts locally. The `mirt` package is used for all Item Response Theory (IRT) modeling.

## Contact

For questions or collaboration, contact: ane_ft@hotmail.com
