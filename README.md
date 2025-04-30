# Norwegian National Reading Test – Thesis Code (2022 & 2023)

This repository contains the full R code used in the analysis of 5th and 8th grade data from the **Norwegian National Reading Test**, conducted in 2022 and 2023. The scripts were written as part of a master’s thesis in Assessment, Measurement and Evaluation at the University of Oslo.

## Repository Contents

The repository includes two main R scripts:

- **`Data_2022.R`**  
  Full workflow for data management and analysis of the 2022 dataset.

- **`Data_2023.R`**  
  Full workflow for data management and analysis of the 2023 dataset.

Each script includes code for:

- Importing and preparing cohort datasets  
- Preliminary data exploration  
- Estimating GPCM and bifactor models  
- Evaluating model and item fit  
- Extracting item parameters  
- Calculating explained common variance (ECV)  
- Estimating ability scores and standard errors  
- Classifying students into mastery levels  
- Comparing classification outcomes and measurement precision
  
## Structure and Approach

- Scripts are organised by **year** and **grade level**.
- Code is clearly structured and annotated for full **transparency and replicability**.
- Procedures are consistent across years to support valid **comparative analysis**.
- A consistent **commenting and variable naming** convention is used to enhance readability.

## How to Use

1. Open either `Data_2022.R` or `Data_2023.R` in your R environment.
2. Ensure required packages (e.g., `mirt`) are installed.
3. Modify file paths if necessary to match your local setup.
4. Execute scripts section-by-section to replicate the full analysis.

> ℹ️ For details on methodology, see the accompanying master’s thesis.

## Contact

For questions or collaboration, contact: ane_ft@hotmail.com
