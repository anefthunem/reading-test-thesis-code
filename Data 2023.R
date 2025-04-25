#Loading the mirt package
library(mirt)

#Import the data for grade 5 and 8
grade5_23 <- read.csv("~/Documents/MAE4191 Master's Thesis/np-lesing-5-trinn_2023.csv", sep = ";")
grade8_23<- read.csv("~/Documents/MAE4191 Master's Thesis/np-lesing-8-trinn_2023.csv", sep = ";")

#Calculate the number of columns excluding "kjonn" - grade 5
num_cols5_23 <- ncol(grade5_23) - sum(colnames(grade5_23) == "kjonn")
#Print the results
print(num_cols5_23)

#Calculate the number of columns excluding "kjonn" - grade 8
num_cols8_23 <- ncol(grade8_23) - sum(colnames(grade8_23) == "kjonn")
#Print the results
print(num_cols5_23)

#Get the number of rows for grade 5 in 2023
num_rows_5_23 <- nrow(grade5_23)
cat("Number of rows for grade 5 in 2023:", num_rows_5_23, "\n")

#Get the number of columns for grade 5 in 2023
num_columns_5_23 <- ncol(grade5_23)
cat("Number of columns for grade 5 in 2023:", num_columns_5_23, "\n")

#Get the number of rows for grade 8 in 2023
num_rows_8_23 <- nrow(grade8_23)
cat("Number of rows for grade 8 in 2023:", num_rows_8_23, "\n")

#Get the number of columns for grade 8 in 2023
num_columns_8_23 <- ncol(grade8_23)
cat("Number of columns for grade 8 in 2023:", num_columns_8_23, "\n")

#Find the unique values for each item in grade 5 (excluding the first column)
unique_values23_5 <- sapply(grade5_23[, -1], function(x) unique(na.omit(x)))
#Print the unique values for each item
print(unique_values23_5)

#Find the unique values for each item in grade 8 (excluding the first column)
unique_values23_8 <- sapply(grade8_23[, -1], function(x) unique(na.omit(x)))
#Print the unique values for each item
print(unique_values23_8)

#Find the proportion of values missing in each column
apply(grade5_23, 2, function(x) mean(is.na(x)))
apply(grade8_23, 2, function(x) mean(is.na(x)))

#Sort the data to find cohort items in grade 5
#Get all column names
column_names_5_23 <- colnames(grade5_23)

#Filter out "kjonn" and columns that start with "A" followed by a number
filtered_columns_5_23 <- grep("^(?!kjonn$|A[0-9])", column_names_5_23, perl = TRUE)

#Show the column numbers that match the criteria
filtered_columns_5_23

#Sort the data to find cohort items in grade 8
#Get all column names
column_names_8_23 <- colnames(grade8_23)

#Filter out "kjonn" and columns that start with "A" followed by a number
filtered_columns_8_23 <- grep("^(?!kjonn$|A[0-9])", column_names_8_23, perl = TRUE)

#Show the column numbers that match the criteria
filtered_columns_8_23

#Make new dataset with only cohort items for grade 5
grade5.23 <- grade5_23[, filtered_columns_5_23]

#Make new dataset with only cohort items for grade 8
grade8.23 <- grade8_23[, filtered_columns_8_23]

#Proportion of missing values for each passage
#Define item groups for Grade 5 passages
passages_5_23 <- list(
  "Fritidsaktiviteter" = names(grade5.23)[1:5],
  "Disney Filmer" = names(grade5.23)[6:11],
  "Ikke lov å le" = names(grade5.23)[12:17],
  "Sjoku" = names(grade5.23)[18:24],
  "Paranotteffekten" = names(grade5.23)[25:30]
)

#Define item groups for Grade 8 passages
passages_8_23 <- list(
  "Tasmansk pungulv" = names(grade8.23)[1:7],
  "Stargate" = names(grade8.23)[8:13],
  "Lysforurensing" = names(grade8.23)[14:19],
  "Skulearbeid" = names(grade8.23)[20:25],
  "Snikfotografen" = names(grade8.23)[26:31],
  "Den korte historien om egget" = names(grade8.23)[32:36],
  "Et skår i gleden" = names(grade8.23)[37:42]
)

#Calculate average missing proportion for each passage - grade 5
cat("Missingness by passage – Grade 5 (2023):\n")
sapply(passages_5_23, function(items) {
  mean(is.na(grade5.23[, items]))
})

#Calculate average missing proportion for each passage - grade 8
cat("\nMissingness by passage – Grade 8 (2023):\n")
sapply(passages_8_23, function(items) {
  mean(is.na(grade8.23[, items]))
})

# ------ Estimating unidimensional model - Grade 5 ------ 
#Fit the GPCM model for grade 5
myGPCMmodel_grade5.23 <- mirt(grade5.23, 1, itemtype = rep("gpcm", ncol(grade5.23)), 
                           SE = TRUE, 
                           verbose = FALSE
)
print(myGPCMmodel_grade5.23)

#Print the summary of the model
summary(myGPCMmodel_grade5.23)

#Extract item parameters
coef(myGPCMmodel_grade5.23)

# ------ Estimating factor scores for the GPCM model - Grade 5 ------
factorscores_GPCM_5.23 <- fscores(myGPCMmodel_grade5.23, method = "MAP", full.scores.SE = TRUE)

#Save factor scores as object
save(factorscores_GPCM_5.23, file = "factorscoresGPCM_5.23.RData")

# ------ Estimating M2 and itemfit for the GPCM model by subset - Grade 5 ------
#Split data by missingness patterns
grade5_missing_patterns.23 <- apply(is.na(grade5.23), 1, paste, collapse = "")
grade5_unique_patterns.23 <- unique(grade5_missing_patterns.23)

#Create subsets for each missing pattern
grade5_data_subsets.23 <- lapply(grade5_unique_patterns.23, function(pattern) {
  grade5.23[grade5_missing_patterns.23 == pattern, , drop = FALSE]
})

#Keep only complete columns in each subset 
grade5_complete_subsets.23 <- lapply(grade5_data_subsets.23, function(subset) {
  subset[ , colSums(is.na(subset)) == 0, drop = FALSE]
})

#Fit Unidimensional GPCM Models for Each Subset
grade5_models.23 <- lapply(grade5_complete_subsets.23, function(subset) {
  if (ncol(subset) > 1) { # Ensure at least two columns to fit a model
    mirt(subset, 1, itemtype = "gpcm") # Fit GPCM model
  } else {
    NULL
  }
})

#Calculate M2 for Each Model
unidim_M2_5.23 <- lapply(grade5_models.23, function(model) {
  if (!is.null(model)) {
    M2(model)
  } else {
    NA
  }
})

#Calculate itemfit for each model
unidim_itemfit_5.23 <- lapply(grade5_models.23, function(model) {
  if (!is.null(model)) {
    itemfit(model, p.adjust = "holm")
  } else {
    NULL
  }
})

#Print results for each subset
for (i in seq_along(grade5_models.23)) {
  if (!is.null(grade5_models.23[[i]])) {
    cat("\nGrade 5 Unidimensional Subset", i, "\n")
    
    #Print M2
    cat("M² Results:\n")
    print(unidim_M2_5.23[[i]])
    
    #Print itemfit
    cat("\nItemfit Results:\n")
    print(unidim_itemfit_5.23[[i]])
    
    #Subset info
    subset <- grade5_complete_subsets.23[[i]]
    cat("\nItems included in this subset:\n", paste(names(subset), collapse = ", "), "\n")
    
    #Count misfits
    misfit_count <- sum(unidim_itemfit_5.23[[i]]$p.S_X2 < 0.05, na.rm = TRUE)
    cat("Number of items with significant misfit:", misfit_count, "out of", nrow(unidim_itemfit_5.23[[i]]), "\n")
  }
}

#Save as Object
save(unidim_M2_5.23, file = "M2_grade5_results.23.RData")
save(unidim_itemfit_5.23, file = "itemfit_unidim_grade5_2023.RData")

# ------ Estimating unidimensional model - Grade 8 ------ 
#Fit the GPCM model for grade 8
myGPCMmodel_grade8.23 <- mirt(grade8.23, 1, itemtype = rep("gpcm", ncol(grade8.23)), 
                           SE = TRUE, 
                           verbose = FALSE
)
print(myGPCMmodel_grade8.23)

#Print the summary of the model
summary(myGPCMmodel_grade8.23)

#Extract item parameters
coef(myGPCMmodel_grade8.23)
coef(myGPCMmodel_grade8.23, printSE = TRUE)

# ------ Estimating factor scores for the GPCM model - Grade 8 ------
factorscores_GPCM_8.23 <- fscores(myGPCMmodel_grade8.23, method = "MAP", full.scores.SE = TRUE)

#Save factor scores as object
save(factorscores_GPCM_8.23, file = "factorscoresGPCM_8.23.RData")

# ------ Estimating M2 and itemfit for the GPCM model by subset - Grade 8 ------
#Split data by missingness patterns
grade8_missing_patterns.23 <- apply(is.na(grade8.23), 1, paste, collapse = "")
grade8_unique_patterns.23 <- unique(grade8_missing_patterns.23)

#Create subsets for each missing pattern
grade8_data_subsets.23 <- lapply(grade8_unique_patterns.23, function(pattern) {
  grade8.23[grade8_missing_patterns.23 == pattern, , drop = FALSE]
})

#Keep only complete columns in each subset
grade8_complete_subsets.23 <- lapply(grade8_data_subsets.23, function(subset) {
  subset[ , colSums(is.na(subset)) == 0, drop = FALSE]
})

#Fit Unidimensional GPCM Models for Each Subset
grade8_models.23 <- lapply(grade8_complete_subsets.23, function(subset) {
  if (ncol(subset) > 1) { # Ensure at least two columns to fit a model
    mirt(subset, 1, itemtype = "gpcm") # Fit GPCM model
  } else {
    NULL
  }
})

# Calculate M2 for Each Model
unidim_M2_8.23 <- lapply(grade8_models.23, function(model) {
  if (!is.null(model)) {
    M2(model)
  } else {
    NA
  }
})

# Calculate itemfit for each model
unidim_itemfit_8.23 <- lapply(grade8_models.23, function(model) {
  if (!is.null(model)) {
    itemfit(model, p.adjust = "holm")
  } else {
    NULL
  }
})

# Print results for each subset
for (i in seq_along(grade8_models.23)) {
  if (!is.null(grade8_models.23[[i]])) {
    cat("\nGrade 8 Unidimensional Subset", i, "\n")
    
    # Print M2
    cat("M² Results:\n")
    print(unidim_M2_8.23[[i]])
    
    # Print itemfit
    cat("\nItemfit Results:\n")
    print(unidim_itemfit_8.23[[i]])
    
    # Subset info
    subset <- grade8_complete_subsets.23[[i]]
    cat("\nItems included in this subset:\n", paste(names(subset), collapse = ", "), "\n")
    
    # Misfit count
    misfit_count <- sum(unidim_itemfit_8.23[[i]]$p.S_X2 < 0.05, na.rm = TRUE)
    cat("Number of items with significant misfit:", misfit_count, "out of", nrow(unidim_itemfit_8.23[[i]]), "\n")
  }
}

# Save results
save(unidim_M2_8.23, file = "M2_grade8_results.23.RData")
save(unidim_itemfit_8.23, file = "itemfit_unidim_grade8_2023.RData")

# ------ Estimating bifactor model using a unidimensional irt model - Grade 5 ------ 
#Fit the bifactor model for Grade 5

#Item names for each stimulus
names(grade5.23)
names(grade5.23)[1:5] #Fritidsaktiviteter
names(grade5.23)[6:11] #Disney Filmer
names(grade5.23)[12:17] #Ikke lov å le
names(grade5.23)[18:24] #Sjoku
names(grade5.23)[25:30] #Paranotteffekten

#Fit a unidimensional model
mirtest1Dgrade5.23 <- mirt(grade5.23, 1, SE = TRUE)

#Specify unidimensional model
bifgrade5.23 <- 'F1 = 1-30'

#Specify residual factors model
#This indicates which residual factor is related to which set of items
specgrade5.23 <- c(rep(1, 5), rep(2, 6), rep(3, 6), rep(4, 7), rep(5, 6))

#Estimate bifactor model with dimension-reduction method with the bfactor() function
mirtest8Dgrade5.23 <- bfactor(grade5.23, specgrade5.23, bifgrade5.23, itemtype = "gpcm", quadpts = 25, TOL = 1e-4, technical = list(NCYCLES = 50000))
mirtest8Dgrade5.23

#Save as Object
save(mirtest8Dgrade5.23, file = "mirtest8Dgrade5.23.RData")

#Estimate bifactor model with standard errors, with starting values from the estimated model above
mirtest8Dgrade5.23SE <- bfactor(grade5.23, specgrade5.23, bifgrade5.23, itemtype = "gpcm", quadpts = 25, TOL = 1e-4, 
                                pars = mod2values(mirtest8Dgrade5.23), SE = TRUE, technical = list(NCYCLES = 50000))
mirtest8Dgrade5.23SE

#Save as Object
save(mirtest8Dgrade5.23SE, file = "mirtest8Dgrade5.23SE.RData")

#Print the summary of the models
summary(mirtest8Dgrade5.23)
summary(mirtest8Dgrade5.23SE)

#Extract item parameters from bifactor model with SEs
coef(mirtest8Dgrade5.23SE, printSE = TRUE)

# ------ Estimating factor scores for the bifactor model - Grade 5 ------
factorscores_Bif_5.23 <- fscores(mirtest8Dgrade5.23SE, method = "MAP", full.scores.SE = TRUE)

# Save as Object
save(factorscores_Bif_5.23, file = "factorscoresBif_5.23.RData")

# ------ Estimating M2 and itemfit for the bifactor model by subset - Grade 5 ------
#Split data by missingness patterns
bif_missing_patterns_grade5.23 <- apply(is.na(grade5.23), 1, paste, collapse = "")
bif_unique_patterns_grade5.23 <- unique(bif_missing_patterns_grade5.23)

#Create subsets for each missing pattern
bif_data_subsets_grade5.23 <- lapply(bif_unique_patterns_grade5.23, function(pattern) {
  grade5.23[bif_missing_patterns_grade5.23 == pattern, , drop = FALSE]
})

#Keep only complete columns in each subset
bif_complete_subsets_grade5.23 <- lapply(bif_data_subsets_grade5.23, function(subset) {
  subset[ , colSums(is.na(subset)) == 0, drop = FALSE]
})

#Fit bifactor models for each subset
bif_models_grade5.23 <- lapply(bif_complete_subsets_grade5.23, function(subset) {
  if (ncol(subset) > 1) { # Ensure at least two columns to fit a model
    # Get the spec factors for the columns present in this subset
    col_indices <- match(names(subset), names(grade5.23))
    subset_specs <- specgrade5.23[col_indices]
    bif_spec <- paste0('F1 = 1-', ncol(subset))
    bfactor(subset, 
            subset_specs,
            bif_spec,
            itemtype = "gpcm",
            quadpts = 25, 
            TOL = 1e-4, 
            technical = list(NCYCLES = 50000))
  } else {
    NULL
  }
})

#Calculate M² for each model
bif_M2_5.23 <- lapply(bif_models_grade5.23, function(model) {
  if (!is.null(model)) {
    M2(model, QMC = TRUE, quadpts = 20000)
  } else {
    NA
  }
})

#Calculate itemfit for each model
bif_itemfit_5.23 <- lapply(bif_models_grade5.23, function(model) {
  if (!is.null(model)) {
    fit <- itemfit(model, p.adjust = "holm", QMC = TRUE, quadpts = 20000)
    return(fit)
  } else {
    return(NULL)
  }
})

#Print results for each subset
for (i in seq_along(bif_models_grade5.23)) {
  model <- bif_models_grade5.23[[i]]
  
  if (!is.null(model)) {
    cat("\nGrade 5 Bifactor Subset", i, "\n")
    
    #Print M2
    cat("M² Results:\n")
    print(bif_M2_5.23[[i]])
    
    #Print itemfit
    cat("\nItemfit Results:\n")
    print(bif_itemfit_5.23[[i]])
    
    #Subset info
    subset <- bif_complete_subsets_grade5.23[[i]]
    col_indices <- match(names(subset), names(grade5.23))
    subset_specs <- specgrade5.23[col_indices]
    
    cat("\nItems included in this subset:\n", paste(names(subset), collapse = ", "), "\n")
    cat("Specific factors used:\n", paste(subset_specs, collapse = ", "), "\n")
    
    # Count significant misfits
    misfit_count <- sum(bif_itemfit_5.23[[i]]$p.S_X2 < 0.05, na.rm = TRUE)
    cat("Number of items with significant misfit:", misfit_count, "out of", nrow(bif_itemfit_5.23[[i]]), "\n")
  }
}

#Save as results
save(bif_M2_5.23, file = "M2_2023_bif_results_grade5.23.RData")
save(bif_itemfit_5.23, file = "itemfit_bifactor_grade5_2023.RData")

# ------ Estimating bifactor model using a unidimensional irt model - Grade 8 ------ 
#Fit the bifactor model for Grade 8

#Item names for each stimulus
names(grade8.23)
names(grade8.23)[1:7] #Tasmansk pungulv
names(grade8.23)[8:13] #Stargate
names(grade8.23)[14:19] #Lysforurensing
names(grade8.23)[20:25] #Skulearbeid
names(grade8.23)[26:31] #Snikfotografen
names(grade8.23)[32:36] #Den korte historien om egget
names(grade8.23)[37:42] #Et skår i gleden

#Fit a unidimensional model
mirtest1Dgrade8.23 <- mirt(grade8.23, 1, SE = TRUE)

#Specify unidimensional model
bifgrade8.23 <- 'F1 = 1-42'

#Specify residual factors model
#This indicates which residual factor is related to which set of items
specgrade8.23 <- c(rep(1, 7), rep(2, 6), rep(3, 6), rep(4, 6), rep(5, 6), rep(6, 5), rep(7, 6))

#Estimate bifactor model with dimension-reduction method with the bfactor() function
mirtest8Dgrade8.23 <- bfactor(grade8.23, specgrade8.23, bifgrade8.23, itemtype = "gpcm", quadpts = 25, TOL = 1e-4, technical = list(NCYCLES = 50000))
mirtest8Dgrade8.23

# Save as Object
save(mirtest8Dgrade8.23, file = "mirtest8Dgrade8.23.RData")

#Estimate bifactor model with standard errors, with starting values from the estimated model above
mirtest8Dgrade8.23SE <- bfactor(grade8.23, specgrade8.23, bifgrade8.23, itemtype = "gpcm", quadpts = 25, TOL = 1e-4, 
                                pars = mod2values(mirtest8Dgrade8.23), SE = TRUE, technical = list(NCYCLES = 50000))
mirtest8Dgrade8.23SE

#Save as Object
save(mirtest8Dgrade8.23SE, file = "mirtest8Dgrade8.23SE.RData")

#Print the summary of the models
summary(mirtest8Dgrade8.23)
summary(mirtest8Dgrade8.23SE)

#Extract item parameters
coef(mirtest8Dgrade8.23SE, printSE = TRUE)

# ------ Estimating factor scores for the bifactor model - Grade 8 ------
factorscores_Bif_8.23 <- fscores(mirtest8Dgrade8.23SE, method = "MAP", full.scores.SE = TRUE)

#Save as Object
save(factorscores_Bif_8.23, file = "factorscoresBif_8.23.RData")

# ------ Estimating M2 and itemfit for the bifactor model by subset - Grade 8 ------
#Split data by missingness patterns
bif_missing_patterns_grade8.23 <- apply(is.na(grade8.23), 1, paste, collapse = "")
bif_unique_patterns_grade8.23 <- unique(bif_missing_patterns_grade8.23)

#Create subsets for each missing pattern
bif_data_subsets_grade8.23 <- lapply(bif_unique_patterns_grade8.23, function(pattern) {
  grade8.23[bif_missing_patterns_grade8.23 == pattern, , drop = FALSE]
})

#Keep only complete columns in each subset
bif_complete_subsets_grade8.23 <- lapply(bif_data_subsets_grade8.23, function(subset) {
  subset[ , colSums(is.na(subset)) == 0, drop = FALSE]
})

#Fit bifactor models for each subset using only group factors
bif_models_grade8.23 <- lapply(bif_complete_subsets_grade8.23, function(subset) {
  if (ncol(subset) > 1) { # Ensure at least two columns to fit a model
    #Get the spec factors for the columns present in this subset
    col_indices <- match(names(subset), names(grade8.23))
    subset_specs <- specgrade8.23[col_indices]
    bif_spec <- paste0('F1 = 1-', ncol(subset))
    bfactor(subset, 
            subset_specs,
            bif_spec,
            itemtype = "gpcm",
            quadpts = 25, 
            TOL = 1e-4, 
            technical = list(NCYCLES = 50000))
  } else {
    NULL
  }
})

#Calculate M² for each model
bif_M2_8.23 <- lapply(bif_models_grade8.23, function(model) {
  if (!is.null(model)) {
    M2(model, QMC = TRUE, quadpts = 20000)
  } else {
    NA
  }
})

#Calculate itemfit for each model
bif_itemfit_8.23 <- lapply(bif_models_grade8.23, function(model) {
  if (!is.null(model)) {
    itemfit(model, p.adjust = "holm", QMC = TRUE, quadpts = 20000)
  } else {
    NULL
  }
})

#Print results for each subset
for (i in seq_along(bif_models_grade8.23)) {
  model <- bif_models_grade8.23[[i]]
  
  if (!is.null(model)) {
    cat("\nGrade 8 Bifactor Subset", i, "\n")
    
    #Print M2
    cat("M² Results:\n")
    print(bif_M2_8.23[[i]])
    
    #Print itemfit
    cat("\nItemfit Results:\n")
    print(bif_itemfit_8.23[[i]])
    
    #Subset info
    subset <- bif_complete_subsets_grade8.23[[i]]
    col_indices <- match(names(subset), names(grade8.23))
    subset_specs <- specgrade8.23[col_indices]
    
    cat("\nItems included in this subset:\n", paste(names(subset), collapse = ", "), "\n")
    cat("Specific factors used:\n", paste(subset_specs, collapse = ", "), "\n")
    
    #Count significant misfits
    misfit_count <- sum(bif_itemfit_8.23[[i]]$p.S_X2 < 0.05, na.rm = TRUE)
    cat("Number of items with significant misfit:", misfit_count, "out of", nrow(bif_itemfit_8.23[[i]]), "\n")
  }
}

#Save as Object
save(bif_M2_8.23, file = "M2_2023_bif_results_grade8.23.RData")
save(bif_itemfit_8.23, file = "itemfit_bifactor_grade8_2023.RData")

# ------ Classification into mastery level for 5th grade - from factor scores ------ 
#Compute the quantile cutoffs for classification from the unidimensional model - Grade 5
quantiles_1D_5.23 <- quantile(factorscores_GPCM_5.23[, "F1"], probs = c(0.25, 0.75), na.rm = TRUE)

#Define mastery levels based on MAP scores from the unidimensional model - Grade 5
mastery_levels_1D_5.23 <- cut(factorscores_GPCM_5.23[, "F1"], 
                              breaks = c(-Inf, quantiles_1D_5.23[1], quantiles_1D_5.23[2], Inf), 
                              labels = c("Mastery Level 1", "Mastery Level 2", "Mastery Level 3"), 
                              include.lowest = TRUE)

#Compute the quantile cutoffs for classification from the bifactor model - Grade 5
quantiles_8D_5.23 <- quantile(factorscores_Bif_5.23[, "F1"], probs = c(0.25, 0.75), na.rm = TRUE)

#Define mastery levels based on MAP scores from the bifactor model - Grade 5
mastery_levels_8D_5.23 <- cut(factorscores_Bif_5.23[, "F1"], 
                              breaks = c(-Inf, quantiles_8D_5.23[1], quantiles_8D_5.23[2], Inf), 
                              labels = c("Mastery Level 1", "Mastery Level 2", "Mastery Level 3"), 
                              include.lowest = TRUE)

#Create classified dataset with both models - Grade 5
classified_students_5.23 <- data.frame(Student_ID = 1:length(factorscores_GPCM_5.23), 
                                       MAP_Score_1D = factorscores_GPCM_5.23, 
                                       Mastery_Level_1D = mastery_levels_1D_5.23,
                                       MAP_Score_8D = factorscores_Bif_5.23[,1], 
                                       Mastery_Level_8D = mastery_levels_8D_5.23)

#Calculate the proportion of students classified the same way by both models - Grade 5
same_classification_5.23 <- sum(classified_students_5.23$Mastery_Level_1D == classified_students_5.23$Mastery_Level_8D, na.rm = TRUE) / nrow(classified_students_5.23)

#Print the proportion of students classified the same way - Grade 5
cat("Proportion of students classified the same way by both models (Grade 5):", same_classification_5.23, "\n")

#Print the first few rows of the classified dataset - Grade 5
head(classified_students_5.23)

# ------ Classification into mastery level for 8th grade - from factor scores ------ 
#Compute the quantile cutoffs for classification from the unidimensional model - Grade 8
quantiles_1D_8.23 <- quantile(factorscores_GPCM_8.23[, "F1"], probs = c(0.1, 0.3, 0.7, 0.9), na.rm = TRUE)

#Define mastery levels based on MAP scores from the unidimensional model - Grade 8
mastery_levels_1D_8.23 <- cut(factorscores_GPCM_8.23[, "F1"], 
                              breaks = c(-Inf, quantiles_1D_8.23[1], quantiles_1D_8.23[2], quantiles_1D_8.23[3], quantiles_1D_8.23[4], Inf), 
                              labels = c("Mastery Level 1", "Mastery Level 2", "Mastery Level 3", "Mastery Level 4", "Mastery Level 5"), 
                              include.lowest = TRUE)

#Compute the quantile cutoffs for classification from the bifactor model - Grade 8
quantiles_8D_8.23 <- quantile(factorscores_Bif_8.23[, "F1"], probs = c(0.1, 0.3, 0.7, 0.9), na.rm = TRUE)

#Define mastery levels based on MAP scores from the bifactor model - Grade 8
mastery_levels_8D_8.23 <- cut(factorscores_Bif_8.23[, "F1"], 
                              breaks = c(-Inf, quantiles_8D_8.23[1], quantiles_8D_8.23[2], quantiles_8D_8.23[3], quantiles_8D_8.23[4], Inf), 
                              labels = c("Mastery Level 1", "Mastery Level 2", "Mastery Level 3", "Mastery Level 4", "Mastery Level 5"), 
                              include.lowest = TRUE)

#Create classified dataset with both models - Grade 8
classified_students_8.23 <- data.frame(Student_ID = 1:length(factorscores_GPCM_8.23), 
                                       MAP_Score_1D = factorscores_GPCM_8.23, 
                                       Mastery_Level_1D = mastery_levels_1D_8.23,
                                       MAP_Score_8D = factorscores_Bif_8.23[,1], 
                                       Mastery_Level_8D = mastery_levels_8D_8.23)

#Calculate the proportion of students classified the same way by both models - Grade 8
same_classification_8.23 <- sum(classified_students_8.23$Mastery_Level_1D == classified_students_8.23$Mastery_Level_8D, na.rm = TRUE) / nrow(classified_students_8.23)

#Print the proportion of students classified the same way - Grade 8
cat("Proportion of students classified the same way by both models (Grade 8):", same_classification_8.23, "\n")

#Print the first few rows of the classified dataset - Grade 8
head(classified_students_8.23)

#Comparing the nested models using anova
anova(myGPCMmodel_grade5.23, mirtest8Dgrade5.23SE)
anova(myGPCMmodel_grade8.23, mirtest8Dgrade8.23SE)

# ------ Caclulating the Explained Common Variance ------

# Grade 5
# Extract loadings using the factor specifications from your model
# Grade 5 has 1 general factor (F1) and 5 specific factors
loadings_5_23 <- extract.mirt(mirtest8Dgrade5.23SE, 'F')

#First column is the general factor
lambda_G_5_23 <- loadings_5_23[, 1]^2

#Remaining columns are specific factors
lambda_S_5_23 <- rowSums(loadings_5_23[, -1, drop=FALSE]^2)

#Calculate ECV - Grade 5
ECV_5_23 <- sum(lambda_G_5_23) / (sum(lambda_G_5_23) + sum(lambda_S_5_23))
cat("ECV for Grade 5 (2023):", round(ECV_5_23, 3), "\n")

#Grade 8
#Grade 8 has 1 general factor (F1) and 7 specific factors
loadings_8_23 <- extract.mirt(mirtest8Dgrade8.23SE, 'F')

#First column is the general factor
lambda_G_8_23 <- loadings_8_23[, 1]^2

#Remaining columns are specific factors
lambda_S_8_23 <- rowSums(loadings_8_23[, -1, drop=FALSE]^2)

#Calculate ECV - Grade 8
ECV_8_23 <- sum(lambda_G_8_23) / (sum(lambda_G_8_23) + sum(lambda_S_8_23))
cat("ECV for Grade 8 (2023):", round(ECV_8_23, 3), "\n")

# ------ Estimating standard errors of factor scores ------

#Unidimensional
SE_uni_5_23 <- factorscores_GPCM_5.23[, "SE_F1"]
SE_uni_8_23 <- factorscores_GPCM_8.23[, "SE_F1"]

#Bifactor (general factor only)
SE_bi_5_23 <- factorscores_Bif_5.23[, "SE_F1"]
SE_bi_8_23 <- factorscores_Bif_8.23[, "SE_F1"]

#Compute Average Standard Errors
avg_SE_uni_5_23 <- mean(SE_uni_5_23, na.rm = TRUE)
avg_SE_bi_5_23  <- mean(SE_bi_5_23, na.rm = TRUE)

avg_SE_uni_8_23 <- mean(SE_uni_8_23, na.rm = TRUE)
avg_SE_bi_8_23  <- mean(SE_bi_8_23, na.rm = TRUE)

#Create Summary Table
SE_comparison_2023 <- data.frame(
  Grade = c(5, 8),
  Year = 2023,
  SE_Unidimensional = c(avg_SE_uni_5_23, avg_SE_uni_8_23),
  SE_Bifactor_General = c(avg_SE_bi_5_23, avg_SE_bi_8_23)
)

print(SE_comparison_2023)

