#Loading the mirt package
library(mirt)

#Import the data for grade 5 and 8
grade5_22 <- read.csv("~/Documents/MAE4191 Master's Thesis/np-lesing-5-trinn_2022.csv", sep = ";")
grade8_22 <- read.csv("~/Documents/MAE4191 Master's Thesis/np-lesing-8-trinn_2022.csv", sep = ";")

#Find the number of items (all columns excluding "kjonn") - grade 5
num_cols5_22 <- ncol(grade5_22) - sum(colnames(grade5_22) == "kjonn")
#Print the results
print(num_cols5_22)

#Find the number of items (all columns excluding "kjonn") - grade 8
num_cols8_22 <- ncol(grade8_22) - sum(colnames(grade8_22) == "kjonn")
#Print the results
print(num_cols8_22)

#Get the number of rows for grade 5 in 2022
num_rows_5_22 <- nrow(grade5_22)
cat("Number of rows for grade 5 in 2022:", num_rows_5_22, "\n")

#Get the number of columns for grade 8 in 2022
num_columns_5_22 <- ncol(grade5_22)
cat("Number of columns for grade 5 in 2022:", num_columns_5_22, "\n")

#Get the number of rows for grade 5 in 2022
num_rows_8_22 <- nrow(grade8_22)
cat("Number of rows for grade 8 in 2022:", num_rows_8_22, "\n")

#Get the number of columns for grade 8 in 2022
num_columns_8_22 <- ncol(grade8_22)
cat("Number of columns for grade 8 in 2022:", num_columns_8_22, "\n")

#Find the unique values for each item in grade 5 (excluding the first column)
unique_values22_5 <- sapply(grade5_22[, -1], function(x) unique(na.omit(x)))
#Print the unique values for each item
print(unique_values22_5)

#Find the unique values for each item in grade 8 (excluding the first column)
unique_values22_8 <- sapply(grade8_22[, -1], function(x) unique(na.omit(x)))
#Print the unique values for each item
print(unique_values22_8)

#Find the proportion of values missing in each column
apply(grade5_22, 2, function(x) mean(is.na(x)))
apply(grade8_22, 2, function(x) mean(is.na(x)))

#Sort the data to find cohort items in grade 5
#Get all column names
column_names_5_22 <- colnames(grade5_22)

#Filter out "kjonn" and columns that start with "A" followed by a number
filtered_columns_5_22 <- grep("^(?!kjonn$|A[0-9])", column_names_5_22, perl = TRUE)

#Show the column numbers that match the criteria
filtered_columns_5_22

#Sort the data to find cohort items in grade 8
#Get all column names
column_names_8_22 <- colnames(grade8_22)

#Filter out "kjonn" and columns that start with "A" followed by a number
filtered_columns_8_22 <- grep("^(?!kjonn$|A[0-9])", column_names_8_22, perl = TRUE)

#Show the column numbers that match the criteria
filtered_columns_8_22

#Make new dataset with only cohort items for grade 5
grade5.22 <- grade5_22[, filtered_columns_5_22]

#Make new dataset with only cohort items for grade 8
grade8.22 <- grade8_22[, filtered_columns_8_22]

#Find the number of cohort items (all columns in new dataset excluding "kjonn") - grade 5
num_cohort5_22 <- ncol(grade5.22) - sum(colnames(grade5.22) == "kjonn")
#Print the results
print(num_cohort5_22)

#Find the number of cohort items (all columns in new dataset excluding "kjonn") - grade 8
num_cohort8_22 <- ncol(grade8.22) - sum(colnames(grade8.22) == "kjonn")
#Print the results
print(num_cohort8_22)

#Proportion of missing values for each passage
#Define item groups for Grade 5
passages_5_22 <- list(
  Kraken = names(grade5.22)[1:6],
  Tromsøbadet = names(grade5.22)[7:12],
  `Spådama i 7B` = names(grade5.22)[13:19],
  Skateboard = names(grade5.22)[20:25],
  `Farlig katt` = names(grade5.22)[26:32]
)

#Define item groups for Grade 8
passages_8_22 <- list(
  `Verdens kaldeste sted` = names(grade8.22)[1:6],
  `Hull og sønn` = names(grade8.22)[7:13],
  iHuman = names(grade8.22)[14:19],
  Havet = names(grade8.22)[20:25],
  Challengerdypet = names(grade8.22)[26:31],
  `Knøttfe og himmelreker` = names(grade8.22)[32:37],
  `Arne og Jakob` = names(grade8.22)[38:42]
)

#Calculate average missing proportion for each passage - grade 5
cat("Missingness by passage – Grade 5 (2022):\n")
sapply(passages_5_22, function(items) {
  mean(is.na(grade5.22[ , items]))
})

#Calculate average missing proportion for each passage - grade 8
cat("\nMissingness by passage – Grade 8 (2022):\n")
sapply(passages_8_22, function(items) {
  mean(is.na(grade8.22[ , items]))
})

# ------ Estimating unidimensional model - Grade 5 ------ 
#Fit the GPCM model for grade 5
myGPCMmodel_grade5.22 <- mirt(grade5.22, 1, itemtype = rep("gpcm", ncol(grade5.22)), 
                              SE = TRUE, 
                              verbose = FALSE
)
print(myGPCMmodel_grade5.22)

#Print the summary of the model
summary(myGPCMmodel_grade5.22)

#Extract item parameters
coef(myGPCMmodel_grade5.22)
coef(myGPCMmodel_grade5.22, printSE = TRUE)

# ------ Estimating factor scores for the GPCM model - Grade 5 ------
factorscores_GPCM_5.22 <- fscores(myGPCMmodel_grade5.22, method = "MAP", full.scores.SE = TRUE)

# ------ Estimating M2 and itemfit for the GPCM model by subset - Grade 5 ------
#Split data by missingness patterns
grade5_missing_patterns.22 <- apply(is.na(grade5.22), 1, paste, collapse = "")
grade5_unique_patterns.22 <- unique(grade5_missing_patterns.22)

#Create subsets for each missing pattern
grade5_data_subsets.22 <- lapply(grade5_unique_patterns.22, function(pattern) {
  grade5.22[grade5_missing_patterns.22 == pattern, , drop = FALSE]
})

#Keep only complete columns in each subset
grade5_complete_subsets.22 <- lapply(grade5_data_subsets.22, function(subset) {
  subset[ , colSums(is.na(subset)) == 0, drop = FALSE]
})

#Fit Unidimensional IRT Models for Each Subset
grade5_models.22 <- lapply(grade5_complete_subsets.22, function(subset) {
  if (ncol(subset) > 1) { # Ensure at least two columns to fit a model
    mirt(subset, 1, itemtype = "gpcm") # Fit unidimensional model
  } else {
    NULL
  }
})

#Calculate M2 for Each Model
unidim_M2_5.22 <- lapply(grade5_models.22, function(model) {
  if (!is.null(model)) {
    M2(model)
  } else {
    NA
  }
})

#Calculate itemfit for each model
unidim_itemfit_5.22 <- lapply(grade5_models.22, function(model) {
  if (!is.null(model)) {
    itemfit(model, p.adjust = "holm")
  } else {
    NULL
  }
})

#Print results for each subset
for (i in seq_along(grade5_models.22)) {
  if (!is.null(grade5_models.22[[i]])) {
    cat("\nGrade 5 Unidimensional Subset", i, "\n")
    
    #Print M2
    cat("M² Results:\n")
    print(unidim_M2_5.22[[i]])
    
    #Print itemfit
    cat("\nItemfit Results:\n")
    print(unidim_itemfit_5.22[[i]])
    
    #Subset info
    subset <- grade5_complete_subsets.22[[i]]
    cat("\nItems included in this subset:\n", paste(names(subset), collapse = ", "), "\n")
    
    #Misfit count
    misfit_count <- sum(unidim_itemfit_5.22[[i]]$p.S_X2 < 0.05, na.rm = TRUE)
    cat("Number of items with significant misfit:", misfit_count, "out of", nrow(unidim_itemfit_5.22[[i]]), "\n")
  }
}

# ------ Estimating unidimensional model - Grade 8 ------ 
#Fit the GPCM model for grade 8
myGPCMmodel_grade8.22 <- mirt(grade8.22, 1, itemtype = rep("gpcm", ncol(grade8.22)), 
                              SE = TRUE, 
                              verbose = FALSE
)
print(myGPCMmodel_grade8.22)

#Print the summary of the model
summary(myGPCMmodel_grade8.22)

#Extract item parameters
coef(myGPCMmodel_grade8.22)
coef(myGPCMmodel_grade8.22, printSE = TRUE)

# ------ Estimating factor scores for the GPCM model - Grade 8 ------
factorscores_GPCM_8.22 <- fscores(myGPCMmodel_grade8.22, method = "MAP", full.scores.SE = TRUE)

# ------ Estimating M2 and itemfit for the GPCM model by subset - Grade 8 ------
#Split data by missingness patterns
grade8_missing_patterns.22 <- apply(is.na(grade8.22), 1, paste, collapse = "")
grade8_unique_patterns.22 <- unique(grade8_missing_patterns.22)

#Create subsets for each missing pattern for Grade 8
grade8_data_subsets.22 <- lapply(grade8_unique_patterns.22, function(pattern) {
  grade8.22[grade8_missing_patterns.22 == pattern, , drop = FALSE]
})

#Keep only complete columns in each subset for Grade 8
grade8_complete_subsets.22 <- lapply(grade8_data_subsets.22, function(subset) {
  subset[ , colSums(is.na(subset)) == 0, drop = FALSE]
})

#Fit Unidimensional GPCM Models for Each Subset for Grade 8
grade8_models.22 <- lapply(grade8_complete_subsets.22, function(subset) {
  if (ncol(subset) > 1) { # Ensure at least two columns to fit a model
    mirt(subset, 1, itemtype = "gpcm") # Fit GPCM model
  } else {
    NULL
  }
})

#Calculate M2 for Each Model
unidim_M2_8.22 <- lapply(grade8_models.22, function(model) {
  if (!is.null(model)) {
    M2(model)
  } else {
    NA
  }
})

#Calculate itemfit for each model
unidim_itemfit_8.22 <- lapply(grade8_models.22, function(model) {
  if (!is.null(model)) {
    itemfit(model, p.adjust = "holm")
  } else {
    NULL
  }
})

#Print results for each subset
for (i in seq_along(grade8_models.22)) {
  if (!is.null(grade8_models.22[[i]])) {
    cat("\nGrade 8 Unidimensional Subset", i, "\n")
    
    #Print M2
    cat("M² Results:\n")
    print(unidim_M2_8.22[[i]])
    
    #Print itemfit
    cat("\nItemfit Results:\n")
    print(unidim_itemfit_8.22[[i]])
    
    #Print subset info
    subset <- grade8_complete_subsets.22[[i]]
    cat("\nItems included in this subset:\n", paste(names(subset), collapse = ", "), "\n")
    
    #Count misfits
    misfit_count <- sum(unidim_itemfit_8.22[[i]]$p.S_X2 < 0.05, na.rm = TRUE)
    cat("Number of items with significant misfit:", misfit_count, "out of", nrow(unidim_itemfit_8.22[[i]]), "\n")
  }
}

# ------ Estimating bifactor model using a unidimensional irt model - Grade 5 ------ 
#Fit the bifactor model for Grade 5

#Item names for each stimulus
names(grade5.22)
names(grade5.22)[1:6] #Kraken
names(grade5.22)[7:12] #Tromsøbadet
names(grade5.22)[13:19] #Spådama i 7B
names(grade5.22)[20:25] #Skateboard
names(grade5.22)[26:32] #Farlig katt

#Fit a unidimensional model
mirtest1Dgrade5.22 <- mirt(grade5.22, 1, SE = TRUE)

#Specify unidimensional model
bifgrade5.22 <- 'F1 = 1-32'

#Specify residual factors model
#This indicates which residual factor is related to which set of items
specgrade5.22 <- c(rep(1, 6), rep(2, 6), rep(3, 7), rep(4, 6), rep(5, 7))

#Estimate bifactor model with dimension-reduction method with the bfactor() function
mirtest8Dgrade5.22 <- bfactor(grade5.22, specgrade5.22, bifgrade5.22, itemtype = "gpcm", quadpts = 25, TOL = 1e-4, technical = list(NCYCLES = 15000))
mirtest8Dgrade5.22

#Estimate bifactor model with standard errors, with starting values from the estimated model above
mirtest8Dgrade5.22SE <- bfactor(grade5.22, specgrade5.22, bifgrade5.22, itemtype = "gpcm", quadpts = 25, TOL = 1e-4, 
                                pars = mod2values(mirtest8Dgrade5.22), SE = TRUE, technical = list(NCYCLES = 15000))
mirtest8Dgrade5.22SE

#Print the summary of the models
summary(mirtest8Dgrade5.22)
summary(mirtest8Dgrade5.22SE)

#Extract item parameters from bifactor model with SEs
coef(mirtest8Dgrade5.22SE, printSE = TRUE)

# ------ Estimating factor scores for the bifactor model - Grade 5 ------
factorscores_Bif_5.22 <- fscores(mirtest8Dgrade5.22SE, method = "MAP", full.scores.SE = TRUE)

# ------ Estimating M2 and itemfit for the bifactor model by subset - Grade 5 ------
#Split data by missingness patterns
bif_missing_patterns_grade5.22 <- apply(is.na(grade5.22), 1, paste, collapse = "")
bif_unique_patterns_grade5.22 <- unique(bif_missing_patterns_grade5.22)

#Create subsets for each missing pattern
bif_data_subsets_grade5.22 <- lapply(bif_unique_patterns_grade5.22, function(pattern) {
  grade5.22[bif_missing_patterns_grade5.22 == pattern, , drop = FALSE]
})

#Keep only complete columns in each subset
bif_complete_subsets_grade5.22 <- lapply(bif_data_subsets_grade5.22, function(subset) {
  subset[, colSums(is.na(subset)) == 0, drop = FALSE]
})

# Fit bifactor models for each subset
bif_models_grade5.22 <- lapply(bif_complete_subsets_grade5.22, function(subset) {
  if (ncol(subset) > 1) {
    # Get the spec factors for the columns present in this subset
    col_indices <- match(names(subset), names(grade5.22))
    subset_specs <- specgrade5.22[col_indices]
    bif_spec <- paste0('F1 = 1-', ncol(subset))
    bfactor(subset, 
            subset_specs,
            bif_spec,
            itemtype = "gpcm",
            quadpts = 25,
            TOL = 1e-4,
            technical = list(NCYCLES = 15000))
  } else {
    NULL
  }
})

#Calculate M2 for Each Model
bif_M2_5.22 <- lapply(bif_models_grade5.22, function(model) {
  if (!is.null(model)) {
    M2(model, QMC = TRUE, quadpts = 20000)
  } else {
    NA
  }
})

#Calculate itemfit for each model
bif_itemfit_5.22 <- lapply(bif_models_grade5.22, function(model) {
  if (!is.null(model)) {
    fit <- itemfit(model, p.adjust = "holm", QMC = TRUE, quadpts = 20000)
    return(fit)
  } else {
    return(NULL)
  }
})

#Print results for each subset
for (i in seq_along(bif_models_grade5.22)) {
  model <- bif_models_grade5.22[[i]]
  
  if (!is.null(model)) {
    cat("\nGrade 5 Bifactor Subset", i, "\n")
    
    #Print M2
    cat("M² Results:\n")
    print(bif_M2_5.22[[i]])
    
    #Print itemfit
    cat("\nItemfit Results:\n")
    print(bif_itemfit_5.22[[i]])
    
    #Subset info
    subset <- bif_complete_subsets_grade5.22[[i]]
    col_indices <- match(names(subset), names(grade5.22))
    subset_specs <- specgrade5.22[col_indices]
    
    cat("\nItems included in this subset:\n", paste(names(subset), collapse = ", "), "\n")
    cat("Specific factors used:\n", paste(subset_specs, collapse = ", "), "\n")
    
    #Count significant misfits
    misfit_count <- sum(bif_itemfit_5.22[[i]]$p.S_X2 < 0.05, na.rm = TRUE)
    cat("Number of items with significant misfit:", misfit_count, "out of", nrow(bif_itemfit_5.22[[i]]), "\n")
  }
}

# ------ Estimating bifactor model using a unidimensional irt model - Grade 8 ------ 
#Fit the bifactor model for Grade 8

#Item names for each stimulus
names(grade8.22)
names(grade8.22)[1:6] #Verdens kaldeste sted
names(grade8.22)[7:13] #Hull og sønn
names(grade8.22)[14:19] #iHuman
names(grade8.22)[20:25] #Havet
names(grade8.22)[26:31] #Challengerdypet
names(grade8.22)[32:37] #Knøttfe og himmelreker
names(grade8.22)[38:42] #Arne og Jakob

#Fit a unidimensional model
mirtest1Dgrade8.22 <- mirt(grade8.22, 1, SE = TRUE)

#Specify unidimensional model
bifgrade8.22 <- 'F1 = 1-42'

#Specify residual factors model
#This indicates which residual factor is related to which set of items
specgrade8.22 <- c(rep(1, 6), rep(2, 7), rep(3, 6), rep(4, 6), rep(5, 6), rep(6, 6), rep(7, 5))

#Estimate bifactor model with dimension-reduction method with the bfactor() function
mirtest8Dgrade8.22 <- bfactor(grade8.22, specgrade8.22, bifgrade8.22, itemtype = "gpcm", quadpts = 25, TOL = 1e-4, technical = list(NCYCLES = 15000))
mirtest8Dgrade8.22

#Estimate bifactor model with standard errors, with starting values from the estimated model above
mirtest8Dgrade8.22SE <- bfactor(grade8.22, specgrade8.22, bifgrade8.22, itemtype = "gpcm", quadpts = 25, TOL = 1e-4, 
                                pars = mod2values(mirtest8Dgrade8.22), SE = TRUE, technical = list(NCYCLES = 15000))
mirtest8Dgrade8.22SE

#Print the summary of the models
summary(mirtest8Dgrade8.22)
summary(mirtest8Dgrade8.22SE)

#Extract item parameters
coef(mirtest8Dgrade8.22SE, printSE = TRUE)

# ------ Estimating factor scores for the bifactor model - Grade 8 ------
factorscores_Bif_8.22 <- fscores(mirtest8Dgrade8.22SE, method = "MAP", full.scores.SE = TRUE)

# ------ Estimating M2 and itemfit for the bifactor model by subset - Grade 8 ------
# Split data by missingness patterns
bif_missing_patterns_grade8.22 <- apply(is.na(grade8.22), 1, paste, collapse = "")
bif_unique_patterns_grade8.22 <- unique(bif_missing_patterns_grade8.22)

# Create subsets for each missing pattern
bif_data_subsets_grade8.22 <- lapply(bif_unique_patterns_grade8.22, function(pattern) {
  grade8.22[bif_missing_patterns_grade8.22 == pattern, , drop = FALSE]
})

# Keep only complete columns in each subset
bif_complete_subsets_grade8.22 <- lapply(bif_data_subsets_grade8.22, function(subset) {
  subset[ , colSums(is.na(subset)) == 0, drop = FALSE]
})

#Fit bifactor models for each subset
bif_models_grade8.22 <- lapply(bif_complete_subsets_grade8.22, function(subset) {
  if (ncol(subset) > 1) { 
    #Get the spec factors for the columns present in this subset
    col_indices <- match(names(subset), names(grade8.22))
    subset_specs <- specgrade8.22[col_indices]
    bif_spec <- paste0('F1 = 1-', ncol(subset))
    bfactor(subset,
            subset_specs,
            bif_spec,
            itemtype = "gpcm",
            quadpts = 25,
            TOL = 1e-4, 
            technical = list(NCYCLES = 15000))
  } else {
    NULL
  }
})

#Calculate M² for each model
bif_M2_8.22 <- lapply(bif_models_grade8.22, function(model) {
  if (!is.null(model)) {
    M2(model, QMC = TRUE, quadpts = 20000)
  } else {
    NA
  }
})

#Calculate itemfit for each model
bif_itemfit_8.22 <- lapply(bif_models_grade8.22, function(model) {
  if (!is.null(model)) {
    itemfit(model, p.adjust = "holm", QMC = TRUE, quadpts = 20000)
  } else {
    NULL
  }
})

#Print results for each subset
for (i in seq_along(bif_models_grade8.22)) {
  model <- bif_models_grade8.22[[i]]
  
  if (!is.null(model)) {
    cat("\nGrade 8 Bifactor Subset", i, "\n")
    
    #Print M2
    cat("M² Results:\n")
    print(bif_M2_8.22[[i]])
    
    #Print itemfit
    cat("\nItemfit Results:\n")
    print(bif_itemfit_8.22[[i]])
    
    #Subset info
    subset <- bif_complete_subsets_grade8.22[[i]]
    col_indices <- match(names(subset), names(grade8.22))
    subset_specs <- specgrade8.22[col_indices]
    
    cat("\nItems included in this subset:\n", paste(names(subset), collapse = ", "), "\n")
    cat("Specific factors used:\n", paste(subset_specs, collapse = ", "), "\n")
    
    #Count significant misfits
    misfit_count <- sum(bif_itemfit_8.22[[i]]$p.S_X2 < 0.05, na.rm = TRUE)
    cat("Number of items with significant misfit:", misfit_count, "out of", nrow(bif_itemfit_8.22[[i]]), "\n")
  }
}

# ------ Classification into mastery level for 5th grade - from factor scores ------ 
#Compute the quantile cutoffs for classification from the unidimensional model - Grade 5
quantiles_1D_5.22 <- quantile(factorscores_GPCM_5.22[, "F1"], probs = c(0.25, 0.75), na.rm = TRUE)

#Define mastery levels based on MAP scores from the unidimensional model - Grade 5
mastery_levels_1D_5.22 <- cut(factorscores_GPCM_5.22 [, "F1"], 
                              breaks = c(-Inf, quantiles_1D_5.22[1], quantiles_1D_5.22[2], Inf), 
                              labels = c("Mastery Level 1", "Mastery Level 2", "Mastery Level 3"), 
                              include.lowest = TRUE)

#Compute the quantile cutoffs for classification from the bifactor model - Grade 5
quantiles_8D_5.22 <- quantile(factorscores_Bif_5.22[, "F1"], probs = c(0.25, 0.75), na.rm = TRUE)

#Define mastery levels based on MAP scores from the bifactor model - Grade 5
mastery_levels_8D_5.22 <- cut(factorscores_Bif_5.22[, "F1"], 
                              breaks = c(-Inf, quantiles_8D_5.22[1], quantiles_8D_5.22[2], Inf), 
                              labels = c("Mastery Level 1", "Mastery Level 2", "Mastery Level 3"), 
                              include.lowest = TRUE)

#Create classified dataset with both models - Grade 5
classified_students_5.22 <- data.frame(Student_ID = 1:length(factorscores_GPCM_5.22), 
                                       MAP_Score_1D = factorscores_GPCM_5.22, 
                                       Mastery_Level_1D = mastery_levels_1D_5.22,
                                       MAP_Score_8D = factorscores_Bif_5.22[,1], 
                                       Mastery_Level_8D = mastery_levels_8D_5.22)

#Calculate the proportion of students classified the same way by both models - Grade 5
same_classification_5.22 <- sum(classified_students_5.22$Mastery_Level_1D == classified_students_5.22$Mastery_Level_8D, na.rm = TRUE) / nrow(classified_students_5.22)

#Print the proportion of students classified the same way - Grade 5
cat("Proportion of students classified the same way by both models (Grade 5):", same_classification_5.22, "\n")

#Print the first few rows of the classified dataset - Grade 5
head(classified_students_5.22)

# ------ Classification into mastery level for 8th grade - from factor scores ------ 
#Compute the quantile cutoffs for classification from the unidimensional model - Grade 8
quantiles_1D_8.22 <- quantile(factorscores_GPCM_8.22[, "F1"], probs = c(0.1, 0.3, 0.7, 0.9), na.rm = TRUE)

#Define mastery levels based on MAP scores from the unidimensional model - Grade 8
mastery_levels_1D_8.22 <- cut(factorscores_GPCM_8.22[, "F1"], 
                              breaks = c(-Inf, quantiles_1D_8.22[1], quantiles_1D_8.22[2], quantiles_1D_8.22[3], quantiles_1D_8.22[4], Inf), 
                              labels = c("Mastery Level 1", "Mastery Level 2", "Mastery Level 3", "Mastery Level 4", "Mastery Level 5"), 
                              include.lowest = TRUE)

#Compute the quantile cutoffs for classification from the bifactor model - Grade 8
quantiles_8D_8.22 <- quantile(factorscores_Bif_8.22[, "F1"], probs = c(0.1, 0.3, 0.7, 0.9), na.rm = TRUE)

#Define mastery levels based on MAP scores from the bifactor model - Grade 8
mastery_levels_8D_8.22 <- cut(factorscores_Bif_8.22[, "F1"], 
                              breaks = c(-Inf, quantiles_8D_8.22[1], quantiles_8D_8.22[2], quantiles_8D_8.22[3], quantiles_8D_8.22[4], Inf), 
                              labels = c("Mastery Level 1", "Mastery Level 2", "Mastery Level 3", "Mastery Level 4", "Mastery Level 5"), 
                              include.lowest = TRUE)

#Create classified dataset with both models - Grade 8
classified_students_8.22 <- data.frame(Student_ID = 1:length(factorscores_GPCM_8.22), 
                                       MAP_Score_1D = factorscores_GPCM_8.22, 
                                       Mastery_Level_1D = mastery_levels_1D_8.22,
                                       MAP_Score_8D = factorscores_Bif_8.22[,1], 
                                       Mastery_Level_8D = mastery_levels_8D_8.22)

#Calculate the proportion of students classified the same way by both models - Grade 8
same_classification_8.22 <- sum(classified_students_8.22$Mastery_Level_1D == classified_students_8.22$Mastery_Level_8D, na.rm = TRUE) / nrow(classified_students_8.22)

#Print the proportion of students classified the same way - Grade 8
cat("Proportion of students classified the same way by both models (Grade 8):", same_classification_8.22, "\n")

#Print the first few rows of the classified dataset - Grade 8
head(classified_students_8.22)

#Comparing the nested models using anova
anova(myGPCMmodel_grade5.22, mirtest8Dgrade5.22SE)
anova(myGPCMmodel_grade8.22, mirtest8Dgrade8.22SE)

# ------ Caclulating the Explained Common Variance ------

#Grade 5
#Extract loadings using the factor specifications from your model
#Grade 5 has 1 general factor (F1) and 5 specific factors
loadings_5_22 <- extract.mirt(mirtest8Dgrade5.22SE, 'F')

#First column is the general factor
lambda_G_5_22 <- loadings_5_22[, 1]^2

#Remaining columns are specific factors
lambda_S_5_22 <- rowSums(loadings_5_22[, -1, drop=FALSE]^2)

#Calculate ECV - Grade 5
ECV_5_22 <- sum(lambda_G_5_22) / (sum(lambda_G_5_22) + sum(lambda_S_5_22))
cat("ECV for Grade 5 (2022):", round(ECV_5_22, 3), "\n")

#Grade 8
#Grade 8 has 1 general factor (F1) and 7 specific factors
loadings_8_22 <- extract.mirt(mirtest8Dgrade8.22SE, 'F')

#First column is the general factor
lambda_G_8_22 <- loadings_8_22[, 1]^2

#Remaining columns are specific factors
lambda_S_8_22 <- rowSums(loadings_8_22[, -1, drop=FALSE]^2)

#Calculate ECV - Grade 8
ECV_8_22 <- sum(lambda_G_8_22) / (sum(lambda_G_8_22) + sum(lambda_S_8_22))
cat("ECV for Grade 8 (2022):", round(ECV_8_22, 3), "\n")

# ------ Estimating standard errors of factor scores ------

#Unidimensional
SE_uni_5_22 <- factorscores_GPCM_5.22[, "SE_F1"]
SE_uni_8_22 <- factorscores_GPCM_8.22[, "SE_F1"]

#Bifactor (general factor only)
SE_bi_5_22 <- factorscores_Bif_5.22[, "SE_F1"]
SE_bi_8_22 <- factorscores_Bif_8.22[, "SE_F1"]

#Compute Average Standard Errors
avg_SE_uni_5_22 <- mean(SE_uni_5_22, na.rm = TRUE)
avg_SE_bi_5_22  <- mean(SE_bi_5_22, na.rm = TRUE)

avg_SE_uni_8_22 <- mean(SE_uni_8_22, na.rm = TRUE)
avg_SE_bi_8_22  <- mean(SE_bi_8_22, na.rm = TRUE)

#Create Summary Table
SE_comparison_2022 <- data.frame(
  Grade = c(5, 8),
  Year = 2022,
  SE_Unidimensional = c(avg_SE_uni_5_22, avg_SE_uni_8_22),
  SE_Bifactor_General = c(avg_SE_bi_5_22, avg_SE_bi_8_22)
)

print(SE_comparison_2022)

