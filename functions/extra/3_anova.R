library(car)
library(dplyr)


mainDir <- 'D:/PC_Cornell/Data/DFO_floods_damages'
setwd(mainDir)

#//////////////////////////////#
# Output Datasets Review #
#//////////////////////////////#


### -------------------------------------------------------------
### -------------------------------------------------------------
### -------------------------------------------------------------
### -------------------------------------------------------------

names_models <- c('LM','RF','XGBoost','SVR','BRNN','KNN')
names_cols <- paste(c('lm','rf','xgb','svr','brnn','knn'),'USD',sep='_')

## All ML outputs ##
lst.DFO21.EMDAT21.out <- readRDS('./Data/output.data.files/ML.DFO21.EMDAT21.damages.predictions_20250507.rds')

## DFO'21 wt EMDAT'21 ##
DFO21_damages_wt_EMDAT21 <- lst.DFO21.EMDAT21.out$DFO21_damages_wt_EMDAT21
names_models <- c('LM','RF','XGBoost','SVR','BRNN','KNN')



#-----------------------------------
## TYPE 1 ANOVA
#-----------------------------------


# --- Targets (responses) and predictors ---------------------------------------
# Six USD estimate columns: treated as the response y in separate models
usd_vars <- c("lm_USD", "rf_USD", "xgb_USD", "svr_USD", "brnn_USD", "knn_USD")

# Predictors (x): numeric + categorical
# duration, area, deaths, gdp are numeric; MainCause and country are categorical (factors)
predictors <- c("duration", "area", "deaths", "gdp", "MainCause", "country")

# --- Ensure correct data types -------------------------------------------------
# MainCause and country should be factors for ANOVA (categorical effects with multiple levels)
df <- DFO21_damages_wt_EMDAT21 %>%
  mutate(
    MainCause = as.factor(MainCause),
    country   = as.factor(country)
  )

# --- Main loop: one ANOVA per USD estimate ------------------------------------
anova_fractions <- lapply(usd_vars, function(usd_var) {
  # Build model formula like: lm_USD ~ duration + area + deaths + gdp + MainCause + country
  form <- as.formula(paste(usd_var, "~", paste(predictors, collapse = " + ")))
  
  # Fit ANOVA model (equivalent to linear model + ANOVA table)
  # na.action = na.omit removes rows with NAs in any model variable
  fit  <- aov(form, data = df, na.action = na.omit)
  
  # Extract the ANOVA table (Type I SS by default here)
  aov_tab <- summary(fit)[[1]]
  # Columns usually: Df | Sum Sq | Mean Sq | F value | Pr(>F)
  # Sum Sq(term) = sum of squares attributable to that term (its contribution to reducing SSE)
  
  # Indices: all rows except the final "Residuals" row are the model terms
  term_rows  <- 1:(nrow(aov_tab) - 1)
  term_names <- rownames(aov_tab)[term_rows]
  
  # --- Sums of Squares (SS) ----------------------------------------------------
  # SS_term = Sum Sq for each predictor (per row)
  term_ss   <- aov_tab[term_rows, "Sum Sq"]
  # SS_res  = residual (unexplained) sum of squares
  ss_resid  <- aov_tab["Residuals", "Sum Sq"]
  
  # SS_model = Σ SS_term (explained variance by all predictors)
  # SS_total = SS_model + SS_resid (total variance around the mean)
  ss_model  <- sum(term_ss, na.rm = TRUE)
  ss_total  <- ss_model + ss_resid
  
  # --- Fractions and R² --------------------------------------------------------
  # Fraction of EXPLAINED variance (a.k.a. share of the model SS):
  # Frac_Model(term_i) = SS_term_i / Σ SS_term
  # -> sums to 1 (100%) across all predictors for a given response
  frac_model <- term_ss / ss_model
  
  # Fraction of TOTAL variance (including residuals):
  # Frac_Total(term_i) = SS_term_i / SS_total
  # -> sums to R² across all predictors
  frac_total <- term_ss / ss_total
  
  # Total R² (explained / total)
  # R² = SS_model / SS_total
  r2 <- ss_model / ss_total
  
  # Pack results for this USD response
  data.frame(
    USD_Type     = usd_var,
    Variable     = term_names,
    # Express as percentages for readability
    Frac_Model   = frac_model * 100,  # % of explained variance (sums to ~100)
    Frac_Total   = frac_total * 100,  # % of total variance (sums to ~R²)
    Total_R2_pct = r2 * 100           # overall % explained by ALL predictors
  )
})

# Combine across the six USD responses
anova_fraction_summary <- bind_rows(anova_fractions)

# Example view: rank predictors within each USD type by their share of explained variance
anova_fraction_summary %>%
  arrange(USD_Type, desc(Frac_Model))



#--------------------------------------------
## plot as bars ##
# Ensure strings are characters and trimmed
anova_fraction_summary$Variable <- trimws(as.character(anova_fraction_summary$Variable))
anova_fraction_summary$USD_Type <- trimws(as.character(anova_fraction_summary$USD_Type))

# Keep only predictors
anova_plot_df <- subset(anova_fraction_summary, Variable != "Residuals")

# Predictors in fixed order
predictors <- c("duration", "area", "deaths", "gdp", "MainCause", "country")

# Models and their display names
usd_models <- unique(anova_plot_df$USD_Type)
names_models <- c('LM','RF','XGBoost','SVR','BRNN','KNN')  # custom names
names_map <- setNames(names_models, usd_models)

# Colors
bar_cols <- c("skyblue", "lightgreen", "orange", "tomato", "orchid", "gold")

# SVG output with larger labels
svg("./Figures/anova_frac_model_baseR_large.svg", width = 14, height = 9)  # bigger canvas
par(mfrow = c(2, 3), mar = c(7, 6, 5, 2), cex.axis = 1.75, cex.lab = 2, cex.main = 2.5)

for (mod in usd_models) {
  df_mod <- subset(anova_plot_df, USD_Type == mod)
  
  # Ensure predictors appear in fixed order; fill missing with 0
  vals <- sapply(predictors, function(p) {
    idx <- match(p, df_mod$Variable)
    if (!is.na(idx)) df_mod$Frac_Model[idx] else 0
  })
  
  # Bar plot with larger text
  bp <- barplot(vals,
                names.arg = predictors,
                col = bar_cols,
                ylim = c(0, max(anova_plot_df$Frac_Model) * 1.2),
                las = 2,
                main = paste0(names_map[mod], " (R² = ", 
                              sprintf("%.1f", unique(df_mod$Total_R2_pct)), "%)"),
                ylab = "% of Model R² Explained",
                cex.names = 1.4)
  
  # Add % labels above bars
  text(bp, vals, labels = sprintf("%.1f", vals), pos = 3, cex = 1.75, font = 2)
}

dev.off()





##---------------------------##
## IF WE DIVIDE DAMAGE BY GDP ##
##---------------------------##

DFO21_damages_wt_EMDAT21[paste0(names_cols, "_perGDP")] <-
  log(DFO21_damages_wt_EMDAT21[names_cols] / DFO21_damages_wt_EMDAT21$gdp)


# --- Targets (responses) and predictors ---------------------------------------
# Six USD estimate columns: treated as the response y in separate models
# usd_vars <- c("lm_USD", "rf_USD", "xgb_USD", "svr_USD", "brnn_USD", "knn_USD")
usd_vars <- paste0(names_cols, "_perGDP")
# Predictors (x): numeric + categorical
# duration, area, deaths, gdp are numeric; MainCause and country are categorical (factors)
predictors <- c("duration", "area", "deaths", "MainCause", "country")

# --- Ensure correct data types -------------------------------------------------
# MainCause and country should be factors for ANOVA (categorical effects with multiple levels)
df <- DFO21_damages_wt_EMDAT21 %>%
  mutate(
    MainCause = as.factor(MainCause),
    country   = as.factor(country)
  )

# --- Main loop: one ANOVA per USD estimate ------------------------------------
anova_fractions <- lapply(usd_vars, function(usd_var) {
  # Build model formula like: lm_USD ~ duration + area + deaths + gdp + MainCause + country
  form <- as.formula(paste(usd_var, "~", paste(predictors, collapse = " + ")))
  
  # Fit ANOVA model (equivalent to linear model + ANOVA table)
  # na.action = na.omit removes rows with NAs in any model variable
  fit  <- aov(form, data = df, na.action = na.omit)
  
  # Extract the ANOVA table (Type I SS by default here)
  aov_tab <- summary(fit)[[1]]
  # Columns usually: Df | Sum Sq | Mean Sq | F value | Pr(>F)
  # Sum Sq(term) = sum of squares attributable to that term (its contribution to reducing SSE)
  
  # Indices: all rows except the final "Residuals" row are the model terms
  term_rows  <- 1:(nrow(aov_tab) - 1)
  term_names <- rownames(aov_tab)[term_rows]
  
  # --- Sums of Squares (SS) ----------------------------------------------------
  # SS_term = Sum Sq for each predictor (per row)
  term_ss   <- aov_tab[term_rows, "Sum Sq"]
  # SS_res  = residual (unexplained) sum of squares
  ss_resid  <- aov_tab["Residuals", "Sum Sq"]
  
  # SS_model = Σ SS_term (explained variance by all predictors)
  # SS_total = SS_model + SS_resid (total variance around the mean)
  ss_model  <- sum(term_ss, na.rm = TRUE)
  ss_total  <- ss_model + ss_resid
  
  # --- Fractions and R² --------------------------------------------------------
  # Fraction of EXPLAINED variance (a.k.a. share of the model SS):
  # Frac_Model(term_i) = SS_term_i / Σ SS_term
  # -> sums to 1 (100%) across all predictors for a given response
  frac_model <- term_ss / ss_model
  
  # Fraction of TOTAL variance (including residuals):
  # Frac_Total(term_i) = SS_term_i / SS_total
  # -> sums to R² across all predictors
  frac_total <- term_ss / ss_total
  
  # Total R² (explained / total)
  # R² = SS_model / SS_total
  r2 <- ss_model / ss_total
  
  # Pack results for this USD response
  data.frame(
    USD_Type     = usd_var,
    Variable     = term_names,
    # Express as percentages for readability
    Frac_Model   = frac_model * 100,  # % of explained variance (sums to ~100)
    Frac_Total   = frac_total * 100,  # % of total variance (sums to ~R²)
    Total_R2_pct = r2 * 100           # overall % explained by ALL predictors
  )
})

# Combine across the six USD responses
anova_fraction_summary <- bind_rows(anova_fractions)

# Example view: rank predictors within each USD type by their share of explained variance
anova_fraction_summary %>%
  arrange(USD_Type, desc(Frac_Model))



#--------------------------------------------
## plot as bars ##
# Ensure strings are characters and trimmed
anova_fraction_summary$Variable <- trimws(as.character(anova_fraction_summary$Variable))
anova_fraction_summary$USD_Type <- trimws(as.character(anova_fraction_summary$USD_Type))

# Keep only predictors
anova_plot_df <- subset(anova_fraction_summary, Variable != "Residuals")

# Predictors in fixed order
predictors <- c("duration", "area", "deaths", "MainCause", "country")

# Models and their display names
usd_models <- unique(anova_plot_df$USD_Type)
names_models <- c('LM','RF','XGBoost','SVR','BRNN','KNN')  # custom names
names_map <- setNames(names_models, usd_models)

# Colors
bar_cols <- c("skyblue", "lightgreen", "orange", "orchid", "gold")

# SVG output with larger labels
svg("./Figures/anova_frac_model_baseR_large_perGDP.svg", width = 14, height = 9)  # bigger canvas
par(mfrow = c(2, 3), mar = c(7, 6, 5, 2), cex.axis = 1.75, cex.lab = 2, cex.main = 2.5)

for (mod in usd_models) {
  df_mod <- subset(anova_plot_df, USD_Type == mod)
  
  # Ensure predictors appear in fixed order; fill missing with 0
  vals <- sapply(predictors, function(p) {
    idx <- match(p, df_mod$Variable)
    if (!is.na(idx)) df_mod$Frac_Model[idx] else 0
  })
  
  # Bar plot with larger text
  bp <- barplot(vals,
                names.arg = predictors,
                col = bar_cols,
                ylim = c(0, max(anova_plot_df$Frac_Model) * 1.2),
                las = 2,
                main = paste0(names_map[mod], " (R² = ", 
                              sprintf("%.1f", unique(df_mod$Total_R2_pct)), "%)"),
                ylab = "% of Model R² Explained",
                cex.names = 1.4)
  
  # Add % labels above bars
  text(bp, vals, labels = sprintf("%.1f", vals), pos = 3, cex = 1.75, font = 2)
}

dev.off()







#-----------------------------------
## PERT ##
## Type 2 ANOVA

library(car)
library(dplyr)
# Optional: library(car)  # uncomment if you want Type II ANOVA

anova_fractions_type2 <- lapply(usd_vars, function(usd_var) {
  form <- as.formula(paste(usd_var, "~", paste(predictors, collapse = " + ")))
  fit  <- lm(form, data = df, na.action = na.omit)
  
  # Type II ANOVA gives SS for each term adjusted for others (order-independent)
  a2 <- car::Anova(fit, type = 2)
  # Residual SS from base anova (same either way)
  a1 <- anova(fit)
  
  ss_col <- if ("Sum Sq" %in% colnames(a2)) "Sum Sq" else if ("SS" %in% colnames(a2)) "SS" else stop("No SS column in Anova table.")
  term_ss   <- a2[[ss_col]]
  term_names<- rownames(a2)
  ss_resid  <- tail(a1[["Sum Sq"]], 1)
  
  ss_model <- sum(term_ss, na.rm = TRUE)
  ss_total <- ss_model + ss_resid
  r2 <- ss_model / ss_total
  
  data.frame(
    USD_Type     = usd_var,
    Variable     = term_names,
    Frac_Model   = 100 * term_ss / ss_model,
    Frac_Total   = 100 * term_ss / ss_total,
    Total_R2_pct = 100 * r2
  )
})

anova_fraction_summary_type2 <- bind_rows(anova_fractions_type2)
