##/-------------------------------------------------------------------\##
## explainable flood damage modeling functions based on 
## local interpretable model-agnostic explanations (LIME)
##/-------------------------------------------------------------------\##
rm(list=ls())

library(readxl)
library(reshape2)
library(caret) # for ML models
library(xgboost) # for xbgTree
library(plyr) # for xbgTree
library(e1071) # for support vector regression
library(kernlab) # for vector regression
library(brnn) # for bayesian neural network
library(viridis) # for color plotting
library(patchwork) # for combining plots consistently
library(lime) # for LIME
library(dplyr) # for computations

##/ directory
mainDir <- 'D:/PC_Cornell/Data/DFO_floods_damages'
setwd(mainDir)

#------------------------------------------------
#--   EMDAT data: 1985 - 2021   -- #
#------------------------------------------------
lst_EMDAT <- readRDS('./Data/processed.data.files/lst_EMDAT.data_training.testing.rds')

df_shuffled <- lst_EMDAT$df_shuffled
df_shuffled.log <-lst_EMDAT$df_shuffled.log
# dim(df_shuffled.log)
# [1] 653   4

x <- colnames(df_shuffled.log)[-1]
y <- colnames(df_shuffled.log)[1]



#------------------------------------------------
## Linear model fit ##
lm_fit = train(damage ~ ., data=df_shuffled.log,
               method='lm',
               trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, lm_fit)
# Explain new observation
lm_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
lm_explanation <- lm_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(lm_explanation[1:9,], ncol=1)

ggplot(lm_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "EMDAT",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## Random forest fit ##
rf_fit = train(damage ~ ., data=df_shuffled.log,
               method='rf',
               importance=TRUE,
               trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, rf_fit)
# Explain new observation
rf_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
rf_explanation <- rf_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(rf_explanation[1:9,], ncol=1)

ggplot(rf_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "EMDAT",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## XGBoost fit ##
xgb_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='xgbTree',
                 trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, xgb_fit)
# Explain new observation
xgb_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
xgb_explanation <- xgb_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(xgb_explanation[1:9,], ncol=1)

ggplot(xgb_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "EMDAT",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## SVR fit ##
svr_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='svmRadial',
                 trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, svr_fit)
# Explain new observation
svr_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
svr_explanation <- svr_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(svr_explanation[1:9,], ncol=1)

ggplot(svr_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "EMDAT",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## BRNN fit ##
brnn_fit <- train(damage ~ ., data=df_shuffled.log,
                  method='brnn',
                  trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, brnn_fit)
# Explain new observation
brnn_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
brnn_explanation <- brnn_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(brnn_explanation[1:9,], ncol=1)

ggplot(brnn_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "EMDAT",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## KNN fit ##
knn_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='knn',
                 trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, knn_fit)
# Explain new observation
knn_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
knn_explanation <- knn_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(knn_explanation[1:9,], ncol=1)

ggplot(knn_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "EMDAT",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability



#------------------------------------------------
## all fits (together) ##
all.lime_emdat <- list(LM = lm_explanation,
                     RF = rf_explanation,
                     XGBoost = xgb_explanation,
                     SVR = svr_explanation,
                     BRNN = brnn_explanation,
                     KNN = knn_explanation)

# All plots
# LIME plots
create_boxplot <- function(data, title) {
  ggplot(data, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
    geom_boxplot(fill = "grey", color = "black", outlier.color = "black", outlier.shape = 16, width = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "orange") +
    geom_jitter(aes(color = feature),  # each feature can have a unique color if desired
                width = 0.2, size = 1.5, alpha = 0.2, show.legend = FALSE) +
    coord_flip() +
    labs(title = title, x = "Feature", y = "mean (LIME feature weight)") +
    theme_minimal() +
    theme(axis.text = element_text(size = 14), plot.title = element_text(size = 18))
}

combined_plot <- (create_boxplot(all.lime_emdat[[1]], names(all.lime_emdat)[1]) |
                    create_boxplot(all.lime_emdat[[2]], names(all.lime_emdat)[2]) |
                    create_boxplot(all.lime_emdat[[3]], names(all.lime_emdat)[3])) /
  (create_boxplot(all.lime_emdat[[4]], names(all.lime_emdat)[4]) |
     create_boxplot(all.lime_emdat[[5]], names(all.lime_emdat)[5]) |
     create_boxplot(all.lime_emdat[[6]], names(all.lime_emdat)[6]))

ggsave(filename = "./Figures/models.EMDAT.LIME_analysis_weights.svg", 
       plot = combined_plot, 
       width = 16,
       height = 8, 
       dpi = 300)


# Individual plot
for (model_name in names(all.lime_emdat)) {
  lime_data <- all.lime_emdat[[model_name]]
  p <- create_boxplot(lime_data, title = model_name)
  
  ggsave(filename = paste0("./Figures/LIME_EMDAT_", model_name, ".svg"),
         plot = p,
         width = 7,
         height = 5,
         dpi = 300)
}

##




#------------------------------------------------
#--   DFO15 data: 1985 - 2015   -- #
#------------------------------------------------
lst_DFO15 <- readRDS('./Data/processed.data.files/lst_DFO15.data_training.testing.rds')

df_shuffled <- lst_DFO15$df_shuffled
df_shuffled.log <-lst_DFO15$df_shuffled.log
# dim(df_shuffled.log)
# [1] 653   4

x <- colnames(df_shuffled.log)[-1]
y <- colnames(df_shuffled.log)[1]



#------------------------------------------------
## Linear model fit ##
lm_fit = train(damage ~ ., data=df_shuffled.log,
               method='lm',
               trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, lm_fit)
# Explain new observation
lm_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
lm_explanation <- lm_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(lm_explanation[1:9,], ncol=1)

ggplot(lm_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "DFO15",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## Random forest fit ##
rf_fit = train(damage ~ ., data=df_shuffled.log,
               method='rf',
               importance=TRUE,
               trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, rf_fit)
# Explain new observation
rf_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
rf_explanation <- rf_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(rf_explanation[1:9,], ncol=1)

ggplot(rf_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "DFO15",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## XGBoost fit ##
xgb_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='xgbTree',
                 trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, xgb_fit)
# Explain new observation
xgb_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
xgb_explanation <- xgb_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(xgb_explanation[1:9,], ncol=1)

ggplot(xgb_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "DFO15",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## SVR fit ##
svr_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='svmRadial',
                 trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, svr_fit)
# Explain new observation
svr_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
svr_explanation <- svr_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(svr_explanation[1:9,], ncol=1)

ggplot(svr_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "DFO15",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## BRNN fit ##
brnn_fit <- train(damage ~ ., data=df_shuffled.log,
                  method='brnn',
                  trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, brnn_fit)
# Explain new observation
brnn_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
brnn_explanation <- brnn_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(brnn_explanation[1:9,], ncol=1)

ggplot(brnn_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "DFO15",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## KNN fit ##
knn_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='knn',
                 trControl=trainControl(method='cv',number=10))
# LIME analysis
explainer <- lime(df_shuffled.log, knn_fit)
# Explain new observation
knn_explanation <- lime::explain(df_shuffled.log[,-1],explainer,n_features=3)
knn_explanation <- knn_explanation %>%
  mutate(feature = as.factor(feature))

# LIME plot
plot_features(knn_explanation[1:9,], ncol=1)

ggplot(knn_explanation, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
  geom_boxplot(fill = "slateblue1", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add horizontal line at zero
  coord_flip() +
  labs(
    title = "DFO15",
    x = "Feature",
    y = "mean (LIME feature weight)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)) # Adjust label size for better readability


#------------------------------------------------
## all fits (together) ##
all.lime_dfo <- list(LM = lm_explanation,
                       RF = rf_explanation,
                       XGBoost = xgb_explanation,
                       SVR = svr_explanation,
                       BRNN = brnn_explanation,
                       KNN = knn_explanation)

# All plots
# LIME plots
create_boxplot <- function(data, title) {
  ggplot(data, aes(x = reorder(feature, feature_weight, FUN = median), y = feature_weight)) +
    geom_boxplot(fill = "grey", color = "black", outlier.color = "black", outlier.shape = 16, width = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "orange") +
    geom_jitter(aes(color = feature),  # each feature can have a unique color if desired
                width = 0.2, size = 1.5, alpha = 0.2, show.legend = FALSE) +
    coord_flip() +
    labs(title = title, x = "Feature", y = "mean (LIME feature weight)") +
    theme_minimal() +
    theme(axis.text = element_text(size = 14), plot.title = element_text(size = 18))
}

combined_plot <- (create_boxplot(all.lime_dfo[[1]], names(all.lime_dfo)[1]) |
                    create_boxplot(all.lime_dfo[[2]], names(all.lime_dfo)[2]) |
                    create_boxplot(all.lime_dfo[[3]], names(all.lime_dfo)[3])) /
  (create_boxplot(all.lime_dfo[[4]], names(all.lime_dfo)[4]) |
     create_boxplot(all.lime_dfo[[5]], names(all.lime_dfo)[5]) |
     create_boxplot(all.lime_dfo[[6]], names(all.lime_dfo)[6]))

ggsave(filename = "./Figures/models.DFO.LIME_analysis_weights.svg", 
       plot = combined_plot, 
       width = 16,
       height = 8, 
       dpi = 300)


# Individual plot
for (model_name in names(all.lime_dfo)) {
  lime_data <- all.lime_dfo[[model_name]]
  p <- create_boxplot(lime_data, title = model_name)
  
  ggsave(filename = paste0("./Figures/LIME_DFO_", model_name, ".svg"),
         plot = p,
         width = 7,
         height = 5,
         dpi = 300)
}


## The end. #



