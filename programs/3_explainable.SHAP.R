##/-------------------------------------------------------------------\##
## explainable flood damage modeling functions based on 
## SHAP (SHapley Additive exPlanations)##
##/-------------------------------------------------------------------\##
rm(list=ls())

library(readxl)
library(reshape2)
library(psych) # for scatter smooth plots
library(caret) # for ML models
library(xgboost) # for xbgTree
library(plyr) # for xbgTree
library(e1071) # for support vector regression
library(kernlab) # for vector regression
library(brnn) # for bayesian neural network
library(iml) # for SHAP for random forest
library(shapviz) # for visualization of SHAP values
library(ranger) # for fast implementation of random forest
library(treeshap) # for SHAP for random forest
library(kernelshap) # for SHAP for linear model in 'train' in caret
library(shapr) # for SHAP
library(viridis) # for color plotting
library(patchwork) # for combining plots consistently
library(ggthemes)

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
lm_fit <- lm(damage ~ ., data = df_shuffled.log)

# SHAP analysis
ks <- permshap(lm_fit, df_shuffled.log[-1], bg_X = df_shuffled.log)
lm_shp <- shapviz(ks)
# SHAP plot
sv_importance(lm_shp)
sv_dependence(lm_shp, v = x, color_var = x, interactions = TRUE)
sv_force(lm_shp, row_id = 1:nrow(df_shuffled.log))
sv_waterfall(lm_shp, row_id = 1:nrow(df_shuffled.log))+
  theme(axis.text = element_text(size = 11))
sv_importance(lm_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))



#------------------------------------------------
## Random forest fit ##
rf_fit = train(damage ~ ., data=df_shuffled.log,
               method='rf',
               importance=TRUE,
               trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(rf_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
rf_shp <- shapviz(s)
# SHAP plot
sv_importance(rf_shp)
sv_importance(rf_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(rf_shp, v = x)
# # or
# rf_fit <- ranger(y = df_shuffled.log$damage, x = df_shuffled.log[-1],
#                  max.depth = 6, num.trees = 500)
# rf_unified_model <- ranger.unify(rf_fit, df_shuffled.log[-1])
# rf_shaps <- treeshap(rf_unified_model, df_shuffled.log[-1], interactions = TRUE)
# rf_shp <- shapviz(rf_shaps, X = df_shuffled.log)
# 
# sv_importance(rf_shp)
# sv_dependence(rf_shp, v = x, color_var = x, interactions = TRUE)
# 
# sv_waterfall(rf_shp, row_id = 1:nrow(df_shuffled.log))+
#   theme(axis.text = element_text(size = 11))
# 
# sv_force(rf_shp, row_id = 1:nrow(df_shuffled.log))



#------------------------------------------------
## XGBoost fit ##
xgb_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='xgbTree',
                 trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(xgb_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
xgb_shp <- shapviz(s)
# SHAP plot
sv_importance(xgb_shp)
sv_importance(xgb_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(xgb_shp, v = x)

# # or
# dtrain <- xgb.DMatrix(data.matrix(df_shuffled.log[x]),label = df_shuffled.log$damage)
# xgb_fit <- xgb.train(params = list(learning_rate = 0.1),
#                      data = dtrain, nrounds = 65)
# 
# # SHAP analysis
# xgb_shp <- shapviz(xgb_fit, X_pred = data.matrix(df_shuffled.log[x]),
#               interactions = TRUE)
# 
# sv_importance(xgb_shp, show_numbers = TRUE)
# sv_dependence(xgb_shp, v = x, color_var = x, interactions = TRUE)
# 
# sv_waterfall(xgb_shp, row_id = 1:nrow(df_shuffled.log))+
#   theme(axis.text = element_text(size = 11))
# 
# sv_force(xgb_shp, row_id = 1:nrow(df_shuffled.log))
# 
# # or
# 
# explainer <- shapr(df_shuffled.log[x], xgb_fit)
# p <- mean(df_shuffled.log$damage)
# explanation <- explain(
#   df_shuffled.log,
#   approach = "empirical",
#   explainer = explainer,
#   prediction_zero = p
# )
# print(explanation$dt)
# plot(explanation, plot_phi0 = FALSE,
#      index_x_test = c(1:10)) # only the first one, but we want for all obs
# 



#------------------------------------------------
## SVR fit ##
svr_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='svmRadial',
                 trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(svr_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
svr_shp <- shapviz(s)
# SHAP plot
sv_importance(svr_shp)
sv_importance(svr_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(svr_shp, v = x)



#------------------------------------------------
## BRNN fit ##
brnn_fit <- train(damage ~ ., data=df_shuffled.log,
                  method='brnn',
                  trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(brnn_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
brnn_shp <- shapviz(s)
# SHAP plot
sv_importance(brnn_shp)
sv_importance(brnn_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(brnn_shp, v = x)



#------------------------------------------------
## KNN fit ##
knn_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='knn',
                 trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(knn_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
knn_shp <- shapviz(s)
# SHAP plot
sv_importance(knn_shp)
sv_importance(knn_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(knn_shp, v = x)



#------------------------------------------------
## all fits (together) ##

all.shps_emdat <- c(LM = lm_shp, RF = rf_shp, XGBoost = xgb_shp,
              SVR = svr_shp, BRNN = brnn_shp, KNN = knn_shp)

# SHAP analysis
shap_plot <- sv_importance(all.shps_emdat,
                           sort_features = TRUE) +
  scale_fill_viridis_d(option = "C", direction = -1, name='EMDAT') +  # Correctly calling the scale function outside viridis_args
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14))+
  theme_few()
ggsave(filename = "./Figures/models.EMDAT.SHAP_analysis_importance.svg", 
       plot = shap_plot, 
       width = 6,
       height = 6, 
       dpi = 400)

# Create individual SHAP plots for each model
plots <- lapply(names(all.shps_emdat), function(model) {
  sv_importance(all.shps_emdat[[model]],
                kind = "bee",
                show_numbers = TRUE,
                sort_features = TRUE, 
                number_size = 4) +
    scale_fill_viridis_d(option = "C", direction = -1, name = model) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.title.x = element_text(size = 14)) +
    theme_few()+
    ggtitle(model) # Add titles to differentiate the models
})

# Combine all plots into one using patchwork
shap_plot <- wrap_plots(plots) + 
  plot_annotation(title = "")

# Save the combined plot
ggsave(filename = "./Figures/models.EMDAT.SHAP_analysis_importance_beeswarm.svg", 
       plot = shap_plot, 
       width = 16,
       height = 8, 
       dpi = 300)




# Create a list of dependence plots per model for a selected feature
key_features <- c("duration", "deaths", "area")
dep_plots_per_model <- list()

for (model in names(all.shps_emdat)) {
  model_feature_plots <- lapply(key_features, function(feature) {
    sv_dependence(all.shps_emdat[[model]],
                  v = feature,
                  color_var = NULL,
                  add_geom = FALSE) +
      geom_point(size = 4, shape = 16, color = "black", alpha = 1) +  # solid color for both center and border
      geom_smooth(method = "loess", color = "darkred", fill = "salmon", se = TRUE, span = 0.75) +
      labs(x = feature, y = "SHAP value") +
      theme_minimal(base_size = 28) +
      theme(
        plot.title = element_blank(),
        legend.position = "none"
      )
  })
  
  dep_plots_per_model[[model]] <- wrap_plots(model_feature_plots, nrow = 1) +
    plot_annotation(title = paste(model),
                    theme = theme(plot.title = element_text(size = 28, face = "bold")))
}

# Save plots
for (model in names(dep_plots_per_model)) {
  ggsave(filename = paste0("./Figures/SHAP_EMDAT_dependence_", model, ".svg"),
         plot = dep_plots_per_model[[model]],
         width = 20, height = 6, dpi = 300)
}



## extra plots (good for looking into individual observation within the dataset)
sv_force(all.shps_emdat,
         row_id = 1:nrow(df_shuffled.log))

sv_waterfall(all.shps_emdat)+
  theme(axis.text = element_text(size = 11))
##

# Only for LM, BRNN, and SVR panels

library(patchwork)
library(grid)

selected_models <- c("LM", "BRNN", "SVR")
key_features    <- c("duration", "deaths", "area")

row_plots <- lapply(selected_models, function(m) {
  
  shap_row <- wrap_plots(lapply(key_features, function(f) {
    sv_dependence(all.shps_emdat[[m]], v = f, color_var = NULL, add_geom = FALSE) +
      geom_point(size = 4, color = "black") +
      geom_smooth(method = "loess", color = "darkred",
                  fill = "salmon", se = TRUE, span = 0.75) +
      labs(x = f, y = "SHAP value") +
      theme_minimal(base_size = 26) +
      theme(legend.position = "none")
  }), nrow = 1)
  
  # ---- Explicit row title (cannot disappear) ----
  wrap_plots(
    wrap_elements(
      textGrob(m, rot = 90,
               gp = gpar(fontsize = 30, fontface = "bold"))
    ),
    shap_row,
    widths = c(0.06, 1)
  )
})

final_panel <- wrap_plots(row_plots, ncol = 1)

ggsave(
  "./Figures/SHAP_EMDAT_dependence_LM_BRNN_SVR.svg",
  final_panel,
  width  = 20,
  height = 16,
  dpi    = 300
)




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
lm_fit <- lm(damage ~ ., data = df_shuffled.log) #  with interaction terms between all predictors

# SHAP analysis
ks <- permshap(lm_fit, df_shuffled.log[-1], bg_X = df_shuffled.log)
lm_shp <- shapviz(ks)
# SHAP plot
sv_importance(lm_shp)
sv_dependence(lm_shp, v = x, color_var = x, interactions = TRUE)
sv_force(lm_shp, row_id = 1:nrow(df_shuffled.log))
sv_waterfall(lm_shp, row_id = 1:nrow(df_shuffled.log))+
  theme(axis.text = element_text(size = 11))
sv_importance(lm_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))



#------------------------------------------------
## Random forest fit ##
rf_fit = train(damage ~ ., data=df_shuffled.log,
               method='rf',
               importance=TRUE,
               trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(rf_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
rf_shp <- shapviz(s)
# SHAP plot
sv_importance(rf_shp)
sv_importance(rf_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(rf_shp, v = x)
# # or
# rf_fit <- ranger(y = df_shuffled.log$damage, x = df_shuffled.log[-1],
#                  max.depth = 6, num.trees = 500)
# rf_unified_model <- ranger.unify(rf_fit, df_shuffled.log[-1])
# rf_shaps <- treeshap(rf_unified_model, df_shuffled.log[-1], interactions = TRUE)
# rf_shp <- shapviz(rf_shaps, X = df_shuffled.log)
# 
# sv_importance(rf_shp)
# sv_dependence(rf_shp, v = x, color_var = x, interactions = TRUE)
# 
# sv_waterfall(rf_shp, row_id = 1:nrow(df_shuffled.log))+
#   theme(axis.text = element_text(size = 11))
# 
# sv_force(rf_shp, row_id = 1:nrow(df_shuffled.log))



#------------------------------------------------
## XGBoost fit ##
xgb_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='xgbTree',
                 trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(xgb_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
xgb_shp <- shapviz(s)
# SHAP plot
sv_importance(xgb_shp)
sv_importance(xgb_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(xgb_shp, v = x)

# # or
# dtrain <- xgb.DMatrix(data.matrix(df_shuffled.log[x]),label = df_shuffled.log$damage)
# xgb_fit <- xgb.train(params = list(learning_rate = 0.1),
#                      data = dtrain, nrounds = 65)
# 
# # SHAP analysis
# xgb_shp <- shapviz(xgb_fit, X_pred = data.matrix(df_shuffled.log[x]),
#               interactions = TRUE)
# 
# sv_importance(xgb_shp, show_numbers = TRUE)
# sv_dependence(xgb_shp, v = x, color_var = x, interactions = TRUE)
# 
# sv_waterfall(xgb_shp, row_id = 1:nrow(df_shuffled.log))+
#   theme(axis.text = element_text(size = 11))
# 
# sv_force(xgb_shp, row_id = 1:nrow(df_shuffled.log))
# 
# # or
# 
# explainer <- shapr(df_shuffled.log[x], xgb_fit)
# p <- mean(df_shuffled.log$damage)
# explanation <- explain(
#   df_shuffled.log,
#   approach = "empirical",
#   explainer = explainer,
#   prediction_zero = p
# )
# print(explanation$dt)
# plot(explanation, plot_phi0 = FALSE,
#      index_x_test = c(1:10)) # only the first one, but we want for all obs
# 



#------------------------------------------------
## SVR fit ##
svr_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='svmRadial',
                 trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(svr_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
svr_shp <- shapviz(s)
# SHAP plot
sv_importance(svr_shp)
sv_importance(svr_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(svr_shp, v = x)



#------------------------------------------------
## BRNN fit ##
brnn_fit <- train(damage ~ ., data=df_shuffled.log,
                  method='brnn',
                  trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(brnn_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
brnn_shp <- shapviz(s)
# SHAP plot
sv_importance(brnn_shp)
sv_importance(brnn_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(brnn_shp, v = x)



#------------------------------------------------
## KNN fit ##
knn_fit <- train(damage ~ ., data=df_shuffled.log,
                 method='knn',
                 trControl=trainControl(method='cv',number=10))
# SHAP analysis
s <- kernelshap(knn_fit, X = df_shuffled.log[, -1], bg_X = df_shuffled.log) 
knn_shp <- shapviz(s)
# SHAP plot
sv_importance(knn_shp)
sv_importance(knn_shp, kind = "both",show_numbers = TRUE, sort_features = TRUE, number_size = 4)+
  theme(axis.text = element_text(size = 12))
sv_dependence(knn_shp, v = x)



#------------------------------------------------
## all fits (together) ##

all.shps_dfo <- c(LM = lm_shp, RF = rf_shp, XGBoost = xgb_shp,
              SVR = svr_shp, BRNN = brnn_shp, KNN = knn_shp)

# SHAP analysis
shap_plot <- sv_importance(all.shps_dfo,
                           sort_features = TRUE) +
  scale_fill_viridis_d(option = "C", direction = -1, name='DFO15') +  # Correctly calling the scale function outside viridis_args
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14))+
  theme_few()

output_path <- paste0("./Figures/models.DFO.SHAP_analysis_importance.svg")
ggsave(filename = output_path,
       plot = shap_plot, width = 6, height = 6, dpi = 400)



# Create individual SHAP plots for each model
plots <- lapply(names(all.shps_dfo), function(model) {
  sv_importance(all.shps_dfo[[model]],
                kind = "bee",
                show_numbers = TRUE,
                sort_features = TRUE, 
                number_size = 4) +
    scale_fill_viridis_d(option = "C", direction = -1, name = model) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.title.x = element_text(size = 14)) +
    theme_few()+
    ggtitle(model) # Add titles to differentiate the models
})

# Combine all plots into one using patchwork
shap_plot <- wrap_plots(plots) + 
  plot_annotation(title = "")

# Save the combined plot
output_path <- paste0("./Figures/models.DFO.SHAP_analysis_importance_beeswarm.svg")
# Save the combined plot
ggsave(filename = output_path, 
       plot = shap_plot, 
       width = 16,
       height = 8, 
       dpi = 300)


# Create a list of dependence plots per model for a selected feature
key_features <- c("duration", "deaths", "area")
# Loop over models instead of features
dep_plots_per_model <- list()

for (model in names(all.shps_dfo)) {
  # Create a dependence plot for each feature in this model
  model_feature_plots <- lapply(key_features, function(feature) {
    sv_dependence(all.shps_dfo[[model]],
                  v = feature,
                  color_var = feature) +
      geom_point(size = 3) +  # ensure points are visible and uncolored
      geom_smooth(method = "loess", color = "darkred", fill = "salmon", se = TRUE, span = 0.75) +  # smooth trend with CI
      scale_color_viridis_c(option = "D", direction = -1, name = feature) +  # custom colorbar
      ggtitle(paste(feature)) +
      theme_minimal(base_size = 20)+
      theme(
        legend.position = "none",  # remove colorbar
        plot.title = element_blank()  # remove any title
      )
  })
  
  # Combine the 3 feature plots into one row per model
  dep_plots_per_model[[model]] <- wrap_plots(model_feature_plots, nrow = 1) +
    plot_annotation(title = paste(model),
                    theme = theme(plot.title = element_text(size = 28, face = "bold")))
}

# Save each model's SHAP dependence plot individually
for (model in names(dep_plots_per_model)) {
  ggsave(filename = paste0("./Figures/SHAP_DFO_dependence_", model, ".svg"),
         plot = dep_plots_per_model[[model]],
         width = 20, height = 6, dpi = 300)
}



# Create a list of dependence plots per model for a selected feature
key_features <- c("duration", "deaths", "area")
dep_plots_per_model <- list()

for (model in names(all.shps_dfo)) {
  model_feature_plots <- lapply(key_features, function(feature) {
    sv_dependence(all.shps_dfo[[model]],
                  v = feature,
                  color_var = NULL,
                  add_geom = FALSE) +
      geom_point(size = 4, shape = 16, color = "black", alpha = 1) +  # solid color for both center and border
      geom_smooth(method = "loess", color = "darkred", fill = "salmon", se = TRUE, span = 0.75) +
      labs(x = feature, y = "SHAP value") +
      theme_minimal(base_size = 28) +
      theme(
        plot.title = element_blank(),
        legend.position = "none"
      )
  })
  
  dep_plots_per_model[[model]] <- wrap_plots(model_feature_plots, nrow = 1) +
    plot_annotation(title = paste(model),
                    theme = theme(plot.title = element_text(size = 28, face = "bold")))
}

# Save plots
for (model in names(dep_plots_per_model)) {
  ggsave(filename = paste0("./Figures/SHAP_DFO_dependence_", model, ".svg"),
         plot = dep_plots_per_model[[model]],
         width = 20, height = 6, dpi = 300)
}





## extra plots (good for looking into individual observation within the dataset)
sv_force(all.shps_dfo,
         row_id = 1:nrow(df_shuffled.log))

sv_waterfall(all.shps_dfo)+
  theme(axis.text = element_text(size = 11))
##




## The End. ##


