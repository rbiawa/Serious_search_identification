################################################################################
# Script to analyze search engagement through listings' revisit indicators and 
#      search variability indicators
# 
#
# source("src/serious_search/serious_search_through_revisit_analysis.R")
################################################################################




#======================================
# Variable selection
#======================================

analysis_df <- visitor_stats %>% 
  dplyr::select("fullvisitorid", 
                # Search intensity
                "n_events", "n_listings", "nb_sessions", "mean_session_size", 
                "mean_click_per_listing", "max_visits_on_listing", "top1_share",
                "n_switches", 
                
                # Revisit variables
                "n_listings_revisited", "prop_listings_revisited", "n_revisits_24h",
                "n_revisits_lt1h", "n_revisits_lt6h", "n_revisits_gt48h",
                "any_revisit",
                
                # Typomorphologic caracteristics variability
                "type_simpson", "room_count_simpson", "price_sd", "area_sd", 
                
                # Spatial variability
                "n_city", "n_dep", "n_reg", "city_simpson", "dep_simpson", 
                "dep_contig", "city_contig",
                
                # Logging and contact indicators
                "is_logged", "phone_display", "mail_form", "any_contact", 
                "any_action", "any_action_fct", "any_contact_fct",
                "mail_form_fct", "phone_display_fct", "any_revisit_fct")

rm(visitor_stats)


num_plot <- plot_continuous_variables(analysis_df, transformation = "log1p")

dir.create("out/pdf/other/", recursive = TRUE, showWarnings = FALSE)

pdf("out/pdf/other/visitor_stats_num_plot_log1p.pdf")


for (var in names(num_plot)) {
  grid.newpage()
  grid.draw(num_plot[[var]])
}

dev.off()


cat_plot <- plot_categorical_variables(analysis_df)



pdf("out/pdf/other/visitor_stats_cat_plot.pdf")


for (var in names(cat_plot)) {
  grid.draw(cat_plot[[var]])
}

dev.off()





#======================================
# Data transform
#======================================

diff_to_max <- function(x) max(1 + x) - x

dist_to_min <- function(x) x - min(x)
dist_to_min1p <- function(x) x - min(x + 1)


log1p_dist_to_min <- function(x) log1p(dist_to_min(x))
inv_dist_to_max_1p <- function(x) 1/(max(1 + x) - x) 

if(exists("logit")) rm(logit)


adjust = 1e-6

# Variables to be transformed into logit
special_variables <- c("top1_share", "type_simpson", "room_count_simpson", "city_simpson",
                       "dep_simpson", "prop_listings_revisited",
                       "n_city", "n_dep", "n_reg")


# log1p transformation

analysis_df_transformed <- analysis_df %>% 
  mutate(across(where(is.numeric) & !all_of(special_variables), log1p)) #log1p_dist_to_min

# Special transformations

analysis_df_transformed$top1_share <- logit(analysis_df$top1_share, adjust = adjust)

analysis_df_transformed$type_simpson <- car::logit(analysis_df_transformed$type_simpson, adjust = adjust)

analysis_df_transformed$room_count_simpson <- inv_dist_to_max_1p(analysis_df_transformed$room_count_simpson)

analysis_df_transformed$city_simpson <- inv_dist_to_max_1p(analysis_df_transformed$city_simpson)

analysis_df_transformed$dep_simpson <- log1p(analysis_df_transformed$dep_simpson)

analysis_df_transformed$prop_listings_revisited <- car::logit(analysis_df_transformed$prop_listings_revisited, adjust = adjust)

analysis_df_transformed$n_reg_fct <- paste0(
  analysis_df_transformed$n_reg,
  ifelse(analysis_df_transformed$n_reg == 1, "_region", "_regions")
)


analysis_df_transformed <- analysis_df_transformed %>%
  mutate(city_contig = as.logical(city_contig),
         dep_contig = as.logical(dep_contig))




summary_stats <- analysis_df_transformed %>%
  dplyr::select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n      = sum(!is.na(value)),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    IQR    = IQR(value, na.rm = TRUE),
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE)
  ) %>%
  arrange(variable)

freq(analysis_df_transformed$any_revisit)


if (interactive()) plot_continuous_variables(analysis_df_transformed)


num_plot <- plot_continuous_variables(analysis_df_transformed)

dir.create("out/pdf/other/", recursive = TRUE, showWarnings = FALSE)

pdf("out/pdf/other/visitor_stats_num_plot_log1p_sep_transf.pdf")


for (var in names(num_plot)) {
  grid.newpage()
  grid.draw(num_plot[[var]])
}

dev.off()


if (interactive()) plot_categorical_variables(analysis_df_transformed, use_percent = TRUE)


cat_plot <- plot_categorical_variables(analysis_df_transformed, use_percent = TRUE)



pdf("out/pdf/other/visitor_stats_cat_plot_transf.pdf")


for (var in names(cat_plot)) {
  grid.draw(cat_plot[[var]])
}

dev.off()





#======================================
# Multivariate analysis: PCA
#======================================


res.PCA <- PCA(analysis_df_transformed %>% 
               dplyr::select("n_events", "n_listings", "nb_sessions", 
                             "mean_session_size", "mean_click_per_listing", 
                             "max_visits_on_listing", "top1_share", 
                             "type_simpson", "room_count_simpson", "price_sd", "area_sd",
                             "city_simpson", 
                             "dep_simpson", 
                             "n_switches", "n_listings_revisited", "prop_listings_revisited", 
                             "n_revisits_24h", "n_revisits_lt1h", "n_revisits_lt6h", "n_revisits_gt48h",
                             "any_revisit", 
                             "is_logged", "phone_display", "mail_form", "any_contact", "any_action", 
                             "dep_contig", "city_contig", "n_reg_fct"),
             ncp=3,
             quali.sup=c(21:29),
             quanti.sup=c(1,2,3,4,5,6,7,8,12:13),
             graph=FALSE
)




summary(res.PCA)



dir.create("out/pdf/serious_search", recursive = TRUE, showWarnings = FALSE)

pdf("out/pdf/serious_search/inertia_distribution.pdf")

ggplot2::ggplot(cbind.data.frame(x = 1:nrow(res.PCA$eig), y = res.PCA$eig[, 2])) +
  ggplot2::aes(x = x, y = y) +
  ggplot2::geom_col(fill = "#5c99ad") +
  ggplot2::xlab("Dimension") +
  ggplot2::ylab("Percentage of variance") +
  #ggplot2::ggtitle("Decomposition of the total inertia") +
  ggplot2::theme_light() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::scale_x_continuous(breaks = 1:nrow(res.PCA$eig))+
  theme_classic()

dev.off()



#====================================
# Axis description
#====================================

fviz_contrib(res.PCA, choice = "var", axes = 1, xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA$rotation), linetype=2)


fviz_contrib(res.PCA, choice = "var", axes = 2, xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA$rotation), linetype=2)

fviz_contrib(res.PCA, choice = "var", axes = 3, xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA$rotation), linetype=2)

#====================================
# Factorial plane plot
#====================================

plot.PCA(res.PCA,
         axes = c(1, 2),
         choix='var',
         select='cos2  0.5',
         unselect=0,
         title="Graphe des variables de l'ACP",
         col.quanti.sup='#0000FF')


fviz_pca_var(
  res.PCA,
  axes = c(1, 2),
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  select.var = list(cos2 = 0.5),
  invisible = "quanti.sup",
  alpha.var = 0.5,
  repel = TRUE,
  title = "Correlation circle colored by contribution",
)+
  theme_minimal() 


fviz_pca_var(
  res.PCA,
  axes = c(1, 2),
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  select.var = list(cos2 = 0.5),
  invisible = "quanti.sup",
  alpha.var = 0.5,
  repel = TRUE,
  geom.var.label = list(size = 0.3),
  title = "Correlation circle colored by contribution"
) +
  theme_minimal()


fviz_pca_var(
  res.PCA,
  axes = c(1, 3),
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  select.var = list(cos2 = 0.5),
  invisible = "quanti.sup",
  alpha.var = 0.5,
  repel = TRUE,
  geom.var.label = list(size = 0.3),
  title = "Correlation circle colored by contribution"
) +
  theme_minimal()



#==================================
#
# LOGISTIQUE REGRESSION
#
#==================================

# Correlation matrix plot



vars_to_keep <- c(
  "n_listings_revisited", "n_listings", "nb_sessions",
  "prop_listings_revisited", "n_revisits_24h",
  "n_revisits_lt1h", "n_revisits_lt6h", "n_revisits_gt48h",
  "any_revisit",
  "room_count_simpson", "price_sd", "area_sd",
  "city_simpson" , "dep_simpson", 
  "revisit_intensity",
  "preference_diversity",
  "preference_diversity", 
  
  "research_category"
)

vars_to_keep <- vars_to_keep[vars_to_keep %in% names(analysis_df_transformed)]

df_corr <- analysis_df_transformed %>% 
  dplyr::select(all_of(vars_to_keep)) %>% 
  dplyr::select(where(is.numeric))   

corr_matrix <- cor(df_corr, use = "pairwise.complete.obs")

# Visualization
if(interactive()) corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  number.cex = 0.6,
  addCoef.col = "black",
  diag = FALSE
)





#==========================
# Contact Modeling
#==========================

dir.create("out/pdf/serious_search/contact_regression", recursive = TRUE, showWarnings = FALSE)
dir.create("out/png/serious_search/contact_regression", recursive = TRUE, showWarnings = FALSE)
dir.create("out/tex/serious_search/contact_regression", recursive = TRUE, showWarnings = FALSE)


contact_model <- glm(any_contact ~ 
                       n_listings + nb_sessions + 
                       # prop_listings_revisited +
                       # n_revisits_24h + 
                       n_listings_revisited + 
                       n_revisits_lt1h + 
                       #n_revisits_lt6h + 
                       n_revisits_gt48h +
                       any_revisit +
                       
                       room_count_simpson + price_sd + area_sd 
                     + city_simpson + dep_simpson 
                     + city_contig + dep_contig + n_reg_fct

                     
                     ,data = analysis_df_transformed,
                     family = binomial)


vif(contact_model)

summary(contact_model)





#========================================
# Influent observations
#========================================

# Cook distance analysis

cookd <- data.frame(
  dist = cooks.distance(contact_model),
  oid = 1:nrow(contact_model$data)
)

if (interactive()){
  ggplot(cookd) + 
    geom_point(aes(x = oid, y = dist ), color = rgb(0.1,0.1,0.1,0.4), size = 1)+
    geom_hline(yintercept = 16/dim(data)[1], color = "red")+
    labs(x = "observations", y = "distance de Cook") + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}




#========================================
# Model selection
#========================================


contact_model <- stepAIC(contact_model,
                         direction = "backward")

vif(contact_model)

summary(contact_model)

contact_model_summary <- odds.ratio(contact_model)



(res_dev <- deviance(contact_model))
(ddl <- df.residual(contact_model))
(dispersion <- res_dev / ddl)

(p_value <- 1 - pchisq(res_dev, ddl))



hoslem.test(contact_model$y, fitted(contact_model), g = 100)



retrieve_package("pscl")


pR2(contact_model)


retrieve_package("DescTools")

PseudoR2(contact_model, which = "all")


retrieve_package("pROC")

predictions <- predict(contact_model, type = "response")

length(predictions) == length(contact_model$data$any_contact)

roc_curve <- roc(contact_model$data$any_contact, 
                 predictions, percent=TRUE, plot=TRUE, ci=TRUE)

if (interactive()) plot(roc_curve)

auc(roc_curve)


roc_curve <- roc(contact_model$data$any_contact, 
                 predictions, percent = TRUE, ci = TRUE)

pdf("out/pdf/serious_search/contact_regression/roc_auc.pdf", width = 10, height = 8)

plot(roc_curve,
     col = "#1c61b6",
     lwd = 3,
     main = "ROC")

auc_value <- auc(roc_curve)
ci_auc <- ci.auc(roc_curve)

legend("bottomright",
       legend = c(
         paste0("AUC = ", round(auc_value, 3)),
         paste0("IC 95% : [", 
                round(ci_auc[1], 3), "; ", 
                round(ci_auc[3], 3), "]")
       ),
       col = "#1c61b6",
       lwd = 3)

dev.off()



# Logit linearity

if(FALSE){

  pdf("out/pdf/serious_search/contact_regression/crplots_model.pdf")
  
  crPlots(contact_model, ask = FALSE)
  
  dev.off()
  
  
  
  pdf("out/pdf/serious_search/contact_regression/avplots_model.pdf")
  
  avPlots(contact_model, ask = FALSE)
  
  dev.off()

}




sink("out/tex/serious_search/contact_regression/contact_model_summary.tex")
print(
  xtable::xtable(contact_model_summary,
                 align = c(rep("l", 1), rep("r", 4)),
                 display = c(rep("s", 1), rep("f", 3), "e")
  ),
  include.rownames = TRUE,   # ou FALSE selon ton besoin
  booktabs = TRUE,            # joli rendu LaTeX
  floating = FALSE   # <-- crucial: no table environment
  ,auto = TRUE
)
sink()





#==========================
# Mail form Modeling
#==========================

dir.create("out/pdf/serious_search/mail_form_regression", recursive = TRUE, showWarnings = FALSE)
dir.create("out/png/serious_search/mail_form_regression", recursive = TRUE, showWarnings = FALSE)
dir.create("out/tex/serious_search/mail_form_regression", recursive = TRUE, showWarnings = FALSE)




mail_form_model <- glm(mail_form ~ 
                         n_listings + nb_sessions + 
                         # prop_listings_revisited +
                         # n_revisits_24h + 
                         # n_listings_revisited + 
                         n_revisits_lt1h + 
                         # n_revisits_lt6h + 
                         n_revisits_gt48h +
                         any_revisit +
                         
                         room_count_simpson + price_sd + area_sd 
                       + city_simpson + dep_simpson 
                       + city_contig + dep_contig + n_reg_fct
                 
                       ,data = analysis_df_transformed,
                       family = binomial)


vif(mail_form_model)

summary(mail_form_model)


odds.ratio(mail_form_model)


#========================================
# Influent observations
#========================================


# Cook distance analysis

cookd_mail <- data.frame(
  dist = cooks.distance(mail_form_model),
  oid = 1:nrow(mail_form_model$data)
)


if (FALSE){

analysis_df_transformed$cookd_mail <- cookd_mail$dist


mail_form_model <- glm(mail_form ~
                         n_listings + nb_sessions +
                         # prop_listings_revisited +
                         # n_revisits_24h +
                         # n_listings_revisited +
                         n_revisits_lt1h +
                         # n_revisits_lt6h +
                         n_revisits_gt48h +
                         any_revisit +

                         room_count_simpson + price_sd + area_sd
                       + city_simpson + dep_simpson
                       + city_contig + dep_contig + n_reg_fct

                       ,data = subset(analysis_df_transformed, cookd_mail < 16/nrow(analysis_df_transformed)),
                       family = binomial)



nrow(mail_form_model$data) - nrow(analysis_df_transformed)

nrow(mail_form_model$data)/nrow(analysis_df_transformed)


vif(mail_form_model)

summary(mail_form_model)

}
#========================================
# Model selection
#========================================


mail_form_model <- stepAIC(mail_form_model,
                           direction = "backward")

vif(mail_form_model)

summary(mail_form_model)

mail_form_model_summary <- odds.ratio(mail_form_model)

(res_dev <- deviance(mail_form_model))
(ddl <- df.residual(mail_form_model))
(dispersion <- res_dev / ddl)

(p_value <- 1 - pchisq(res_dev, ddl))


hoslem.test(mail_form_model$y, fitted(mail_form_model), g = 10)


pR2(mail_form_model)



PseudoR2(mail_form_model, which = "all")


predictions <- predict(mail_form_model, type = "response")

length(predictions) == length(mail_form_model$data$mail_form)

roc_curve <- roc(mail_form_model$data$mail_form, predictions, percent=TRUE, plot=TRUE, ci=TRUE)
plot(roc_curve)

auc(roc_curve)



roc_curve <- roc(mail_form_model$data$mail_form, predictions, percent = TRUE, ci = TRUE)

pdf("out/pdf/serious_search/mail_form_regression/mail_roc_auc.pdf", width = 10, height = 8)

plot(roc_curve,
     col = "#1c61b6",
     lwd = 3,
     main = "ROC")

auc_value <- auc(roc_curve)
ci_auc <- ci.auc(roc_curve)

legend("bottomright",
       legend = c(
         paste0("AUC = ", round(auc_value, 3)),
         paste0("IC 95% : [", 
                round(ci_auc[1], 3), "; ", 
                round(ci_auc[3], 3), "]")
       ),
       col = "#1c61b6",
       lwd = 3)

dev.off()




sink("out/tex/serious_search/mail_form_regression/mail_form_model_summary.tex")
print(
  xtable::xtable(mail_form_model_summary,
                 align = c(rep("l", 1), rep("r", 4)),
                 display = c(rep("s", 1), rep("f", 3), "e")
  ),
  include.rownames = TRUE,   
  booktabs = TRUE,            
  floating = FALSE   
  ,auto = TRUE
)
sink()



# Logit linearity
 

if(FALSE){
  
  pdf("out/pdf/serious_search/mail_form_regression/mail_form_crplots_model.pdf")
  
  crPlots(mail_form_model, ask = FALSE)
  
  dev.off()
  
  
  
  pdf("out/pdf/serious_search/mail_form_regression/mail_form_avplots_model.pdf")
  
  avPlots(mail_form_model, ask = FALSE)
  
  dev.off() 
  
  
}

