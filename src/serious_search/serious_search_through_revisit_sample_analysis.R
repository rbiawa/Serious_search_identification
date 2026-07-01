#########################################################################################
# BEHAVIORAL ANALYSIS: BUYER vs CURIOUS
# 
# 
#
# source("src/serious_search/serious_search_through_revisit_sample_analysis.R")
#########################################################################################



#========================
# Sampling
#========================



{
  
  # Size
  
  prop <- .6
  
  size <- round(prop*nrow(analysis_df)/100)
  
  set.seed(123)
  
  users <- sample(unique(analysis_df_transformed$fullvisitorid), size = size)
  
  
  analysis_df_transformed_sample <- analysis_df_transformed %>% 
    filter(fullvisitorid %in% users)
}








summary_stats_samp <- analysis_df_transformed_sample %>%
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




if (interactive()) plot_continuous_variables(analysis_df_transformed_sample)


num_plot <- plot_continuous_variables(analysis_df_transformed_sample)

dir.create("out/pdf/other/", recursive = TRUE, showWarnings = FALSE)

pdf("out/pdf/other/visitor_stats_num_plot_log1p_sep_transf.pdf")


for (var in names(num_plot)) {
  grid.newpage()
  grid.draw(num_plot[[var]])
}

dev.off()


if (interactive()) plot_categorical_variables(analysis_df_transformed_sample, use_percent = TRUE)


cat_plot <- plot_categorical_variables(analysis_df_transformed_sample, use_percent = TRUE)



pdf("out/pdf/other/visitor_stats_cat_plot_transf.pdf")


for (var in names(cat_plot)) {
  grid.draw(cat_plot[[var]])
}

dev.off()




#======================================
# Multivariate analysis: PCA
#======================================


# With the original datframe : 

if (interactive() & FALSE) {
  Factoshiny(analysis_df_transformed_sample %>% 
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
                  "dep_contig", "city_contig", "n_reg_fct") %>% 
    dplyr::sample_n(5000)
  )
}



res.PCA_sample <- PCA(analysis_df_transformed_sample %>% 
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




summary(res.PCA_sample)


#Factoshiny(res.PCA_sample)


dir.create("out/pdf/serious_search", recursive = TRUE, showWarnings = FALSE)

pdf("out/pdf/serious_search/inertia_distribution_sample.pdf")

ggplot2::ggplot(cbind.data.frame(x = 1:nrow(res.PCA_sample$eig), y = res.PCA_sample$eig[, 2])) +
  ggplot2::aes(x = x, y = y) +
  ggplot2::geom_col(fill = "#5c99ad") +
  ggplot2::xlab("Dimension") +
  ggplot2::ylab("Percentage of variance") +
  #ggplot2::ggtitle("Decomposition of the total inertia") +
  ggplot2::theme_light() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::scale_x_continuous(breaks = 1:nrow(res.PCA_sample$eig))+
  theme_classic()

dev.off()



#====================================
# Axis description
#====================================

fviz_contrib(res.PCA_sample, choice = "var", axes = 1, xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA_sample$rotation), linetype=2)


fviz_contrib(res.PCA_sample, choice = "var", axes = 2, xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA_sample$rotation), linetype=2)

fviz_contrib(res.PCA_sample, choice = "var", axes = 3, xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA_sample$rotation), linetype=2)

#====================================
# Factorial plane plot
#====================================

plot.PCA(res.PCA_sample,
         choix='var',
         select='cos2  0.5',
         unselect=0,
         title="Graphe des variables de l'ACP",
         col.quanti.sup='#0000FF')


fviz_pca_var(
  res.PCA_sample,
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
  res.PCA_sample,
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
  res.PCA_sample,
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










#=============================================
# Clustering with HCPC method
#=============================================

#https://bookdown.org/evraloui/lbira2110/clustering.html
#https://mtes-mct.github.io/parcours_r_module_analyse_multi_dimensionnelles/classification-clustering.html


res.HCPC_sample <- HCPC(res.PCA_sample,
                 #        nb.clust=3,
                 consol=FALSE,
                 graph=FALSE)

res.HCPC_sample$desc.axes

res.HCPC_sample$desc.var

plot.HCPC(res.HCPC_sample,choice='tree',title='Arbre hiérarchique')

plot.HCPC(res.HCPC_sample,choice='map',draw.tree=FALSE,title='Plan factoriel')

plot.HCPC(res.HCPC_sample,
          choice='3D.map',
          ind.names=FALSE,centers.plot=FALSE,angle=60,
          title='Arbre hiérarchique sur le plan factoriel')

plot.HCPC(res.HCPC_sample,
          choice='map',
          draw.tree=FALSE,
          title='Plan factoriel',
          axes=c(1,3))

plot.HCPC(res.HCPC_sample,
          choice='3D.map',
          ind.names=FALSE,
          centers.plot=FALSE,
          angle=60,
          title='Arbre hiérarchique sur le plan factoriel',
          axes=c(1,3))



analysis_df_transformed_sample$cluster_hcpc <- res.HCPC_sample$data.clust$clust


freq(analysis_df_transformed_sample$cluster_hcpc)


analysis_df_transformed_sample[, research_category := factor(cluster_hcpc)]

levels(analysis_df_transformed_sample$research_category) <- c(
  "Passive Browsers", #"Casual Passers‑by"
  "Engaged Comparers",
  "Intensive Explorers"
)

freq(analysis_df_transformed_sample$cluster_hcpc)

freq(analysis_df_transformed_sample$research_category)



analysis_df_transformed_sample$revisit_intensity <- res.PCA_sample$ind$coord[,1]
analysis_df_transformed_sample$preference_diversity <- res.PCA_sample$ind$coord[,2]
analysis_df_transformed_sample$temporal_rhythm <- res.PCA_sample$ind$coord[,3]



#==============================
# Cluster description
#==============================

freq(analysis_df_transformed_sample$any_revisit_fct)

prop.table(table(analysis_df_transformed_sample$research_category,
                 analysis_df_transformed_sample$any_revisit_fct),
           1)



#======================================
# Plot cluster
#======================================

fviz_pca_ind(
  res.PCA_sample,
  axes = c(1, 2),
  geom.ind = "point",
  habillage = analysis_df_transformed_sample$cluster_hcpc,
  #  addEllipses = TRUE,
  alpha.ind = 0.2,             
  pointshape = 1            
)


fviz_pca_ind(
  res.PCA_sample,
  axes = c(1, 2),
  select.ind = list(cos2 = 0.5),
  geom.ind = "point",
  habillage = analysis_df_transformed_sample$cluster_hcpc,
  #  addEllipses = TRUE,
  alpha.ind = 0.2,             
  pointshape = 1            
)



fviz_pca_ind(
  res.PCA_sample,
  axes = c(1, 2),
  geom.ind = "point",
  habillage = analysis_df_transformed_sample$research_category,
  #  addEllipses = TRUE,
  alpha.ind = 0.2,             
  pointshape = 1            
)


fviz_pca_ind(
  res.PCA_sample,
  axes = c(1, 2),
  select.ind = list(cos2 = 0.5),
  geom.ind = "point",
  habillage = analysis_df_transformed_sample$research_category,
  #  addEllipses = TRUE,
  alpha.ind = 0.2,             
  pointshape = 1            
)



fviz_pca_ind(
  res.PCA_sample,
  axes = c(1, 3),
  geom.ind = "point",
  habillage = analysis_df_transformed_sample$research_category,
  #  addEllipses = TRUE,
  alpha.ind = 0.2,             
  pointshape = 1            
)




fviz_pca_ind(
  res.PCA_sample,
  axes = c(1, 3),
  select.ind = list(cos2 = 0.5),
  geom.ind = "point",
  habillage = analysis_df_transformed_sample$research_category,
  #  addEllipses = TRUE,
  alpha.ind = 0.2,             
  pointshape = 1            
)





fviz_cluster(res.HCPC_sample, geom = "point", ellipse = TRUE)

fviz_cluster(res.HCPC_sample, geom = "point", ellipse = TRUE, axes = c(1, 3))

#======================================
# Clsutering quality
#======================================


sil <- cluster::silhouette(as.numeric(res.HCPC_sample$data.clust$clust), dist(res.PCA_sample$ind$coord))
if (interactive()) plot(sil, color = c("red", "green", "blue"))

aggregate(sil[, 3], list(cluster = as.numeric(res.HCPC_sample$data.clust$clust)), mean)






#======================================
# Who are the revistors of cluster 1
#======================================


analysis_df_transformed_sample <- analysis_df_transformed_sample %>% 
  mutate(
    cluster_hcpc_mod = as.numeric(as.character(cluster_hcpc)),
    cluster_hcpc_mod = if_else(cluster_hcpc_mod == 1 & any_revisit, 4, cluster_hcpc_mod)
  ) %>% 
  mutate(cluster_hcpc_mod = as.factor(cluster_hcpc_mod))



freq(analysis_df_transformed_sample$cluster_hcpc_mod)

axes <- c(1, 3)


fviz_pca_ind(
  res.PCA_sample,
  axes = axes,
  geom.ind = "point",
  habillage = analysis_df_transformed_sample$cluster_hcpc_mod,
  #  addEllipses = TRUE,
  alpha.ind = 0.2,             
  pointshape = 1            
)


fviz_pca_ind(
  res.PCA_sample,
  axes = axes,
  select.ind = list(cos2 = 0.5),
  geom.ind = "point",
  habillage = analysis_df_transformed_sample$cluster_hcpc_mod,
  #  addEllipses = TRUE,
  alpha.ind = 0.2,             
  pointshape = 1            
)






















#==================================
#
# LOGISTIQUE REGRESSION
#
#==================================

# is_logged  
# phone_display           
# mail_form

# any_action
# any_contact 


# Correlation matrix plot



vars_to_keep <- c(
  "n_listings_revisited", "n_listings", "nb_sessions",
  "prop_listings_revisited", "n_revisits_24h",
  "n_revisits_lt1h", "n_revisits_lt6h", "n_revisits_gt48h",
  "any_revisit",
  "room_count_simpson", "price_sd", "area_sd",
  "revisit_intensity",
  "preference_diversity",
  "preference_diversity", 
  
  "research_category"
)


df_corr <- analysis_df_transformed_sample %>% 
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




#==============================
# Variables labeling
#==============================

# library(labelled)
# var_label(d$sport) <- "Pratique du sport ?"
# var_label(d$sexe) <- "Sexe"
# var_label(d$grpage) <- "Groupe d'âges"
# var_label(d$etud) <- "Niveau d'étude"
# var_label(d$relig) <- "Pratique religieuse"
# var_label(d$heures.tv) <- "Heures de télévision / jour"
# var_label(d$trav.imp) <- "Importance du travail"




#==========================
# Contact Modeling
#==========================

contact_model <- glm(any_contact ~ 
                       n_listings + nb_sessions + 
                       #                      prop_listings_revisited +
                       #                       n_revisits_24h + 
                       n_listings_revisited + 
                       n_revisits_lt1h + 
                       n_revisits_lt6h + 
                       n_revisits_gt48h +
                       any_revisit_fct +
                       
                       room_count_simpson + price_sd + area_sd 
                     + city_simpson + dep_simpson 
                     + city_contig + dep_contig
                     #                     + revisit_intensity  
                     #                    +  preference_diversity 
                     
                     #                      + research_category
                     
                     ,data = analysis_df_transformed_sample,
                     family = binomial)


vif(contact_model)

summary(contact_model)



#========================================
# Model selection
#========================================


contact_model <- stepAIC(contact_model,
                         direction = "backward")

vif(contact_model)

summary(contact_model)



(res_dev <- deviance(contact_model))
(ddl <- df.residual(contact_model))
(dispersion <- res_dev / ddl)

(p_value <- 1 - pchisq(res_dev, ddl))



hoslem.test(contact_model$y, fitted(contact_model), g = 10)



#install.packages("pscl")
#library(pscl)

pR2(contact_model)

if (!require("DescTools")) install.packages("DescTools");library(DescTools)

PseudoR2(contact_model, which = "all")

#library(pROC)

predictions <- predict(contact_model, type = "response")

length(predictions) == length(contact_model$data$any_contact)

roc_curve <- roc(contact_model$data$any_contact, predictions, percent=TRUE, plot=TRUE, ci=TRUE)
plot(roc_curve)

auc(roc_curve)


roc_curve <- roc(contact_model$data$any_contact, predictions, percent = TRUE, ci = TRUE)

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



#crPlots(mod, ask = FALSE)

dir.create("out/pdf/serious_search/glm/", recursive = TRUE, showWarnings = FALSE)
dir.create("out/png/serious_search/glm/", recursive = TRUE, showWarnings = FALSE)

pdf("out/pdf/serious_search/glm/crplots_modele.pdf")

crPlots(contact_model, ask = FALSE)

dev.off()



pdf("out/pdf/serious_search/glm/avplots_modele.pdf")

avPlots(contact_model, ask = FALSE)

dev.off()








#==========================
# Mail form Modeling
#==========================

mail_form_model <- glm(mail_form ~ 
                         n_listings + nb_sessions + 
                         #                      prop_listings_revisited +
                         n_revisits_24h + 
                         n_listings_revisited + 
                         n_revisits_lt1h + 
                         n_revisits_lt6h + 
                         n_revisits_gt48h +
                         any_revisit_fct +
                         
                         room_count_simpson + price_sd + area_sd 
                       + city_simpson + dep_simpson 
                       + city_contig + dep_contig
                       #                     + revisit_intensity  
                       #                    +  preference_diversity 
                       
                       #                      + research_category
                       
                       ,data = analysis_df_transformed_sample,
                       family = binomial)


vif(mail_form_model)

summary(mail_form_model)



#========================================
# Model selection
#========================================


mail_form_model <- stepAIC(mail_form_model,
                           direction = "backward")

vif(mail_form_model)

summary(mail_form_model)



(res_dev <- deviance(mail_form_model))
(ddl <- df.residual(mail_form_model))
(dispersion <- res_dev / ddl)

(p_value <- 1 - pchisq(res_dev, ddl))


hoslem.test(mail_form_model$y, fitted(mail_form_model), g = 10)

#install.packages("pscl")
#library(pscl)

pR2(mail_form_model)

if (!require("DescTools")) install.packages("DescTools");library(DescTools)

PseudoR2(mail_form_model, which = "all")


#library(pROC)

predictions <- predict(mail_form_model, type = "response")

length(predictions) == length(mail_form_model$data$mail_form)

roc_curve <- roc(mail_form_model$data$mail_form, predictions, percent=TRUE, plot=TRUE, ci=TRUE)
plot(roc_curve)

auc(roc_curve)

dir.create("out/pdf/serious_search/mail_form_regression",
           recursive = TRUE, showWarnings = FALSE)

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



# Logit linearity



#crPlots(mod, ask = FALSE)



pdf("out/pdf/serious_search/mail_form_regression/mail_form_crplots_model.pdf")

crPlots(mail_form_model, ask = FALSE)

dev.off()



pdf("out/pdf/serious_search/mail_form_regression/mail_form_avplots_model.pdf")

avPlots(mail_form_model, ask = FALSE)

dev.off()