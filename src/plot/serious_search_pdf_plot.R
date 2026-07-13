################################################################################
# Plot serious search graphics
# 
# 
#
# source("src/plot/serious_search_pdf_plot.R")
################################################################################

if(!require("patchwork")) install.packages("patchwork"); library(patchwork)
if(!require("xtable")) install.packages("xtable"); library(xtable)

#=======================
# Plot parameters
#=======================

my_blue <- c("#498ea5", "#94d3e7", "#1c61b6")[1] 

my_red  <- c("#742C08", "#e6550d")[1]

my_gray <- c("#dfd5d5b7", "gray", "#857a7a")[1] 

my_green  <- c("#20d480", "#93d393", "green")[1]

font_size <- 20


#=====================================
#
# Univariate analysis
#
#=====================================

col_contact <- c(my_red, my_blue)
col_revisit    <- c("tomato3", "turquoise")

dir.create("out/pdf/serious_search/univariate_analysis/", 
           recursive = TRUE, showWarnings = FALSE)

freq(analysis_df_transformed_sample$any_action)
freq(analysis_df_transformed_sample$any_contact)
freq(analysis_df_transformed_sample$mail_form)
freq(analysis_df_transformed_sample$phone_display)
freq(analysis_df_transformed_sample$any_revisit)



{
  # Table 1
  df_contact <- as.data.frame(prop.table(table(
    analysis_df_transformed_sample$any_contact)
    )
  )
  names(df_contact) <- c("any_contact", "prop")
  
  # Table 2
  df_mail <- as.data.frame(prop.table(
    table(analysis_df_transformed_sample$mail_form)
    )
  )
  names(df_mail) <- c("mail", "prop")
  
  
  p1 <- ggplot(df_contact, aes(x = any_contact, y = prop, fill = any_contact)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = col_contact) +
    labs(
    #  title = "contact proportion",
      x = "Any contact",
      y = "Proportion"
    ) +
    theme_minimal()
  
  p2 <- ggplot(df_mail, aes(x = mail, y = prop, fill = mail)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = col_contact) +
    labs(
#      title = "Mail form proportion",
      x = "Mail form",
      y = ""
    ) +
    theme_minimal()
}

pdf("out/pdf/serious_search/univariate_analysis/contacts.pdf",
    width = 14, height = 4)
p1 + p2 + plot_layout(ncol = 2)
dev.off()


# Revisit vs contact temporality


if(exists("contact_behavior")) {
  
  contact_behavior$timing_category <- factor(
    contact_behavior$timing_category,
    levels = c("revisit_strictly_before", "simultaneous_revisit_and_contact",
               "revisit_after_contact", "no_revisit")
  )
  
  tab <- freq(contact_behavior$timing_category, cum = TRUE) %>% 
    mutate(n = round(n), 'val%' = NULL, 'val%cum' = NULL)
  
  
  print(
    xtable(tab,
           align = c("l", "r", "r", "r"),
           display = c("s", "d", rep("f", 2))
           ),
    include.rownames = TRUE,
    booktabs = TRUE

  )

  sink("out/tex/serious_search/univariate_analysis/table_timing_category.tex")
  print(xtable(tab,
               align = c("l", "r", "r", "r"),
               display = c("s", "d", rep("f", 2))
             ), 
        include.rownames = TRUE,
        booktabs = TRUE,
        floating = FALSE
        )
  sink()
  
  
  
  df_rev_vs_cont <- as.data.frame(prop.table(
    table(contact_behavior$timing_category)
    )
  )
  names(df_rev_vs_cont) <- c("timing_category", "prop")
  
  
  rev_vs_cont <- ggplot(df_rev_vs_cont, aes(x = timing_category, 
                                            y = prop, fill = timing_category)) +
    geom_bar(stat = "identity") +
    #scale_fill_manual(values = c(my_blue, my_red)) +
    labs(
      #  title = "Timing category",
      x = "Timing category",
      y = "Proportion"
    ) +
    theme_minimal()
}




tab_clust <- freq(analysis_df_transformed_sample$research_category,
                  cum = TRUE) %>% 
  mutate(n = round(n), 'val%' = NULL, 'val%cum' = NULL)


print(
  xtable(tab_clust,
         align = c("l", "r", "r", "r"),
         display = c("s", "d", rep("f", 2))
  ),
  include.rownames = TRUE,
  booktabs = TRUE
  
)

sink("out/tex/serious_search/univariate_analysis/search_categories.tex")
print(xtable(tab_clust,
             align = c("l", "r", "r", "r"),
             display = c("s", "d", rep("f", 2))
), 
include.rownames = TRUE,
booktabs = TRUE,
floating = FALSE
)
sink()




#===============================
# Serious searchers
#===============================



analysis_df_transformed <- analysis_df_transformed %>% 
  mutate(serious_searcher = (any_action + any_revisit) > 0)

freq(analysis_df_transformed$serious_searcher)

analysis_df_transformed[, serious_searcher_fct :=
                factor(serious_searcher, 
                       labels = c("Neither contacted nor visited", 
                                  "Contacted or revisited"))
                ]

freq(analysis_df_transformed$serious_searcher_fct)


tab_serious_searchers <- freq(analysis_df_transformed$serious_searcher_fct, 
                              cum = TRUE) %>% 
  mutate(n = round(n), 'val%' = NULL, 'val%cum' = NULL)

sink("out/tex/serious_search/univariate_analysis/serious_searchers.tex")
print(xtable(tab_serious_searchers,
             align = c("l", "r", "r", "r"),
             display = c("s", "d", rep("f", 2))
), 
include.rownames = TRUE,
booktabs = TRUE,
floating = FALSE  
)
sink()


#===============================
# Contact frequencies
#===============================

tab_any_contact <- freq(analysis_df_transformed$any_contact_fct, cum = TRUE) %>% 
  mutate(n = round(n), 'val%' = NULL, 'val%cum' = NULL)

sink("out/tex/serious_search/univariate_analysis/any_contact.tex")
print(xtable(tab_any_contact,
             align = c("l", "r", "r", "r"),
             display = c("s", "d", rep("f", 2))
), 
include.rownames = TRUE,
booktabs = TRUE,
floating = FALSE
)
sink()


tab_mail_form <- freq(analysis_df_transformed$mail_form_fct, cum = TRUE) %>% 
  mutate(n = round(n), 'val%' = NULL, 'val%cum' = NULL)

sink("out/tex/serious_search/univariate_analysis/mail_form.tex")
print(xtable(tab_mail_form,
             align = c("l", "r", "r", "r"),
             display = c("s", "d", rep("f", 2))
), 
include.rownames = TRUE,
booktabs = TRUE,
floating = FALSE
)
sink()


#===============================
# Revisit indicator
#===============================

tab_any_revisit <- freq(analysis_df_transformed$any_revisit_fct, cum = TRUE) %>% 
  mutate(n = round(n), 'val%' = NULL, 'val%cum' = NULL)

sink("out/tex/serious_search/univariate_analysis/any_revisit.tex")
print(xtable(tab_any_revisit,
             align = c("l", "r", "r", "r"),
             display = c("s", "d", rep("f", 2))
), 
include.rownames = TRUE,
booktabs = TRUE,
floating = FALSE
)
sink()



#=====================================
#
# Bivariate analysis
#
#=====================================

retrieve_package("stringr")

col_contact <- c(my_red, my_blue)
col_revisit <- c("tomato3", "turquoise")

dir.create("out/pdf/serious_search/bivariate_analysis/", 
           recursive = TRUE, showWarnings = FALSE)


revisit_by_contact_tabl <- prop.table(
  table(analysis_df_transformed$any_revisit_fct,
        analysis_df_transformed$any_contact_fct), 
  2)   

contact_by_revisit_tabl <- prop.table(
  table(analysis_df_transformed$any_contact_fct,
        analysis_df_transformed$any_revisit_fct), 
  2)  


df1 <- as.data.frame(as.table(contact_by_revisit_tabl))



p1 <- ggplot(df1, aes(Var2, Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = col_contact) +
  labs(x = "Any contact", y = "Proportion", fill = "Any revisit") +
  theme_minimal(base_size = font_size) +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, 
                               size = 10, vjust = 0.5, lineheight = 0.5),
    axis.text.y = element_text(size = 10, lineheight = 0.8),
    legend.text  = element_text(size = 10, lineheight = 0.5)
  )



df2 <- as.data.frame(as.table(revisit_by_contact_tabl))
# df2$Var1 <- str_wrap(df2$Var1, width = 10)
# df2$Var2 <- str_wrap(df2$Var2, width = 10)

p2 <- ggplot(df2, aes(Var2, Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = col_revisit) +
  labs(x = "Any contact", y = "", fill = "Any revisit") +
  theme_minimal(base_size = font_size) +
  theme(
    axis.title.x = element_text(size = 10), 
    axis.title.y = element_text(size = 10), 
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, 
                               size = 10, vjust = 0.5, lineheight = 0.5),
    axis.text.y = element_text(size = 10, lineheight = 0.8),
    legend.text  = element_text(size = 10, lineheight = 0.5)
  )






pdf("out/pdf/serious_search/bivariate_analysis/revisit_vs_contact_plot.pdf", 
    width = 7, height = 7)
p1 + p2 + plot_layout(widths = .5)
dev.off()


{
  
  revisit_by_mail_form_tabl <- prop.table(
    table(analysis_df_transformed$any_revisit_fct,
          analysis_df_transformed$mail_form_fct), 
    2)   # proportions par revisite
  
  contact_by_mail_form_tabl <- prop.table(
    table(analysis_df_transformed$mail_form_fct,
          analysis_df_transformed$any_revisit_fct), 
    2
  )   # proportions par revisite
  
  
  
  tbl_mail_1 <- as.data.frame(as.table(contact_by_mail_form_tabl))
  p_mail_1 <- ggplot(tbl_mail_1, aes(Var2, Freq, fill = Var1)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = col_contact)+
    labs(x = "Any revisit", y = "Proportion", fill = "Mail form")+
    theme_minimal(base_size = font_size) +
    theme(
      axis.title.x = element_text(size = 10), 
      axis.title.y = element_text(size = 10),
      legend.title = element_text(size = 10),
      axis.text.x = element_text(angle = 45, 
                                 size = 10, vjust = 0.5, lineheight = 0.5),
      axis.text.y = element_text(size = 10, lineheight = 0.8),
      legend.text  = element_text(size = 10, lineheight = 0.5)
    )
  
  
  
  tbl_mail_2 <- as.data.frame(as.table(revisit_by_mail_form_tabl))
  p_mail_2 <- ggplot(tbl_mail_2, aes(Var2, Freq, fill = Var1)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = col_revisit)+
    labs(x = "Mail form", y = "", fill = "Any revisit")+
    theme_minimal(base_size = font_size) +
    theme(
      axis.title.x = element_text(size = 10),       
      axis.title.y = element_text(size = 10),      
      legend.title = element_text(size = 10),
      axis.text.x = element_text(angle = 45, 
                                 size = 10, vjust = 0.5, lineheight = 0.5),
      axis.text.y = element_text(size = 10, lineheight = 0.8),
      legend.text  = element_text(size = 10, lineheight = 0.5)
    )
  
 
}


pdf("out/pdf/serious_search/bivariate_analysis/revisit_vs_mail_plot.pdf",
    width = 7, height = 7)
p_mail_1 + p_mail_2 + plot_layout(widths = .5)
dev.off()


pdf("out/pdf/serious_search/bivariate_analysis/revisit_vs_contact_mail_plot.pdf",
    width = 15, height = 7)
(p1 + p2) +  (p_mail_1 + p_mail_2) + plot_layout(widths = .5)
dev.off()


pdf("out/pdf/serious_search/bivariate_analysis/correlation_matrix.pdf",
    width = 5, height = 5)

corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  number.cex = 0.6,
  addCoef.col = "black",
  diag = FALSE
)

dev.off()



#=========================
# Enhanced bivariate plots
#=========================

#https://larmarange.github.io/analyse-R/graphiques-bivaries-ggplot2.html


analysis_df_transformed$any_revisit_fct <- forcats::fct_explicit_na(factor(analysis_df_transformed$any_revisit_fct))
analysis_df_transformed$any_contact_fct <- forcats::fct_explicit_na(factor(analysis_df_transformed$any_contact_fct))
analysis_df_transformed$mail_form_fct <- forcats::fct_explicit_na(factor(analysis_df_transformed$mail_form_fct))


ggplot(analysis_df_transformed) +
  aes(x = any_revisit_fct, fill = any_contact_fct) +
  geom_bar(position = "fill") +
  geom_text(aes(by = any_revisit_fct), stat = "prop", position = position_fill(.5)) +
  scale_fill_manual(values = col_contact)+
  xlab("Revisit") +
  ylab("Proportion") +
  labs(fill = "Contact") +
  scale_y_continuous(labels = scales::percent)+
  theme_minimal(base_size = font_size) +
  theme(
    axis.title.x = element_text(size = 10),        # taille du label X
    axis.title.y = element_text(size = 10),        # taille du label Y (si non vide)
    legend.title = element_text(size = 10),        # taille du titre de légende
    axis.text.x = element_text(angle = 45, size = 10, vjust = 0.5, lineheight = 0.5),
    axis.text.y = element_text(size = 10, lineheight = 0.8),
    legend.text  = element_text(size = 10, lineheight = 0.5)
  )



bivariate_plot <- function(data, var, by_var, 
                           x_lab = "", y_lab = FALSE,
                           legend_name = var, colors) {
  
  g <- ggplot(data, aes(x = {{ by_var }}, fill = {{ var }})) +
    geom_bar(position = "fill") +
    geom_text(aes(by = {{ by_var }}), stat = "prop", position = position_fill(.5), size = 3) +
    scale_fill_manual(values = colors) +
    xlab(x_lab) +
    ylab("") +
    labs(fill = legend_name) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal(base_size = font_size) +
    theme(
      axis.title.x = element_text(size = 10),        # taille du label X
      axis.title.y = element_text(size = 10),        # taille du label Y (si non vide)
      legend.title = element_text(size = 10),        # taille du titre de légende
      axis.text.x = element_text(angle = 45, size = 10, vjust = 0.5, lineheight = 0.5),
      axis.text.y = element_text(size = 10, lineheight = 0.8),
      legend.text  = element_text(size = 10, lineheight = 0.5)
    )
  
  if (y_lab) {
    g <- g + ylab("Proportion")
  }
  
  return(g)
}


# Contact vs revisit

g_contact_1 <- bivariate_plot(
  data = analysis_df_transformed,
  var = any_contact_fct,
  by_var = any_revisit_fct,
  x_lab = "Any revist",
  y_lab = TRUE,
  legend_name = "Any contact",
  colors = col_contact
)


g_contact_2 <- bivariate_plot(
  data = analysis_df_transformed,
  var = any_revisit_fct,
  by_var = any_contact_fct,
  x_lab = "Any contact",
  y_lab = FALSE,
  legend_name = "Any revist",
  colors = col_revisit
)


g_contact_1 + g_contact_2 + plot_layout(widths = .5)



# Mail form vs revisit

g_mail_1 <- bivariate_plot(
  data = analysis_df_transformed,
  var = mail_form_fct,
  by_var = any_revisit_fct,
  x_lab = "Any revist",
  y_lab = TRUE,
  legend_name = "Mail form",
  colors = col_contact
)


g_mail_2 <- bivariate_plot(
  data = analysis_df_transformed,
  var = any_revisit_fct,
  by_var = mail_form_fct,
  x_lab = "Mail form",
  y_lab = FALSE,
  legend_name = "Any revist",
  colors = col_revisit
)


g_mail_1 + g_mail_2 + plot_layout(widths = 0.5)



pdf("out/pdf/serious_search/bivariate_analysis/revisit_vs_contact_plot_1.pdf", width = 9, height = 7)
(g_contact_1 + g_contact_2) + plot_layout(widths = .5)

dev.off()


pdf("out/pdf/serious_search/bivariate_analysis/revisit_vs_mail_plot_1.pdf",
    width = 9, height = 7)
(g_mail_1 + g_mail_2) + plot_layout(widths = .5)

dev.off()


pdf("out/pdf/serious_search/bivariate_analysis/revisit_vs_contact_mail_plot_2.pdf",
    width = 7, height = 7)

(g_contact_1 + g_contact_2) +
  (g_mail_1 + g_mail_2) 

dev.off()




#=================================
# Search categories vs contact
#=================================

prop.table(table(
  analysis_df_transformed_sample$research_category,
  analysis_df_transformed_sample$any_contact_fct
), 
1)


prop.table(table(
  analysis_df_transformed_sample$research_category,
  analysis_df_transformed_sample$mail_form_fct
), 
1)

# research_category vs contact

research_category_vs_contact <- bivariate_plot(
  data = analysis_df_transformed_sample,
  var = any_contact_fct,
  by_var = research_category,
  x_lab = "Any revist",
  y_lab = TRUE,
  legend_name = "Any contact",
  colors = col_contact
)



pdf("out/pdf/serious_search/bivariate_analysis/research_category_vs_contact.pdf",
    width = 7, height = 7)

research_category_vs_contact

dev.off()



# research_category vs mail_form

research_category_vs_mail_form <- bivariate_plot(
  data = analysis_df_transformed_sample,
  var = mail_form_fct,
  by_var = research_category,
  x_lab = "Any revist",
  y_lab = TRUE,
  legend_name = "Mail form",
  colors = col_contact
)



pdf("out/pdf/serious_search/bivariate_analysis/research_category_vs_mail_form.pdf",
    width = 7, height = 7)

research_category_vs_mail_form

dev.off()







#=====================================
#
# Factorial analysis
#
#=====================================



#====================================
# Inertia and Axis description
#====================================


#=====================
# Inertia
#=====================

pdf("out/pdf/serious_search/acp_clustering/inertia_distribution.pdf")

inertia <- ggplot2::ggplot(cbind.data.frame(x = 1:nrow(res.PCA$eig), y = res.PCA$eig[, 2])) +
  ggplot2::aes(x = x, y = y) +
  ggplot2::geom_col(fill = "#5c99ad") +
  ggplot2::xlab("Dimension") +
  ggplot2::ylab("Percentage of variance") +
  ggplot2::ggtitle("Decomposition of the total inertia") +
  ggplot2::theme_light() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::scale_x_continuous(breaks = 1:nrow(res.PCA$eig))+
  theme_classic()

print(inertia)

dev.off()


#=====================
# Axis contribution
#=====================

ax_col <- "#498ea5"

# 1. Calculates maximum contribution value on all three axes
contrib1 <- res.PCA$var$contrib[, 1]
contrib2 <- res.PCA$var$contrib[, 2]
contrib3 <- res.PCA$var$contrib[, 3]
max_y <- ceiling(max(contrib1, contrib2, contrib3) / 5) * 5  # round on the upper 5 

# 2. Generates each graph with the same Y scale

# Axis 1
pdf("out/pdf/serious_search/acp_clustering/axis_1_contrib.pdf")
dim1 <- fviz_contrib(res.PCA,
                     choice = "var",
                     axes = 1, 
                     fill = ax_col,   # #91d2e7
                     color = ax_col,  # #91d2e7
                     xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA$rotation), linetype=2)

  # + ylim(0, max_y)
print(dim1)
dev.off()



# Axis 2
pdf("out/pdf/serious_search/acp_clustering/axis_2_contrib.pdf")
dim2 <- fviz_contrib(res.PCA,
                     choice = "var",
                     axes = 2, 
                     fill = ax_col,   # #91d2e7
                     color = ax_col,  # #91d2e7
                     xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA$rotation), linetype=2)
  # + ylim(0, max_y)

print(dim2)
dev.off()

# Axis 3
pdf("out/pdf/serious_search/acp_clustering/axis_3_contrib.pdf")
dim3 <- fviz_contrib(res.PCA,
                     choice = "var",
                     axes = 3, 
                     fill = ax_col,   # #91d2e7
                     color = ax_col,  # #91d2e7
                     xtickslab.rt = 45) + 
  geom_hline(yintercept = 100/ncol(res.PCA$rotation), linetype=2)
# + ylim(0, max_y)

print(dim3)
dev.off()



pdf("out/pdf/serious_search/acp_clustering/axis.pdf", width = 9, height = 7)

 print(inertia + dim1 + dim2 + dim3)

dev.off()



#=====================
# Factorial plans
#=====================


# Representation quality : first two axis

pdf("out/pdf/serious_search/acp_clustering/res.PCA_12_rep_qual.pdf")

rep_qual <- fviz_pca_var(res.PCA, col.var = "cos2") +
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = 0.5) +
  theme_minimal()

print(rep_qual)
dev.off()






# Correlation circle colored by contribution


pdf("out/pdf/serious_search/acp_clustering/contrib_col_circle_1_2.pdf", width = 6, height = 5.9)

cor_circ12 <- fviz_pca_var(
  res.PCA,
  axes = c(1, 2),
  col.var = "contrib",
  gradient.cols = c("blue", "orange", "red"),
  select.var = list(cos2 = 0.5),
  invisible = "quanti.sup",
  alpha.var = 0.5,
  cex = 0.3,
  repel = TRUE,
  labelsize = 3,   
  title = "Correlation circle colored by contribution") + 
  theme_minimal()

  print(cor_circ12) 

dev.off()



pdf("out/pdf/serious_search/acp_clustering/contrib_col_circle_1_2_simp.pdf", width = 7, height = 5)

cor_circ12_simp <- plot.PCA(res.PCA,
                       choix='var',
                       select='cos2  0.5',
                       unselect=0,
                       alpha.var = 0.5,
                       cex = 0.7,
                       title="Graphe des variables de l'ACP",
                       col.quanti.sup='#0000FF')
print(cor_circ12_simp)
dev.off()


pdf("out/pdf/serious_search/acp_clustering/contrib_col_circle_1_3.pdf")

cor_circ13 <- fviz_pca_var(
  res.PCA,
  axes = c(1, 3),
  col.var = "contrib",
  gradient.cols = c("blue", "orange", "red"),
  select.var = list(cos2 = 0.5),
  invisible = "quanti.sup",
  alpha.var = 0.5,
  cex = 0.5,
  repel = TRUE,
  labelsize = 3, 
  title = "Correlation circle colored by contribution") +
  theme_minimal()

print(cor_circ13) 

dev.off()





pdf("out/pdf/serious_search/acp_clustering/contrib_col_circle_2_3.pdf")

cor_circ23 <- fviz_pca_var(
  res.PCA,
  axes = c(2, 3),
  col.var = "contrib",
  gradient.cols = c("blue", "orange", "red"),
  select.var = list(cos2 = 0.5),
  invisible = "quanti.sup",
  alpha.var = 0.5,
  cex = 0.5,
  repel = TRUE,
  labelsize = 3, 
  title = "Correlation circle colored by contribution") +
  theme_minimal()

print(cor_circ23) 

dev.off()





#=================================
# Plan 12
#=================================


supp_qual_var <- plot.PCA(
  res.PCA,
  invisible = c("ind", "ind.sup"),
  select = "cos2 0.5",
  title = "Qualitative variables",
  label = c("quali"),
  cex = 0.9
)

pdf("out/pdf/serious_search/acp_clustering/factor_plot_12.pdf", width = 13)
print(supp_qual_var)
dev.off()


pdf("out/pdf/serious_search/acp_clustering/quant_qual_var.pdf", width = 13)
print(
  cor_circ12 /supp_qual_var
  + plot_layout(heights = c(12, 7))
)
dev.off()



if(FALSE) {

  
  indplot_all <- fviz_pca_ind(
    res.PCA,
    axes = c(1, 2),
  #  select.ind = list(cos2 = 0.5),
    invisible = "ind.sup",
    alpha.ind  = .2,
    label = "quali",
    repel = TRUE,
    title = "all observations",
    geom = "point",
    pointshape = 21,
    pointsize = .8,
    fill = NA
  )
  
  
  
  
  indplot_filt <- fviz_pca_ind(
    res.PCA,
    axes = c(1, 2),
    select.ind = list(cos2 = 0.5),
    invisible = "ind.sup",
    alpha.ind  = .2,
    label = "quali",
    repel = TRUE,
    title = "well represented (cos2 >= 0.5)",
    geom = "point",
    pointshape = 21,
    pointsize = 0.8,
    fill = NA
  )
  
  
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/indiv_all.pdf")
  print(indplot_all)
  dev.off()
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/indiv_filt.pdf")
  print(indplot_filt)
  dev.off()
  
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/indiv_plan.pdf")
  print(indplot_all + indplot_filt)
  dev.off()
  
  pdf("out/pdf/serious_search/acp_clustering/ind_qual_var.pdf", width = 13)
  print(
    supp_qual_var /
      (indplot_all + indplot_filt) +
      plot_layout(heights = c(6, 4))
  )
  dev.off()
  
}





#print(cor_circ13 + supp_qual_var)




#=====================================
#
# Clustering
#
#=====================================
#library(ggdendro)

#ggtree <- ggdendrogram(res.HCPC_sample$call$t$tree, rotate = FALSE, size = 0.5)

#cluster_color <- c("red", "green", "blue")

cluster_color <- c("black", "red", "green")


if(exists("res.HCPC_sample")) {

  pdf("out/pdf/serious_search/acp_clustering/hierarchica_tree.pdf")
  plot.HCPC(res.HCPC_sample, choice='tree',
            title='Hierarchical tree')
  dev.off()
  
  pdf("out/pdf/serious_search/acp_clustering/hierarchica_tree_pca.pdf")
  plot.HCPC(res.HCPC_sample,
            choice='3D.map',
            ind.names=FALSE,centers.plot=FALSE,angle=60,
            title='Hierarchical tree on the factorial plane 12')
  dev.off()
  
  
  #==========================
  # Plan 12
  #==========================
  
  pdf("out/pdf/serious_search/acp_clustering/cluster_plot_all.pdf")  
    cluster_plot_all <- fviz_pca_ind(
      res.PCA_sample,
      axes = c(1, 2),
      geom.ind = "point",
      habillage = analysis_df_transformed_sample$research_category,
      palette = cluster_color,
  #    addEllipses = TRUE,
      alpha.ind = 0.2,             
      pointshape = 1            
    )
    
  
    print(cluster_plot_all)
  dev.off()  
  
  pdf("out/pdf/serious_search/acp_clustering/cluster_plot_filt.pdf")    
    cluster_plot_filt <- fviz_pca_ind(
      res.PCA_sample,
      axes = c(1, 2),
      select.ind = list(cos2 = 0.5),
      geom.ind = "point",
      habillage = analysis_df_transformed_sample$research_category,
      palette = cluster_color,
  #    addEllipses = TRUE,
      alpha.ind = 0.2,             
      pointshape = 1            
    )
    
    print(cluster_plot_filt)
  dev.off()  
  
  
  cluster_plot_all_noleg <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 2),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
  #  addEllipses = TRUE,
    alpha.ind = 0.2,
    pointshape = 1,
    title = "all observations"
  ) +
    theme(legend.position = "none")
  
  cluster_plot_filt_leg_cust <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 2),
    select.ind = list(cos2 = 0.5),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
  #  addEllipses = TRUE,
    alpha.ind = 0.2,
    pointshape = 1,
    title = "well represented observations"
  ) +
    labs(
      color = "Research category",
      fill  = "Research category"   
    ) +
    theme(
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 12)
    )
  
  
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/clust_both.pdf", width = 15)
    print(
         (cluster_plot_all_noleg + cluster_plot_filt_leg_cust) +
          plot_layout(widths  = 15, heights = 0.5)
    )
  dev.off()
  
  
  
  
  
  #==========================
  # Plan 13
  #==========================
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/hierarchica_tree_pca_13.pdf")
  plot.HCPC(res.HCPC_sample,
            choice='3D.map',
            ind.names=FALSE,centers.plot=FALSE,angle=60,
            axes=c(1,3),
            title='Hierarchical tree on the factorial plane 13')
  dev.off()
  
  
  
  
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/cluster_plot_all_13.pdf")  
  cluster_plot_all_13 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 3),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #    addEllipses = TRUE,
    alpha.ind = 0.2,             
    pointshape = 1            
  )
  
  
  print(cluster_plot_all_13)
  dev.off()  
  
  pdf("out/pdf/serious_search/acp_clustering/cluster_plot_filt_13.pdf")    
  cluster_plot_filt_13 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 3),
    select.ind = list(cos2 = 0.5),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #    addEllipses = TRUE,
    alpha.ind = 0.2,             
    pointshape = 1            
  )
  
  print(cluster_plot_filt_13)
  dev.off()  
  
  
  cluster_plot_all_noleg_13 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 3),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #  addEllipses = TRUE,
    alpha.ind = 0.2,
    pointshape = 1,
    title = "all observations"
  ) +
    theme(legend.position = "none")
  
  cluster_plot_filt_leg_cust_13 <- fviz_pca_ind(
    res.PCA_sample,
    axes = c(1, 3),
    select.ind = list(cos2 = 0.5),
    geom.ind = "point",
    habillage = analysis_df_transformed_sample$research_category,
    palette = cluster_color,
    #  addEllipses = TRUE,
    alpha.ind = 0.2,
    pointshape = 1,
    title = "well represented observations"
  ) +
    labs(
      color = "Research category",
      fill  = "Research category"   
    ) +
    theme(
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 12)
    )
  
  
  
  
  
  pdf("out/pdf/serious_search/acp_clustering/clust_both_13.pdf", width = 15)
  print(
    (cluster_plot_all_noleg_13 + cluster_plot_filt_leg_cust_13) +
      plot_layout(widths  = 15, heights = 0.5)
  )
  dev.off()
  
  
  
  
  
  
  
  #======================================
  # Clsutering quality
  #======================================
  
  
  sil <- cluster::silhouette(as.numeric(res.HCPC_sample$data.clust$clust),
                             dist(res.PCA_sample$ind$coord))
  if (interactive()) plot(sil,
                          main = "Silhouette Widths by Cluster",
                          col = ax_col)
  
  
  pdf("out/pdf/serious_search/acp_clustering/silhouette.pdf", width = 13)
    plot(sil,
         main = "Silhouette widths by cluster",
         col = cluster_color
         )
  dev.off()
  
  aggregate(sil[, 3], 
            list(cluster = as.numeric(res.HCPC_sample$data.clust$clust)), mean)
  
}


#=====================================
#
# Logistique regression visualization
#
#=====================================

#https:/larmarange.github.io/analyse-R/regression-logistique.html#r%C3%A9gression-logistique-binaire

#===============================
#
# Contact model
#
#===============================


retrieve_package("gtsummary")
retrieve_package("gt")
retrieve_package("webshot2")


perform_logit_linearity <- FALSE

dir.create("out/pdf/serious_search/contact_regression",
           recursive = TRUE, showWarnings = FALSE)


if(perform_logit_linearity) {
    
  pdf("out/pdf/serious_search/contact_regression/crplots_modele.pdf")
  
  crPlots(contact_model, ask = FALSE)
  
  dev.off()
  
  
  
  pdf("out/pdf/serious_search/contact_regression/avplots_modele.pdf")
  
  avPlots(contact_model, ask = FALSE)
  
  dev.off()

}


odds.ratio(contact_model)

#library(broom)

#tidy(contact_model, conf.int = TRUE, exponentiate = TRUE)

tidy_plus_plus(contact_model, exponentiate = TRUE)




coeff <- tbl_regression(contact_model, exponentiate = TRUE)


coeff_gt <- as_gt(coeff)

gtsave(coeff_gt, 
       filename = "out/pdf/serious_search/contact_regression/contact_model_coeff.png")


gtsave(coeff_gt, 
       filename = "out/pdf/serious_search/contact_regression/contact_model_coeff.html")




#==========================
# Effect graphics
#==========================

retrieve_package("GGally")
retrieve_package("forestmodel")
retrieve_packages("effects")
retrieve_package("ggeffects")


pdf("out/pdf/serious_search/contact_regression/odds_ratios_plot.pdf", width = 8, height = 5)

ggcoef_model(contact_model, exponentiate = TRUE)

dev.off()



pdf("out/pdf/serious_search/contact_regression/odds_ratios_plot_wth_values.pdf", width = 8, height = 5)
forest_model(contact_model)

dev.off()



# pdf("out/pdf/serious_search/contact_regression/effects_plots.pdf", width = 10, height = 8)
# plot(allEffects(contact_model))
# 
# dev.off()




pdf("out/pdf/serious_search/contact_regression/predicted_propabilities.pdf", width = 11, height = 8)
cowplot::plot_grid(plotlist = plot(ggeffect(contact_model)))
dev.off()






#===============================
#
# Mail form model
#
#===============================



#==========================
# Effect graphics
#==========================


pdf("out/pdf/serious_search/mail_form_regression/mail_form_odds_ratios_plot.pdf", width = 8, height = 5)

ggcoef_model(mail_form_model, exponentiate = TRUE)

dev.off()




pdf("out/pdf/serious_search/mail_form_regression/mail_form_odds_ratios_plot_wth_values.pdf", width = 8, height = 5)
forest_model(mail_form_model)

dev.off()




# pdf("out/pdf/serious_search/mail_form_regression/mail_form_effects_plots.pdf", width = 10, height = 8)
# plot(allEffects(mail_form_model))
# 
# dev.off()


pdf("out/pdf/serious_search/mail_form_regression/mail_form_predicted_propabilities.pdf", width = 11, height = 8)
cowplot::plot_grid(plotlist = plot(ggeffect(mail_form_model)))
dev.off()

