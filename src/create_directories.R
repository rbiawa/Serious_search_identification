#########################################################################################
# A script to create directories
# 
# 
#
# source("src/create_directories.R")
#########################################################################################


#====================
# PDF
#====================

dir.create("out/pdf/serious_search/univariate_analysis/", 
           recursive = TRUE, showWarnings = FALSE)

dir.create("out/pdf/serious_search/bivariate_analysis/", 
           recursive = TRUE, showWarnings = FALSE)

dir.create("out/pdf/serious_search/acp_clustering/", 
           recursive = TRUE, showWarnings = FALSE)

dir.create("out/pdf/serious_search/contact_regression/", 
           recursive = TRUE, showWarnings = FALSE)

dir.create("out/pdf/serious_search/mail_form_regression/", 
           recursive = TRUE, showWarnings = FALSE)



#====================
# Tex
#====================

dir.create("out/tex/serious_search/univariate_analysis/", 
           recursive = TRUE, showWarnings = FALSE)

dir.create("out/tex/serious_search/bivariate_analysis/", 
           recursive = TRUE, showWarnings = FALSE)

dir.create("out/tex/serious_search/acp_clustering/", 
           recursive = TRUE, showWarnings = FALSE)

dir.create("out/tex/serious_search/contact_regression/", 
           recursive = TRUE, showWarnings = FALSE)

dir.create("out/tex/serious_search/mail_form_regression/", 
           recursive = TRUE, showWarnings = FALSE)



#====================
# RData
#====================

dir.create("out/Rdata/serious_search/", 
           recursive = TRUE, showWarnings = FALSE)
