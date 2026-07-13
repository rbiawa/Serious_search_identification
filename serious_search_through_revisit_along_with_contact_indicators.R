###############################################################################
# Identify serious search with revisit and contact (phone number displaying and
#      mail form submission) indicators.
# 
#
# source("serious_search_through_revisit_along_with_contact_indicators.R")
###############################################################################


#==============================
# Processing parameters
#==============================

# Leave R after processing ? If so, set LEAVE_R to TRUE
LEAVE_R <- FALSE

# Event action label

## for phone call (or number displaying)
PHONE_NUMBER_DISPLAY <- "phone_display-number" 

## For mail form submission
MAIL_FORM_SUBMISSION <- "mail_form-submitted"

# Spatial variable names for department and city
DEP_ID_VARIABLE      <- "dep_ID"
CITY_ID_VARIABLE     <- "sl_insee_city_id"


# Set the sample size for clustering (is set to 100 as default value)
SAMPLE_PROPORTION    <- 100 # in [0,100]










#=========================
# Create directories
#=========================

dir.create("out/parquet/serious_search", 
           recursive = TRUE, showWarnings = FALSE)


#=========================
# Laod packages
#=========================

source("src/packages_loading.R")

#=========================
# Load data
#=========================

source("src/load_data.R")

#============================
# Compute contact indicators
#============================

source("src/serious_search/contact_indicators.R")

#===============================================
# Script to compute listings' revisit indicators 
#   and search variability indicators
#===============================================

source("src/serious_search/serious_search_through_revisit.R")


serious_search_data <- visitor_stats %>% 
  mutate(is_serious = as.logical(any_contact + any_revisit)
         )


arrow::write_parquet(serious_search_data, 
                     "out/parquet/serious_search/serious_search_data.parquet")


#===========================
# Close R ?
#===========================

if (LEAVE_R) q(save = "no", status = 0, runLast = FALSE)
