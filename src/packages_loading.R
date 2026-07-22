##############################################################################
# Packages used during the thesis.
# 
# 
#
# source("src/packages_loading.R")
##############################################################################







#===============================================================================
# Function that checks if a package is installed or not. If yes, it loads it 
# into the session otherwise, it downloads it before loading it into memory.
#
# package_name: a string of a package name , which must be enclosed in quotation
#   marks ("").
#===============================================================================


retrieve_package <- function(package_name) {

  # Check if package is already installed
  if (!requireNamespace(package_name, quietly = TRUE)) {

    # Try installing the package
    install.packages(
      package_name,
      dependencies = TRUE,
      verbose = FALSE
    )

    # Check again after installation
    if (requireNamespace(package_name, quietly = TRUE)) {
      cat("Package", package_name, "installed successfully!\n")
    } else {
      cat("ERROR: Package", package_name, "could NOT be installed.\n")
      return(invisible(FALSE))
    }
  }

  # Load the package
  library(package_name, character.only = TRUE, verbose = FALSE)
  invisible(TRUE)
}





#====================================================
#                   Laod packages              
#====================================================

# For general settings
retrieve_package("lubridate") # For date variables
retrieve_package("gridExtra") # For window management



retrieve_package("arrow") # for ".parquet" file loading
retrieve_package("purrr")
retrieve_package("dplyr")
retrieve_package("tidyr")
retrieve_package("knitr")
retrieve_package("stringr")
retrieve_package("questionr")
retrieve_package("readr")
retrieve_package("ggplot2")
retrieve_package("readxl")
retrieve_package("writexl")
retrieve_package("rlang")
retrieve_package("stats")
retrieve_package("skimr")
retrieve_package("entropy") # For entropy computation
retrieve_package("car") # To be checked on the server (not installing)
retrieve_package("corrplot")


# For data manipulation
retrieve_package("data.table")
retrieve_package("future")
retrieve_package("furrr")


#===========================
# For graph object
#===========================
retrieve_package("igraph")




#retrieve_package("tmap")
retrieve_package("plotly")
#retrieve_package("gtsummary")
retrieve_package("broom.helpers")





#===========================
# For spatial analysis
#===========================
retrieve_package("sf") # For cartography
retrieve_package("cartography")
retrieve_package("geosphere")
retrieve_package("ggspatial") # To enhance ggplot2 with cartography
retrieve_package("RColorBrewer") # For color palette management
retrieve_package("classInt") # For variable discretization
retrieve_package("cartograflow") # For contiguity
retrieve_package("spdep") # For Moran's autocorrelation test
retrieve_package("vegan") # For Mantel test and diversity metrics
retrieve_package("ineq") # For Gini index



#===========================
# For statistical modeling
#===========================

retrieve_package("lmtest")
retrieve_package("pROC")
retrieve_package("pscl")
retrieve_package("DescTools")
retrieve_package("ROCR")
retrieve_package("ResourceSelection") # for Hosmer-Lemeshow test
retrieve_package("MASS")
retrieve_package("AER")

#===========================
# PCA and clustering
#===========================

retrieve_package("memoise") # For memory optimisation
retrieve_package("rprojroot")
retrieve_package("stringi") # For string prossessing
retrieve_package("plotly")
retrieve_package("DataExplorer")
retrieve_package("DT")
retrieve_package("Hmisc")
retrieve_package("leaflet")
retrieve_package("moments")
retrieve_package("DescTools") # for normalized kurtosis

retrieve_package("FactoMineR")
#retrieve_package("Factoshiny")
retrieve_package("cluster")
retrieve_package("ClusterR") # For minibatch
retrieve_package("ggdendro")
retrieve_package("distances")  # for big matrices
retrieve_package("fpc")        # for Big data AHC
retrieve_package("ggfortify")  # For Biplot (PCA variable + individuals) plot
retrieve_package("plotly")     # for interactive plots
retrieve_package("GGally")     # for Scatterplot matrix (2D)
retrieve_package("factoextra") # for PCA analysis and visualization

retrieve_package("gridExtra")   # for images combination with ggarrange
library(grid)                   #is a R base package (no install)




#=========================== 
# Cluster number
#===========================

retrieve_package("NbClust")
retrieve_package("clusterCrit")



#=====================================
# User functions
#=====================================

# Load general processing functions
source("src/processing_functions.R")
