#########################################################################################
# Export tables into latex format
# 
# 
#
# source("src/plot/save_tables.R")
#########################################################################################



kable(summary_stats, format = "latex", booktabs = TRUE)

# code to put in overleaf


retrieve_package("xtable")

xtable(summary_stats)


dir.create("out/tables/", recursive = TRUE, showWarnings = FALSE)

print(xtable(summary_stats),
      file = "out/tables/summary_stats.tex",
      include.rownames = FALSE)

#on overleaf:

#\input{summary_stats.tex}

# An enhanced table

retrieve_package("gt")


summary_stats %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  gtsave("out/tables/summary_stats.tex")

# Or format by specific columns

summary_stats %>%
  gt() %>%
  fmt_number(columns = c(mean, sd), decimals = 3) %>%
  fmt_number(columns = c(median, IQR), decimals = 2) %>%
  fmt_number(columns = c(min, max), decimals = 1) %>%
  gtsave("out/tables/summary_stats.tex")



#on overleaf:

#\input{summary_stats.tex}
