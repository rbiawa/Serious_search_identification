################################################################################
# A script to analyze the revisiting temporality
# 
# 
#
# source("src/serious_search/revisit_time_analysis.R")
################################################################################

retrieve_package("xtable")

plot_num_var <- function(var) {
  
  # Statistics
  quartiles <- quantile(var, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  mean_val  <- mean(var, na.rm = TRUE)
  
  df <- data.frame(x = var)
  
  # Density
  p1 <- ggplot(df, aes(x = x)) +
    geom_density(fill = "lightgray") +
    geom_vline(xintercept = quartiles, color = "blue", linetype = "dashed") +
    geom_vline(xintercept = mean_val, color = "red") +
    labs(x = "Variable", y = "Densité") +
    theme_minimal()
  
  # Violon + boxplot
  p2 <- ggplot(df, aes(x = "", y = x)) +
    geom_violin(fill = "lightblue") +
    geom_boxplot(width = 0.2, fill = "white") +
    labs(y = "Variable") +
    theme_minimal()
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}



#============================================
# 1. Time by session
#============================================


if(!exists("events")) {
  events <- read_parquet("in/events.parquet") %>%
    as.data.table()
  
  events <- events %>%
    mutate(
      datetime = as.POSIXct(datetime)
    )
}



visits <- events[
  , .(
    start_visit = min(datetime),
    end_visit   = max(datetime),
    duration_sec = as.numeric(max(datetime) - min(datetime)),
    duration_min = as.numeric(max(datetime) - min(datetime)) / 60,
    n_events = .N
  ),
  by = .(fullvisitorid, visitid)
]

visits$duration_hour <- visits$duration_min/60

summary(visits$duration_min)

summary(visits$duration_hour)


#====================================================================
# Compute listing revisits using data.table
# ---------------------------------------------------------------
# This chunk identifies revisits of the same listing by the same user,
# classifies them as intra‑session or inter‑session, and computes the
# time elapsed between successive consultations.
#====================================================================

# Sort events so that successive consultations are correctly ordered
setorder(events, fullvisitorid, id_listing, datetime)

# Compute the visit rank for each (user, listing) pair
events[
  , visit_rank := seq_len(.N),
  by = .(fullvisitorid, id_listing)
]

# Flag revisits (any rank > 1)
events[
  , is_revisit := visit_rank > 1
]

# Classify revisits:
# - intra_session: same visitid as previous consultation
# - inter_session: different visitid from previous consultation
events[
  , revisit_type := fifelse(
    is_revisit & visitid == shift(visitid),
    "intra_session",
    fifelse(
      is_revisit & visitid != shift(visitid),
      "inter_session",
      NA_character_
    )
  ),
  by = .(fullvisitorid, id_listing)
]

# Compute time elapsed since previous consultation of the same listing
events[
  , time_since_last := as.numeric(datetime - shift(datetime), units = "secs"),
  by = .(fullvisitorid, id_listing)
]

#====================================================================
# The table 'events' now contains:
# - visit_rank: order of consultations for each listing
# - is_revisit: TRUE/FALSE
# - revisit_type: intra_session / inter_session / NA
# - time_since_last: seconds since previous consultation
#====================================================================


if(TRUE) save(visits, events,
              file = "out/Rdata/serious_search/visit_and_revisit_time.RData")



#=====================================
# Revisit temporality
#=====================================


revisits <- events[is_revisit == TRUE]


revisits[, time_since_last_hour := time_since_last/3600]

summary(revisits$time_since_last_hour)

summary <-  revisits %>%
  dplyr::select(time_since_last_hour) %>% 
  summarise(
    min    = min(time_since_last_hour, na.rm = TRUE),
    Q1     = quantile(time_since_last_hour, .25, na.rm = TRUE),
    median = median(time_since_last_hour, na.rm = TRUE),
    mean   = mean(time_since_last_hour, na.rm = TRUE),
    sd     = sd(time_since_last_hour, na.rm = TRUE),
    Q3     = quantile(time_since_last_hour, .75, na.rm = TRUE),
    max    = max(time_since_last_hour, na.rm = TRUE)
) 

row.names(summary) <- "inter_revisit_time"

if (interactive() & exists("plot_num_var")) plot_num_var(revisits$time_since_last_hour)



# save table in .tex format

sink("out/tex/serious_search/univariate_analysis/inter_revisit_time.tex")
print(
  xtable::xtable(summary,
                 align = c(rep("l", 1), rep("r", 7)),
                 display = c(rep("s", 1), rep("f", 7))
  ),
  include.rownames = FALSE,   # ou FALSE selon ton besoin
  booktabs = TRUE,            # joli rendu LaTeX
  floating = FALSE   # <-- crucial: no table environment
)
sink()



#=====================================
# Session duration
#=====================================


session <- visits %>%
  dplyr::select(duration_hour, n_events) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>% 
  summarise(
    min    = min(value, na.rm = TRUE),
    Q1     = quantile(value, .25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    Q3     = quantile(value, .75, na.rm = TRUE),
    max    = max(value, na.rm = TRUE)
  )


# save table in .tex format

sink("out/tex/serious_search/univariate_analysis/session_time.tex")
print(
  xtable::xtable(session,
                 align = c(rep("l", 2), rep("r", 7)),
                 display = c(rep("s", 2), rep("f", 7))
  ),
  include.rownames = FALSE, 
  booktabs = TRUE,            
  floating = FALSE   
)
sink()