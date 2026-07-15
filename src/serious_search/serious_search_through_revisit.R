################################################################################
# Script to compute listings' revisit indicators and search variability 
#     indicators
#
# source("src/serious_search/serious_search_through_revisit.R")
################################################################################


check_connectivity <- function(views, contig_graph, user_id, loc_col, contiguous_col = "contiguous") {
  
  start_time    <- Sys.time()
  
  views[, (contiguous_col) := {
    locations <- unique(get(loc_col))  # Extract unique locations dynamically
    
    # Induced subgraph with only the locations viewed by the user
    contig_subgraph <- induced_subgraph(contig_graph, vids = locations)
    
    # Check if the subgraph is connected
    is_connected(contig_subgraph)
  }, by = get(user_id)]
  
  end_time    <- Sys.time()
  (duration <- end_time - start_time)
  print(duration)
  
  return(views)
}



#================================================
# 1. Filter events with features data
#================================================

events2 <- events[id_listing %in% features$id_listing, ] 


users_listing_nb <- events2[, .(n_listings = uniqueN(id_listing)), by = fullvisitorid]

gc()

users_listing_nb <- users_listing_nb[n_listings > 2]


events2 <- events2[fullvisitorid %in% users_listing_nb$fullvisitorid,] 




#================================================
# 2. Join with the characteristics of the listings
#================================================

ev_feat <- events2 %>%
  left_join(features, by = "id_listing")

rm(events2, users_listing_nb)

gc()

#====================================================
# 3. Detection of revisits and calculation of delays
#====================================================


setDT(ev_feat)

ev_revisit <- ev_feat[
  order(fullvisitorid, id_listing, datetime)
][
  , `:=`(
    n_visits_listing = .N,
    is_revisit = .N > 1,
    lag_datetime = shift(datetime)
  ),
  by = .(fullvisitorid, id_listing)
][
  , diff_hours := as.numeric(difftime(datetime, lag_datetime, units = "hours"))
]


rm(ev_feat)

#======================================
# Compute spatial contiguity variable
#======================================

if (!exists("DEP_ID_VARIABLE") || !exists("CITY_ID_VARIABLE")){
  DEP_ID_VARIABLE      <- "dep_ID"
  CITY_ID_VARIABLE      <- "city_ID"
}





ev_revisit <- check_connectivity(ev_revisit
                                   , contig_graph   = dep_contig_graph
                                   , user_id        = "fullvisitorid"
                                   , loc_col        = DEP_ID_VARIABLE
                                   , contiguous_col = "dep_contig")


ev_revisit <- check_connectivity(ev_revisit
                                   , contig_graph   = city_contig_graph
                                   , user_id        = "fullvisitorid"
                                   , loc_col        = CITY_ID_VARIABLE
                                   , contiguous_col = "city_contig")

#================================================
# 4. User indicators : coherence and dispersion
#================================================
{

  setorder(ev_revisit, fullvisitorid, datetime)
  
  #==========================
  # 4.1 Revisit indicators
  #==========================
  
  revisit_indicators  <- ev_revisit[
    , .(
      # =============================
      # 0. Basic counts
      # =============================
      n_events   = .N,
      n_listings = uniqueN(id_listing),
      nb_sessions  = uniqueN(visitid),
      
      # =============================
      # 1. Navigation intensity
      # =============================
      mean_session_size      = .N / uniqueN(visitid),
      mean_click_per_listing = mean(n_visits_listing),
      max_visits_on_listing  = max(n_visits_listing),
      
     
      
      # =============================
      # 2. Sequential behaviour
      # =============================
      n_switches = sum(id_listing != shift(id_listing), na.rm = TRUE),
      
      mean_intervisit_time   = mean(as.numeric(diff(datetime), units = "hours"), na.rm = TRUE),
      median_intervisit_time = median(as.numeric(diff(datetime), units = "hours"), na.rm = TRUE),
      
      # =============================
      # 3. Refined revisit metrics
      # =============================
      n_listings_revisited     = uniqueN(id_listing[is_revisit == TRUE]),
      prop_listings_revisited  = uniqueN(id_listing[is_revisit == TRUE]) / uniqueN(id_listing),
      top1_share = max(n_visits_listing) / .N,
      
      
      n_revisits_24h  = sum(diff_hours <= 24, na.rm = TRUE),
      n_revisits_lt1h = sum(diff_hours <= 1, na.rm = TRUE),
      n_revisits_lt6h = sum(diff_hours <= 6, na.rm = TRUE),
      n_revisits_gt48h = sum(diff_hours > 48, na.rm = TRUE),
      
      mean_revisit_delay   = mean(diff_hours[diff_hours > 0], na.rm = TRUE),
      median_revisit_delay = median(diff_hours[diff_hours > 0], na.rm = TRUE)
      
  ),
    by = fullvisitorid
  ]
  

  
  
  #==========================
  # 4.1 Dispersion indicators
  #==========================  
  
  
  
  visitor_listing <- unique(
    ev_revisit,
    by = c("fullvisitorid", "id_listing")
  )
  
  
  variability_indicators <- visitor_listing[
    
    ,.( 
    
    # =============================
     # 1. Dispersion of qualitative features
     # =============================

     type_simpson = simpson.unb(table(item_type)),
     
     room_count_simpson = simpson.unb(table(fct_room_count)),
     ratio_room_count   = max(room_count, na.rm = TRUE) / min(room_count, na.rm = TRUE),
     
     # =============================
     # 2. Dispersion of quantitative features
     # =============================
     price_entropy = ineq::entropy(price),
     price_sd      = sd(price, na.rm = TRUE),
     ratio_price   = max(price, na.rm = TRUE) / min(price, na.rm = TRUE),
     
     area_entropy = ineq::entropy(area),
     area_sd      = sd(area, na.rm = TRUE),
     ratio_area   = max(area, na.rm = TRUE) / min(area, na.rm = TRUE),
     
     sqm_price_entropy = ineq::entropy(sqm_price),
     sqm_price_sd      = sd(sqm_price, na.rm = TRUE),
     ratio_sqm_price   = max(sqm_price, na.rm = TRUE) / min(sqm_price, na.rm = TRUE),
     
     
     # =============================
     # 3. Geography
     # =============================
     n_city = uniqueN(city_ID),
     n_dep  = uniqueN(dep_ID),
     n_reg  = uniqueN(reg_ID),
     # City diversity
     city_simpson = simpson.unb(table(city_ID)),
     
     
     # Subregion diversity
     dep_simpson = simpson.unb(table(dep_ID)),
     
     
     
     # Locality contiguity
     dep_contig = as.factor(dep_contig),  # Preserve dep contiguity
     
     city_contig = as.factor(city_contig)    
    ),
      by = fullvisitorid]
    
  variability_indicators <- variability_indicators[, .SD[1], by = fullvisitorid] 
    
  #==========================
  # 4.3 Merging indicators
  #==========================      
    
 
  visitor_stats <- merge(
    revisit_indicators,
    variability_indicators,
    by = "fullvisitorid",
    all = TRUE
  )
  

}



rm(events, events2, ev_revisit, visitor_listing, revisit_indicators,
   variability_indicators, features, dep_contig, dep_contig_graph, 
   city_contig, city_contig_graph)

#================================================
# 5. User indicators : add contact indicators
#================================================

visitor_stats <- merge(visitor_stats,
                       events_serious_indicator %>% 
                         dplyr::select(- nb_sessions),
                       , by = c("fullvisitorid")
                       , all.x = TRUE
)


rm(events_serious_indicator)



setDT(visitor_stats)

visitor_stats[, any_contact := as.logical(mail_form + phone_display)]
visitor_stats[, any_action := as.logical(mail_form + phone_display + is_logged)]
visitor_stats[, any_revisit := as.logical(n_listings_revisited)]

# Factors

visitor_stats[, any_action_fct :=
                factor(any_action, labels = c("No action taken", "Action taken"))]

visitor_stats[, any_contact_fct :=
                factor(any_contact, labels = c("No contact made", "Contact made"))]

visitor_stats[, mail_form_fct :=
                factor(mail_form, labels = c("No mail form sent", "Mail form sent"))]

visitor_stats[, phone_display_fct :=
                factor(phone_display, labels = c("No phone displayed", "Phone displayed"))]

visitor_stats[, any_revisit_fct :=
                factor(any_revisit, labels = c("No revisit", "Revisit"))]





