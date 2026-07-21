################################################################################
# Temporality between Revist and Contact
# 
# 
#
# source("src/serious_search/revisit_contact_temporality.R")
################################################################################

if (!exists("PHONE_NUMBER_DISPLAY") || !exists("MAIL_FORM_SUBMISSION")) {
  PHONE_NUMBER_DISPLAY <- "phone_display-number"
  MAIL_FORM_SUBMISSION <- "mail_form-submitted"
}


if(!exists("mail_phone")) {
  mail_phone <- read_parquet("in/mail_phone.parquet") %>%
    as.data.table()
}


if(!exists("events")) {
  events <- read_parquet("in/events.parquet") %>%
    as.data.table()
  
  events <- events %>%
    mutate(
      datetime = as.POSIXct(datetime)
    )
}


# =====================================
# 1. Keep only true contact actions
# =====================================

contact_actions <- c(MAIL_FORM_SUBMISSION, PHONE_NUMBER_DISPLAY)

contacts <- mail_phone %>%
  filter(event_action %in% contact_actions) %>%
  mutate(event_type = "contact") %>%
  dplyr::select(fullvisitorid, visitid, datetime, id_listing, event_type)

# =============================================
# 2. Keep only users who actually contacted
# =============================================

users_with_contact <- unique(contacts$fullvisitorid)

events_clean <- events %>%
  filter(fullvisitorid %in% users_with_contact) %>%
  mutate(event_type = "visit") %>%
  dplyr::select(fullvisitorid, visitid, datetime, id_listing, event_type)

# =============================================================
# 3. Compute revisits ONLY inside events (the reliable source)
#    -> avoids artificial revisits due to hour-only timestamps
#    -> keep ONLY the first real revisit per user
# =============================================================

revisits <- events_clean %>%
  group_by(fullvisitorid, id_listing) %>%
  arrange(datetime) %>%
  mutate(
    visit_rank = row_number(),
    is_revisit = visit_rank > 1,
    revisit_type = case_when(
      is_revisit & visitid == lag(visitid) ~ "intra_session",
      is_revisit & visitid != lag(visitid) ~ "inter_session",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(is_revisit == TRUE) %>%
  arrange(fullvisitorid, datetime) %>%
  group_by(fullvisitorid) %>%
  slice(1) %>%   # === KEEP ONLY THE FIRST REAL REVISIT PER USER ===
  ungroup() %>%
  dplyr::select(fullvisitorid, id_listing, datetime, visitid, revisit_type)

# ===================================================================
# 4. Identify the first contact per user
# ===================================================================

first_contact <- contacts %>%
  group_by(fullvisitorid) %>%
  arrange(datetime, visitid) %>%   # ensure correct ordering when datetime ties
  slice(1) %>%                     # earliest contact (datetime + visitid)
  ungroup() %>%
  dplyr::select(fullvisitorid,
         first_contact_time = datetime,
         first_contact_visitid = visitid)

# ===================================================================
# 5. Compare the FIRST revisit vs. the FIRST contact
# ===================================================================

revisit_vs_contact <- revisits %>%
  left_join(first_contact, by = "fullvisitorid") %>%
  mutate(
    timing_category = case_when(
      
      # === revisite strictly before contact ===
      datetime < first_contact_time ~ "revisit_strictly_before",
      datetime == first_contact_time & visitid < first_contact_visitid ~ "revisit_strictly_before",
      
      # === revisite simultaneous with contact ===
      datetime == first_contact_time & visitid == first_contact_visitid ~ "simultaneous_revisit_and_contact",
      
      # === revisite after contact ===
      datetime > first_contact_time ~ "revisit_after_contact",
      datetime == first_contact_time & visitid > first_contact_visitid ~ "revisit_after_contact",
      
      TRUE ~ NA_character_
    )
  )


freq(revisit_vs_contact$timing_category)

# =====================================================
# 6. Build user-level typology:
#    (1) timing_category: 4 modalities
#    (2) revisit_session_type: intra / inter / none
# =====================================================

timing_by_user <- first_contact %>%
  left_join(revisit_vs_contact, by = "fullvisitorid") %>%
  mutate(
    # === if no revisits at all ===
    timing_category = case_when(
      is.na(timing_category) ~ "no_revisit",
      TRUE ~ timing_category
    ),
    revisit_session_type = replace_na(revisit_type, "none")
  ) %>%
  dplyr::select(fullvisitorid, timing_category, revisit_session_type)

# ================================================
# 7. Final merge: one row per user with contact
# ================================================

contact_behavior <- first_contact %>%
  left_join(timing_by_user, by = "fullvisitorid")

freq(contact_behavior$timing_category)

# ===================================================================
# 8. Save output
# ===================================================================


if(TRUE) save(revisit_vs_contact,
              contact_behavior,
              file = "out/Rdata/serious_search/contact_revisit_behavior.RData")


