#########################################################################################
# Behavioural Temporality: Revist vs Contact
# 
# 
#
# source("src/serious_search/revisit_contact_temporality.R")
#########################################################################################


if (!exists("mail_phone")) {
  mail_phone <- read_parquet("in/raw/Se_loger/mail_phone/mail_phone_2021.parquet") %>%
    as.data.table()
  
  mail_phone[, date := as.Date(date, format = "%Y%m%d")]
}

# ===================================================================
# 0. Reconstruct timestamps (hour-only precision)
# ===================================================================

events <- events_year %>%
  mutate(
    datetime = ymd_h(paste(date, hour)),
    datetime = as.POSIXct(datetime)
  )

mail_phone <- mail_phone %>%
  mutate(
    datetime = ymd_h(paste(date, hour)),
    datetime = as.POSIXct(datetime)
  )

# ===================================================================
# 1. Keep only true contact actions
# ===================================================================

contact_actions <- c("mail_form-submitted", "phone_display-number")

contacts <- mail_phone %>%
  filter(event_action %in% contact_actions) %>%
  mutate(event_type = "contact") %>%
  dplyr::select(fullvisitorid, visitid, datetime, hour, id_listing, event_type)

# ===================================================================
# 2. Keep only users who actually contacted
# ===================================================================

users_with_contact <- unique(contacts$fullvisitorid)

events_clean <- events %>%
  filter(fullvisitorid %in% users_with_contact) %>%
  mutate(event_type = "visit") %>%
  dplyr::select(fullvisitorid, visitid, datetime, hour, id_listing, event_type)

# ===================================================================
# 3. Compute revisits ONLY inside events (the reliable source)
#    → avoids artificial revisits due to hour-only timestamps
#    → keep ONLY the first real revisit per user
# ===================================================================

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
  arrange(datetime, visitid) %>%   # === ensure correct ordering when datetime ties ===
  slice(1) %>%                     # === earliest contact (datetime + visitid) ===
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

# ===================================================================
# 6. Build user-level typology:
#    (1) timing_category: 4 modalities
#    (2) revisit_session_type: intra / inter / none
# ===================================================================

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

# ===================================================================
# 7. Final merge: one row per user with contact
# ===================================================================

contact_behavior <- first_contact %>%
  left_join(timing_by_user, by = "fullvisitorid")

freq(contact_behavior$timing_category)

cat(
  "Pour la majorité des utilisateurs qui contactent, la revisite intervient au minimum pendant le premier contact.\n",
  "Dans près de deux cas sur trois, elle précède ou accompagne directement la prise de contact.\n",
  "Et plus de trois utilisateurs sur quatre revisitent au moins une fois, ce qui fait de la revisite un comportement quasi nécessaire à l’entrée en relation."
)
# ===================================================================
# 8. Save output
# ===================================================================

dir.create("out/Rdata/serious_search/", recursive = TRUE, showWarnings = FALSE)

if(TRUE) save(events, revisits,
              contact_behavior,
              file = "out/Rdata/serious_search/contact_revisit_all_24032026.RData")


if(TRUE) save(revisit_vs_contact,
              contact_behavior,
              file = "out/Rdata/serious_search/contact_revisit_behavior.RData")


q(save = "no", status = 0, runLast = FALSE)
