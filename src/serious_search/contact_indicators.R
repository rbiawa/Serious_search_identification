##############################################################################
# Compute contact indicators.
# 
# 
#
# source("src/serious_search/contact_indicators.R")
##############################################################################






events <- events_year %>%
  mutate(
    datetime = ymd_h(paste(date, hour)),
    datetime = as.POSIXct(datetime)
  )

# 1) Summarize at the visit level
visits <- events[, .(
  any_logged  = any(is_logged == 1),      # at least one connected event
  any_unlogged = any(is_logged == 0)     # at least one unconnected event
), by = .(fullvisitorid, visitid)]

# 2) Summarize at the user level
visits_summary <- visits[, .(
  nb_visits = .N,
  nb_visits_logged = sum(any_logged),                     # 100% or partially connected visits
  nb_visits_hybrid = sum(any_logged & any_unlogged)       # hybrid visits
), by = fullvisitorid]


rm(visits)

summary(visits_summary$nb_visits)
summary(visits_summary$nb_visits_logged)


events <- merge(events, visits_summary, by = "fullvisitorid", all.x = TRUE)

rm(visits_summary)


#====================
# Serious measure 
#====================



users_attributes <- events[, .(
  nb_visits        = nb_visits,
  nb_listings      = .N,
  nb_visits_logged = nb_visits_logged,
  is_logged        = nb_visits_logged > 0
), by = fullvisitorid][, .SD[1], by = fullvisitorid]


summary(users_attributes$nb_visits)
summary(users_attributes$nb_visits_logged)


freq(users_attributes$is_logged)

t.test(nb_listings ~ is_logged,
       data = users_attributes)


# user_account <- events[, .(
#   nb_listings     = .N,
#   nb_is_logged     = sum(as.numeric(is_logged))
# ), by = fullvisitorid]



#==========================================
#   mail_phone data : serious measure    ##
#==========================================


mail_phone <- read_parquet("in/raw/Se_loger/mail_phone/mail_phone_2021.parquet") %>%
  as.data.table()

mail_phone[, date := as.Date(date, format = "%Y%m%d")]

summary(mail_phone$date)

freq(mail_phone$is_logged)

freq(mail_phone$event_action)

look_for(mail_phone)


if(interactive()) mail_phone[order(fullvisitorid, visitid)] %>% View()



mail_phone <- mail_phone[
  date <= max(events$date),]


action_summary <- mail_phone[, .(
  
  # Number of listings with at least one email sent
  nb_listing_mailed = uniqueN(id_listing[event_action == "mail_form-submitted"]),
  
  # Number of listings with at least one phone number display
  nb_listing_phone_disp = uniqueN(id_listing[event_action == "phone_display-number"])
  
), by = fullvisitorid]

summary(action_summary$nb_listing_phone_disp)
summary(action_summary$nb_listing_mailed)


mail_phone <- merge(mail_phone, action_summary, by = "fullvisitorid", all.x = TRUE)

rm(action_summary)

summary(mail_phone$nb_listing_phone_disp)
summary(mail_phone$nb_listing_mailed)



user_mail_phone_account <- mail_phone[
  , .(
    nb_listings      = .N,
    nb_is_logged     = sum(as.numeric(is_logged)),
    nb_phone_display = sum(event_action == "phone_display-number"),
    nb_mail_form     = sum(event_action == "mail_form-submitted"),
    is_logged        = sum(as.numeric(is_logged)) > 0,
    phone_display    = sum(event_action == "phone_display-number") > 0,
    mail_form        = sum(event_action == "mail_form-submitted") > 0,
    nb_listing_phone_disp = nb_listing_phone_disp,
    nb_listing_mailed     = nb_listing_mailed
  ),
  by = fullvisitorid
][, .SD[1], by = fullvisitorid]


rm(mail_phone)

summary(user_mail_phone_account$nb_mail_form)
summary(user_mail_phone_account$nb_phone_display)
summary(user_mail_phone_account$nb_is_logged)
summary(user_mail_phone_account$nb_is_logged)


# ggplot(data = user_mail_phone_account)+
#   geom_point(aes(x = nb_listings, y = nb_is_logged), alpha = 0.3)


freq(user_mail_phone_account$is_logged)

t.test(nb_listings ~ is_logged,
       data = user_mail_phone_account)

#=========================

freq(user_mail_phone_account$phone_display)

t.test(nb_listings ~ phone_display,
       data = user_mail_phone_account)

#=========================

freq(user_mail_phone_account$mail_form)

t.test(nb_listings ~ mail_form,
       data = user_mail_phone_account)


#==========================================
# user serious indicators
#==========================================

serious_indicator <- merge(
  users_attributes 
  , user_mail_phone_account 
  , by = c("fullvisitorid")
  , all.x = TRUE
)

test <- serious_indicator[,.(fullvisitorid, 
                             nb_listings.x, 
                             nb_listings.y, 
                             diff = nb_listings.x - nb_listings.y)
]

rm(test, users_attributes)

# missing_listing <- liste_1$id_listing[!(liste_1$id_listing %in% liste_2$id_listing)]
# 
# non_missing_listing <- liste_1$id_listing[(liste_1$id_listing %in% liste_2$id_listing)]


events_serious_indicator <- serious_indicator[
  , .(
    fullvisitorid,
    nb_listings      = nb_listings.x,
    nb_visits        = nb_visits,
    nb_visits_logged = nb_visits_logged,
    is_logged     = is_logged.x,
    phone_display = fifelse(is.na(phone_display), FALSE, phone_display),
    mail_form     = fifelse(is.na(mail_form), FALSE, mail_form),
    nb_is_logged  = nb_is_logged,
    nb_phone_display = nb_phone_display,
    nb_mail_form     = nb_mail_form,
    nb_listing_phone_disp = nb_listing_phone_disp,
    nb_listing_mailed     = nb_listing_mailed
    
    
  )
]


rm(serious_indicator)

num_var_list <- c("nb_is_logged", "nb_phone_display", "nb_mail_form", 
                  "nb_listing_phone_disp", "nb_listing_mailed")

events_serious_indicator[, (num_var_list) :=
                           lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
                         .SDcols = num_var_list]


logical_va_list <- c("is_logged", "phone_display", "mail_form")

events_serious_indicator[, (logical_va_list) :=
                           lapply(.SD, function(x) fifelse(is.na(x), FALSE, x)),
                         .SDcols = logical_va_list]


#=======================================
# Save Data
#=======================================

dir.create("out/Rdata/serious_search/", recursive = TRUE, showWarnings = FALSE)

if(TRUE) save(events_serious_indicator,
              file = "out/Rdata/serious_search/events_serious_indicator.RData")
