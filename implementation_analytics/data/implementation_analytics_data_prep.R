# This script produces all the data files required for Implementation Analytics Dashboard

library(tidyverse)
library(lubridate)
library(magrittr)
library(rstudioapi)

load(file.path(getActiveProject(), 'data_prep', 'raw_event_data.RData'))
load(file.path(getActiveProject(), 'data_prep', 'raw_item_response_data.RData'))
folder_path <- file.path(getActiveProject(), 'implementation_analytics', 'data')

#------GENERAL DATA FOR ALL TABS------

districts <- event_data_2 %$%
  unique(districtname) %>%
  str_to_title() %>%
  gtools::mixedsort()

subjects <- event_data_2 %$%
  unique(subject)

grade_bands <- event_data_2 %$%
  unique(grade_band)

district_subjects <- event_data_2 %>%
  distinct(districtname, subject) %>%
  mutate(districtname = str_to_title(districtname))

district_grade_bands <- event_data_2 %>%
  distinct(districtname, grade_band) %>%
  mutate(districtname = str_to_title(districtname))

district_products <- event_data_2 %>%
  distinct(districtname, product) %>%
  mutate(districtname = str_to_title(districtname))

save(
  districts,
  subjects,
  grade_bands,
  district_subjects,
  district_grade_bands,
  district_products,
  file = file.path(folder_path,'general_data.RData'))


#-------OVERALL USAGE TAB DATA PREP-----

#-----infobox data prep----

ou_infobox_rdf <- event_data_2 %>%
  group_by(districtname) %>%
  summarise(n_products = n_distinct(product),
            n_product_groups = n_distinct(product_group),
            n_classes = n_distinct(classname),
            n_schools = n_distinct(schoolname),
            n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher'])) %>%
  mutate(districtname = str_to_title(districtname))

#------overall usage pieplot data prep----

ou_n_users_rdf <- event_data_2 %>%
  group_by(districtname, userroletype) %>%
  summarise(tot_users = n_distinct(userid),
            tot_sessions = n_distinct(sessionid)) %>%
  ungroup()

ou_subject_wise_rdf <- event_data_2 %>%
  group_by(districtname,subject,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  left_join(ou_n_users_rdf, by = c('districtname', 'userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname))

ou_grade_band_wise_filter_none_rdf <- event_data_2 %>%
  group_by(districtname,grade_band,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  ungroup() %>%
  left_join(ou_n_users_rdf, by = c('districtname', 'userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>%
  mutate(districtname = str_to_title(districtname))

ou_grade_band_wise_filter_subject_rdf <- event_data_2 %>%
  group_by(districtname,subject,grade_band,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname)) %>%
  left_join(ou_subject_wise_rdf %>%
              dplyr::select(districtname, subject, userroletype, tot_users = n_users, tot_sessions = n_sessions),
            by = c('districtname','subject','userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2))
  
ou_product_group_wise_filter_none_rdf <- event_data_2 %>%
  group_by(districtname,product_group,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  ungroup() %>%
  left_join(ou_n_users_rdf, by = c('districtname', 'userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>%
  mutate(districtname = str_to_title(districtname))

ou_product_group_wise_filter_subject_rdf <- event_data_2 %>%
  group_by(districtname,subject,product_group,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname)) %>%
  left_join(ou_subject_wise_rdf %>% dplyr::select(districtname, subject, userroletype, tot_users = n_users, tot_sessions = n_sessions),
            by = c('districtname','subject','userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2))

ou_product_group_wise_filter_grade_band_rdf <- event_data_2 %>%
  group_by(districtname,grade_band,product_group,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname)) %>%
  left_join(ou_grade_band_wise_filter_none_rdf %>% dplyr::select(districtname, grade_band, userroletype, tot_users = n_users, tot_sessions = n_sessions),
            by = c('districtname','grade_band','userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2))


#----active weeks data prep----

ou_active_weeks_abs_rdf <- event_data_2 %>%
  group_by(districtname, userroletype, userid) %>%
  summarise(active_weeks = n_distinct(date_binned_by_week)) %>%
  ungroup() %>%
  mutate(act_weeks_label = case_when(active_weeks<=8 ~ '2 month or less',
                                between(active_weeks,9,12) ~ '2 to 3 months',
                                between(active_weeks,13,17) ~ '3 to 4 months',
                                between(active_weeks,18,25) ~ '4 to 6 months',
                                active_weeks>=26 ~ 'More than 6 months')) %>%
  group_by(districtname,userroletype,act_weeks_label) %>%
  summarise(n_users = n()) %>%
  ungroup() %>%
  left_join(ou_n_users_rdf %>% dplyr::select(-tot_sessions) , by = c('districtname', 'userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         color = case_when(act_weeks_label == '2 month or less' ~ '#ff2e2e',
                           act_weeks_label == '2 to 3 months' ~ '#ff8d30',
                           act_weeks_label == '3 to 4 months' ~ '#fff240',
                           act_weeks_label == '4 to 6 months' ~ '#ccff40',
                           act_weeks_label == 'More than 6 months' ~ '#4bff2b'),
         act_weeks_label = factor(act_weeks_label,
                             levels = c('2 month or less','2 to 3 months','3 to 4 months',
                                        '4 to 6 months','More than 6 months')),
         districtname = str_to_title(districtname)) %>%
  arrange(act_weeks_label)


ou_active_weeks_perc_rdf <- event_data_2 %>%
  group_by(districtname, userroletype, userid) %>%
  summarise(active_weeks = n_distinct(date_binned_by_week)) %>%
  ungroup() %>%
  mutate(perc_active_weeks = round(100*(active_weeks/52),2),
         act_weeks_label = case_when(perc_active_weeks<=20 ~ '20 percent or less',
                                between(perc_active_weeks,20.01,30) ~ '20 to 30 percent',
                                between(perc_active_weeks,30.01,40) ~ '30 to 40 percent',
                                between(perc_active_weeks,40.01,50) ~ '40 to 50 percent',
                                perc_active_weeks>=50.01 ~ 'More than 50 percent')) %>%
  group_by(districtname,userroletype,act_weeks_label) %>%
  summarise(n_users = n()) %>%
  ungroup() %>%
  left_join(ou_n_users_rdf %>% dplyr::select(-tot_sessions) , by = c('districtname', 'userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         color = case_when(act_weeks_label == '20 percent or less' ~ '#ff2e2e',
                           act_weeks_label == '20 to 30 percent' ~ '#ff8d30',
                           act_weeks_label == '30 to 40 percent' ~ '#fff240',
                           act_weeks_label == '40 to 50 percent' ~ '#ccff40',
                           act_weeks_label == 'More than 50 percent' ~ '#4bff2b'),
         act_weeks_label = factor(act_weeks_label,
                             levels = c('20 percent or less','20 to 30 percent','30 to 40 percent',
                                        '40 to 50 percent','More than 50 percent')),
         districtname = str_to_title(districtname)) %>%
  arrange(act_weeks_label)

save(ou_infobox_rdf,
     ou_subject_wise_rdf,
     ou_grade_band_wise_filter_none_rdf,
     ou_grade_band_wise_filter_subject_rdf,
     ou_product_group_wise_filter_none_rdf,
     ou_product_group_wise_filter_subject_rdf,
     ou_product_group_wise_filter_grade_band_rdf,
     ou_active_weeks_abs_rdf,
     ou_active_weeks_perc_rdf,
     file = file.path(folder_path, 'overall_usage_tab_data.RData'))


#-------USAGE OVER TIME TAB DATA PREP-----

#----top used pg's----

uot_districtwise_top_pg_rdf <- event_data_2 %>%
  group_by(districtname, product_group) %>%
  summarise(n_users = n_distinct(userid)) %>%
  arrange(desc(n_users)) %>%
  filter(row_number()<=4) %>%
  ungroup() %>%
  dplyr::select(-n_users)

#-----usage over time plot-----
#res:month

uot_overall_by_month_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date_month == floor_date(min(date), unit = 'month')),
         last = (date_month == floor_date(max(date), unit = 'month'))) %>%
  ungroup() %>%
  group_by(districtname, userroletype, date_month) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Month')

uot_subject_by_month_rdf <- event_data_2 %>%
  group_by(subject, userid) %>%
  mutate(first = (date_month == floor_date(min(date), unit = 'month')),
         last = (date_month == floor_date(max(date), unit = 'month'))) %>%
  ungroup() %>%
  group_by(districtname, subject, userroletype, date_month) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Month')

uot_grade_band_by_month_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date_month == floor_date(min(date), unit = 'month')),
         last = (date_month == floor_date(max(date), unit = 'month'))) %>%
  ungroup() %>%
  group_by(districtname, grade_band, userroletype, date_month) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Month')

uot_top_4_product_group_by_month_rdf <- event_data_2 %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(product_group, userid) %>%
  mutate(first = (date_month == floor_date(min(date), unit = 'month')),
         last = (date_month == floor_date(max(date), unit = 'month'))) %>%
  ungroup() %>%
  group_by(districtname, product_group, userroletype, date_month) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Month')

#res:week

uot_overall_by_week_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(districtname, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Week')

uot_subject_by_week_rdf <- event_data_2 %>%
  group_by(subject, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(districtname, subject, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Week')

uot_grade_band_by_week_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(districtname, grade_band, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Week')

uot_top_4_product_group_by_week_rdf <- event_data_2 %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(product_group, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(districtname, product_group, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Week')

#Res:Day

uot_overall_by_day_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>%
  ungroup() %>%
  group_by(districtname, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Day')

uot_subject_by_day_rdf <- event_data_2 %>%
  group_by(subject, userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>%
  ungroup() %>%
  group_by(districtname, subject, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Day')

uot_grade_band_by_day_rdf <- event_data_2 %>%
  group_by(grade_band, userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>% 
  ungroup() %>%
  group_by(districtname, grade_band, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Day')

uot_top_4_product_group_by_day_rdf <- event_data_2 %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(product_group, userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>%
  ungroup() %>%
  group_by(districtname, product_group, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Day')


uot_overall_rdf <- bind_rows(uot_overall_by_day_rdf %>% rename(dateid = date),
                             uot_overall_by_month_rdf %>% rename(dateid = date_month),
                             uot_overall_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  mutate(tot_users = new + return,
         districtname = str_to_title(districtname))


uot_subject_rdf <- bind_rows(uot_subject_by_day_rdf %>% rename(dateid = date),
                             uot_subject_by_month_rdf %>% rename(dateid = date_month),
                             uot_subject_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  mutate(tot_users = new + return,
         districtname = str_to_title(districtname))

uot_grade_band_rdf <- bind_rows(uot_grade_band_by_day_rdf %>% rename(dateid = date),
                                uot_grade_band_by_month_rdf %>% rename(dateid = date_month),
                                uot_grade_band_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  mutate(tot_users = new + return,
         districtname = str_to_title(districtname))


uot_top_4_product_group_rdf <- bind_rows(uot_top_4_product_group_by_day_rdf %>% rename(dateid = date),
                                         uot_top_4_product_group_by_month_rdf %>% rename(dateid = date_month),
                                         uot_top_4_product_group_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  mutate(tot_users = new + return,
         districtname = str_to_title(districtname))

#----tot users data prep----

uot_overall_tot_users_rdf <- event_data_2 %>%
  group_by(districtname, userroletype) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()

uot_subjectwise_tot_users_rdf <- event_data_2 %>%
  group_by(districtname, userroletype, subject) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()

uot_gbwise_tot_users_rdf <- event_data_2 %>%
  group_by(districtname, userroletype, grade_band) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()

uot_pgwise_tot_users_rdf <- event_data_2 %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(districtname, userroletype, product_group) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()


#-------avg session duration data prep-----

session_duration_rdf <- event_data_2 %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(sessionid) %>%
  mutate(session_duration = as.numeric(difftime(max(timestamp), min(timestamp), unit = 'mins'))) %>%
  ungroup() %>%
  distinct(sessionid, session_duration, districtname, subject, grade_band, product_group, product, userroletype, date, date_binned_by_week, date_month)

#usage type:overall

uot_asd_overall_by_month_rdf <- session_duration_rdf %>%
  group_by(districtname, userroletype, date_month) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date_month) %>%
  mutate(res = 'Month')

uot_asd_overall_by_week_rdf <- session_duration_rdf %>%
  group_by(districtname, userroletype, date_binned_by_week) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date_binned_by_week) %>%
  mutate(res = 'Week')

uot_asd_overall_by_day_rdf <- session_duration_rdf %>%
  group_by(districtname, userroletype, date) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date) %>%
  mutate(res = 'Day')

#usage type:subject

uot_asd_subject_by_month_rdf <- session_duration_rdf %>%
  group_by(districtname, subject, userroletype, date_month) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date_month) %>%
  mutate(res = 'Month')

uot_asd_subject_by_week_rdf <- session_duration_rdf %>%
  group_by(districtname, subject, userroletype, date_binned_by_week) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date_binned_by_week) %>%
  mutate(res = 'Week')

uot_asd_subject_by_day_rdf <- session_duration_rdf %>%
  group_by(districtname, subject, userroletype, date) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date) %>%
  mutate(res = 'Day')

#usage type:grade band

uot_asd_grade_band_by_month_rdf <- session_duration_rdf %>%
  group_by(districtname, grade_band, userroletype, date_month) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date_month) %>%
  mutate(res = 'Month')

uot_asd_grade_band_by_week_rdf <- session_duration_rdf %>%
  group_by(districtname, grade_band, userroletype, date_binned_by_week) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date_binned_by_week) %>%
  mutate(res = 'Week')

uot_asd_grade_band_by_day_rdf <- session_duration_rdf %>%
  group_by(districtname, grade_band, userroletype, date) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date) %>%
  mutate(res = 'Day')

#usage type:product group

uot_asd_top_4_product_group_by_month_rdf <- session_duration_rdf %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(districtname, product_group, userroletype, date_month) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date_month) %>%
  mutate(res = 'Month')

uot_asd_top_4_product_group_by_week_rdf <- session_duration_rdf %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(districtname, product_group, userroletype, date_binned_by_week) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date_binned_by_week) %>%
  mutate(res = 'Week')

uot_asd_top_4_product_group_by_day_rdf <- session_duration_rdf %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(districtname, product_group, userroletype, date) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  rename(dateid = date) %>%
  mutate(res = 'Day')


uot_asd_overall_rdf <- bind_rows(uot_asd_overall_by_day_rdf,
                                 uot_asd_overall_by_month_rdf,
                                 uot_asd_overall_by_week_rdf) %>%
  mutate(districtname = str_to_title(districtname)) %>%
  inner_join(y = uot_overall_rdf %>%
               dplyr::select(districtname, userroletype, dateid, res, tot_users), 
             by = c('districtname','userroletype', 'dateid', 'res')) %>%
  rename(n_users = tot_users) %>%
  left_join(y = uot_overall_tot_users_rdf %>%
              mutate(districtname = str_to_title(districtname)),
            by = c('districtname', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2))


uot_asd_subject_rdf <- bind_rows(uot_asd_subject_by_day_rdf,
                                 uot_asd_subject_by_month_rdf,
                                 uot_asd_subject_by_week_rdf) %>%
  mutate(districtname = str_to_title(districtname)) %>%
  inner_join(y = uot_subject_rdf %>%
               dplyr::select(districtname, subject, userroletype, dateid, res, tot_users), 
             by = c('districtname', 'subject', 'userroletype', 'dateid', 'res')) %>%
  rename(n_users = tot_users) %>%
  left_join(y = uot_subjectwise_tot_users_rdf %>%
              mutate(districtname = str_to_title(districtname)),
            by = c('districtname', 'subject', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2))

uot_asd_grade_band_rdf <- bind_rows(uot_asd_grade_band_by_day_rdf,
                                    uot_asd_grade_band_by_month_rdf,
                                    uot_asd_grade_band_by_week_rdf) %>%
  mutate(districtname = str_to_title(districtname)) %>%
  inner_join(y = uot_grade_band_rdf %>%
               dplyr::select(districtname, grade_band, userroletype, dateid, res, tot_users), 
             by = c('districtname', 'grade_band', 'userroletype', 'dateid', 'res')) %>%
  rename(n_users = tot_users) %>%
  left_join(y = uot_gbwise_tot_users_rdf %>%
              mutate(districtname = str_to_title(districtname)),
            by = c('districtname', 'grade_band', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2))


uot_asd_top_4_product_group_rdf <- bind_rows(uot_asd_top_4_product_group_by_day_rdf,
                                             uot_asd_top_4_product_group_by_month_rdf,
                                             uot_asd_top_4_product_group_by_week_rdf) %>%
  mutate(districtname = str_to_title(districtname)) %>%
  inner_join(y = uot_top_4_product_group_rdf %>%
               dplyr::select(districtname, product_group, userroletype, dateid, res, tot_users), 
             by = c('districtname', 'product_group', 'userroletype', 'dateid', 'res')) %>%
  rename(n_users = tot_users) %>%
  left_join(y = uot_pgwise_tot_users_rdf %>%
              mutate(districtname = str_to_title(districtname)),
            by = c('districtname', 'product_group', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2))



#------events per user data prep-----

#usage type:overall

uot_aeu_overall_by_month_rdf <- event_data_2 %>%
  group_by(districtname, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date_month) %>%
  mutate(res = 'Month')

uot_aeu_overall_by_week_rdf <- event_data_2 %>%
  group_by(districtname, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date_binned_by_week) %>%
  mutate(res = 'Week')

uot_aeu_overall_by_day_rdf <- event_data_2 %>%
  group_by(districtname, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date) %>%
  mutate(res = 'Day')


#usage type:subject

uot_aeu_subject_by_month_rdf <- event_data_2 %>%
  group_by(districtname, subject, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date_month) %>%
  mutate(res = 'Month')

uot_aeu_subject_by_week_rdf <- event_data_2 %>%
  group_by(districtname, subject, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date_binned_by_week) %>%
  mutate(res = 'Week')

uot_aeu_subject_by_day_rdf <- event_data_2 %>%
  group_by(districtname, subject, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date) %>%
  mutate(res = 'Day')

#usage type:grade band

uot_aeu_grade_band_by_month_rdf <- event_data_2 %>%
  group_by(districtname, grade_band, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date_month) %>%
  mutate(res = 'Month')

uot_aeu_grade_band_by_week_rdf <- event_data_2 %>%
  group_by(districtname, grade_band, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date_binned_by_week) %>%
  mutate(res = 'Week')

uot_aeu_grade_band_by_day_rdf <- event_data_2 %>%
  group_by(districtname, grade_band, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date) %>%
  mutate(res = 'Day')

#usage type: product groups

uot_aeu_top_4_product_group_by_month_rdf <- event_data_2 %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(districtname, product_group, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date_month) %>%
  mutate(res = 'Month')

uot_aeu_top_4_product_group_by_week_rdf <- event_data_2 %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(districtname, product_group, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date_binned_by_week) %>%
  mutate(res = 'Week')

uot_aeu_top_4_product_group_by_day_rdf <- event_data_2 %>%
  semi_join(y = uot_districtwise_top_pg_rdf, by = c('districtname','product_group')) %>%
  group_by(districtname, product_group, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  rename(dateid = date) %>%
  mutate(res = 'Day')

uot_aeu_overall_rdf <- bind_rows(uot_aeu_overall_by_day_rdf,
                                 uot_aeu_overall_by_month_rdf,
                                 uot_aeu_overall_by_week_rdf) %>%
  left_join(y = uot_overall_tot_users_rdf, by = c('districtname', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2),
         districtname = str_to_title(districtname))


uot_aeu_subject_rdf <- bind_rows(uot_aeu_subject_by_day_rdf %>% mutate(res = 'Day'),
                                 uot_aeu_subject_by_month_rdf %>% mutate(res = 'Month'),
                                 uot_aeu_subject_by_week_rdf %>% mutate(res = 'Week')) %>%
  left_join(y = uot_subjectwise_tot_users_rdf, by = c('districtname', 'userroletype', 'subject')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2),
         districtname = str_to_title(districtname))

uot_aeu_grade_band_rdf <- bind_rows(uot_aeu_grade_band_by_day_rdf %>% mutate(res = 'Day'),
                                    uot_aeu_grade_band_by_month_rdf %>% mutate(res = 'Month'),
                                    uot_aeu_grade_band_by_week_rdf %>% mutate(res = 'Week')) %>%
  left_join(y = uot_gbwise_tot_users_rdf, by = c('districtname', 'userroletype', 'grade_band')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2),
         districtname = str_to_title(districtname))


uot_aeu_top_4_product_group_rdf <- bind_rows(uot_aeu_top_4_product_group_by_day_rdf %>% mutate(res = 'Day'),
                                             uot_aeu_top_4_product_group_by_month_rdf %>% mutate(res = 'Month'),
                                             uot_aeu_top_4_product_group_by_week_rdf %>% mutate(res = 'Week')) %>%
  left_join(y = uot_pgwise_tot_users_rdf, by = c('districtname', 'userroletype', 'product_group')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2),
         districtname = str_to_title(districtname))

uot_districtwise_top_pg_rdf <- uot_districtwise_top_pg_rdf %>%
  mutate(districtname = str_to_title(districtname))

save(uot_districtwise_top_pg_rdf,
     uot_overall_rdf,
     uot_subject_rdf,
     uot_grade_band_rdf,
     uot_top_4_product_group_rdf,
     uot_asd_overall_rdf,
     uot_asd_subject_rdf,
     uot_asd_grade_band_rdf,
     uot_asd_top_4_product_group_rdf,
     uot_aeu_overall_rdf,
     uot_aeu_subject_rdf,
     uot_aeu_grade_band_rdf,
     uot_aeu_top_4_product_group_rdf,
     file = file.path(folder_path, 'usage_over_time_tab_data.RData'))

#-----CURRICULUM INSIGHTS TAB DATA PREP-----

#----district wise tot users data prep-----

ci_district_product_wise_tot_users <- event_data_2 %>%
  group_by(districtname, product, userroletype) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname))

#------usage over time plots data prep-----
#plot type: nvr
ci_nvr_rdf <- event_data_2 %>%
  group_by(product, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(districtname, product, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname),
         tot_users = new + return) 

#plot type: asd
ci_asd_rdf <- session_duration_rdf %>%
  group_by(districtname, product, userroletype, date_binned_by_week) %>%
  summarise(avg_duration = round(mean(session_duration),2)) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname)) %>%
  inner_join(ci_nvr_rdf %>% dplyr::select(-new,-leaving,-return,-bouncing), 
             by = c('districtname','product','userroletype','date_binned_by_week')) %>%
  rename(n_users = tot_users) %>%
  left_join(ci_district_product_wise_tot_users, by = c('districtname','product','userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2))

#plot type: aeu
ci_aeu_rdf <- event_data_2 %>%
  group_by(districtname, product, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname)) %>%
  left_join(ci_district_product_wise_tot_users, by = c('districtname','product','userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2))

#----curriculum pacing plot data prep------

#labels data prep
ci_topic_lesson_rdf <- event_data_2 %>%
  distinct(product, title) %>%
  mutate(title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = '') %>%
           str_replace(pattern = 'test', replacement = ' Test') %>%
           factor(levels = gtools::mixedsort(unique(.)))) %>%
  arrange(product, title) %>%
  mutate(title = as.character(title),
         topic = str_extract(string = title ,pattern = 'Topic[[:digit:]]+')) %>%
  group_by(product, topic) %>%
  mutate(title_labels = if_else(row_number() == 1, topic, '')) %>%
  ungroup() %>%
  dplyr::select(-topic)

ci_topic_content_rdf <- event_data_2 %>%
  distinct(product, title, content) %>%
  mutate(title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = '') %>%
           str_replace(pattern = 'test', replacement = ' Test') %>%
           factor(levels = gtools::mixedsort(unique(.))),
         content = str_to_title(content) %>%
           str_replace(pattern = '_', replacement = ' ') %>%
           factor(levels = gtools::mixedsort(unique(.)))) %>%
  arrange(product, title, content) %>%
  mutate(title = as.character(title),
         content = as.character(content),
         topic = str_extract(string = title, pattern = 'Topic[[:digit:]]+')) %>%
  group_by(product, topic) %>%
  mutate(content_labels = if_else(row_number() == 1, topic, '')) %>%
  ungroup() %>%
  dplyr::select(-topic)

#usage heatmap data
ci_usage_heatmap_rdf <- event_data_2 %>%
  group_by(districtname, product, userroletype, title, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid),
            n_events = n(),
            n_events_per_user = round(n_events/n_users,2)) %>%
  ungroup() %>%
  mutate(title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = '') %>%
           str_replace(pattern = 'test', replacement = ' Test') %>%
           factor(levels = gtools::mixedsort(unique(.)))) %>%
  arrange(districtname, product, title) %>%
  mutate(title = as.character(title),
         districtname = str_to_title(districtname))

#assessment score data prep
ci_assess_score_rdf <- item_response_data %>%
  mutate(date_binned_by_week = ceiling_date(date(timestamp),unit = 'week')) %>%
  group_by(product, test_id, date_binned_by_week, userid) %>%
  summarise(user_score = mean(score*100)) %>%
  summarise(avg_score = round(mean(user_score),2)) %>%
  ungroup()

#performance heatmap data prep
ci_perf_heatmap_rdf <- event_data_2 %>%
  filter(contenttype %in% c('Quiz','Test'), userroletype %in% c('Student')) %>%
  group_by(districtname, product, product_id, title, content, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  mutate(test_id = paste0(product_id,'_',content)) %>%
  dplyr::select(-product_id, -content) %>%
  inner_join(y = ci_assess_score_rdf, by = c('product','test_id','date_binned_by_week')) %>%
  mutate(title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = '') %>%
           str_replace(pattern = 'test', replacement = ' Test') %>%
           factor(levels = gtools::mixedsort(unique(.)))) %>%
  arrange(districtname, product, title) %>%
  mutate(title = as.character(title),
         districtname = str_to_title(districtname)) %>%
  dplyr::select(-test_id)

#performance dotchart data prep
ci_perf_dotchart_rdf <- event_data_2 %>%
  group_by(districtname, product, product_id, title, content, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  mutate(test_id = paste0(product_id,'_',content)) %>%
  dplyr::select(-product_id) %>%
  left_join(y = ci_assess_score_rdf, by = c('product','test_id','date_binned_by_week')) %>%
  mutate(title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = '') %>%
           str_replace(pattern = 'test', replacement = ' Test') %>%
           factor(levels = gtools::mixedsort(unique(.))),
         content = str_to_title(content) %>%
           str_replace(pattern = '_', replacement = ' ') %>%
           factor(levels = gtools::mixedsort(unique(.)))) %>%
  arrange(districtname, product, title, content) %>%
  mutate(title = as.character(title),
         content = as.character(content),
         districtname = str_to_title(districtname)) %>%
  dplyr::select(-test_id)

save(ci_topic_lesson_rdf,
     ci_topic_content_rdf,
     ci_nvr_rdf,
     ci_asd_rdf,
     ci_aeu_rdf,
     ci_usage_heatmap_rdf,
     ci_perf_heatmap_rdf,
     ci_perf_dotchart_rdf,
     file = file.path(folder_path, 'curriculum_insights_tab_data.RData'))




#------LEADERBOARD TAB DATA PREP-----

#----school leaderboard data prep----

#usage type: overall

lb_schoolwise_overall_rdf <- event_data_2 %>%
  group_by(districtname, schoolname, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(districtname, schoolname) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents,na.rm = TRUE),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student'],na.rm = TRUE),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher'], na.rm = TRUE),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0)) %>%
  mutate(districtname = str_to_title(districtname),
         schoolname = str_to_title(schoolname))

#usage type: by product

lb_schoolwise_by_product_rdf <- event_data_2 %>%
  group_by(districtname, schoolname, product, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(districtname, schoolname, product) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents,na.rm = TRUE),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student'],na.rm = TRUE),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher'], na.rm = TRUE),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0)) %>%
  mutate(districtname = str_to_title(districtname),
         schoolname = str_to_title(schoolname))


#----class leaderboard data prep----

#usage type: overall

lb_classwise_overall_rdf <- event_data_2 %>%
  group_by(districtname, classname, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(districtname, classname) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents,na.rm = TRUE),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student'],na.rm = TRUE),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher'], na.rm = TRUE),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0)) %>%
  mutate(districtname = str_to_title(districtname),
         classname = str_to_title(classname))

#usage type: by product

lb_classwise_by_product_rdf <- event_data_2 %>%
  group_by(districtname, classname, product, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(districtname, classname, product) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents,na.rm = TRUE),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student'],na.rm = TRUE),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher'], na.rm = TRUE),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0)) %>%
  mutate(districtname = str_to_title(districtname),
         classname = str_to_title(classname))


#----school plots----

#plot type: nvr users

#usage type: overall

lb_schoolwise_overall_nvr_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(schoolname, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]),
            returning = n_distinct(userid[!first])) %>%
  ungroup() %>%
  mutate(schoolname = str_to_title(schoolname))

#usage type: by product

lb_schoolwise_by_product_nvr_rdf <- event_data_2 %>%
  group_by(product, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(schoolname, product, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]),
            returning = n_distinct(userid[!first])) %>%
  ungroup() %>%
  mutate(schoolname = str_to_title(schoolname))

#plot type: eot

#usage type: overall

lb_schoolwise_overall_eot_rdf <- event_data_2 %>%
  group_by(schoolname, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid),
            n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(schoolname = str_to_title(schoolname))

#usage type: by product

lb_schoolwise_by_product_eot_rdf <- event_data_2 %>%
  group_by(schoolname, product, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid),
            n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(schoolname = str_to_title(schoolname))

#----class plots----

#plot type: nvr users

#usage type: overall

lb_classwise_overall_nvr_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(classname, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]),
            returning = n_distinct(userid[!first])) %>%
  ungroup() %>%
  mutate(classname = str_to_title(classname))

#usage type: by product

lb_classwise_by_product_nvr_rdf <- event_data_2 %>%
  group_by(product, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(classname, product, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]),
            returning = n_distinct(userid[!first])) %>%
  ungroup() %>%
  mutate(classname = str_to_title(classname))

#plot type: eot

#usage type: overall

lb_classwise_overall_eot_rdf <- event_data_2 %>%
  group_by(classname, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid),
            n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(classname = str_to_title(classname))

#usage type: by product

lb_classwise_by_product_eot_rdf <- event_data_2 %>%
  group_by(classname, product, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid),
            n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(classname = str_to_title(classname))


save(lb_classwise_overall_rdf,
     lb_classwise_by_product_rdf,
     lb_classwise_overall_nvr_rdf,
     lb_classwise_overall_eot_rdf,
     lb_classwise_by_product_nvr_rdf,
     lb_classwise_by_product_eot_rdf,
     lb_schoolwise_overall_rdf,
     lb_schoolwise_by_product_rdf,
     lb_schoolwise_overall_nvr_rdf,
     lb_schoolwise_overall_eot_rdf,
     lb_schoolwise_by_product_nvr_rdf,
     lb_schoolwise_by_product_eot_rdf,
     file = file.path(folder_path, 'leaderboard_tab_data.RData'))



