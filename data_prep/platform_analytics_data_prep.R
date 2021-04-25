library(tidyverse)
library(lubridate)
library(magrittr)
library(rstudioapi)

load('raw_event_data.RData')
folder_path <- file.path(rstudioapi::getActiveProject(), 'data_prep', 'platform_analytics_data_files')

#----------GENERAL DATA FOR ALL TABS--------

product_groups <- event_data_2 %$%
  unique(product_group)

grade_bands <- event_data_2 %$%
  unique(grade_band)

subjects <- event_data_2 %$%
  unique(subject)

n_students <- n_distinct(event_data_2$userid[event_data_2$userroletype == 'Student'])
n_teachers <- n_distinct(event_data_2$userid[event_data_2$userroletype == 'Teacher'])
n_student_sessions <- n_distinct(event_data_2$sessionid[event_data_2$userroletype == 'Student'])
n_teacher_sessions <- n_distinct(event_data_2$sessionid[event_data_2$userroletype == 'Teacher'])

top_4_product_groups <- event_data_2 %>%
  group_by(product_group) %>%
  summarise(n_sessions = n_distinct(sessionid)) %>%
  arrange(desc(n_sessions)) %>%
  .$product_group %>%
  .[1:4]

product_group_products_rdf <- event_data_2 %>%
  distinct(product_group, product)

save(product_groups,
     grade_bands,
     subjects,
     top_4_product_groups,
     n_students,
     n_teachers,
     product_group_products_rdf,
     file = file.path(folder_path,'general_data.RData'))



#------OVERALL USAGE DATA PREP--------

# info box data
ou_info_box_rdf <- event_data_2 %>%
  summarise(n_products = n_distinct(product),
            n_product_groups = n_distinct(product_group),
            n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname),
            n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher'])
  ) %>%
  pivot_longer(cols = 1:(length(names(.))), names_to = 'variable', values_to = 'count')





#plot and table data

ou_all_users_data_rdf <- event_data_2 %>%
  group_by(userroletype) %>%
  summarise(tot_users = n_distinct(userid),
            tot_sessions = n_distinct(sessionid)) %>%
  ungroup()



ou_subject_wise_rdf <- event_data_2 %>%
  group_by(subject,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  left_join(ou_all_users_data_rdf, by = 'userroletype') %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>%
  ungroup()


ou_grade_band_wise_filter_none_rdf <- event_data_2 %>%
  group_by(grade_band,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  left_join(ou_all_users_data_rdf, by = 'userroletype') %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>%
  ungroup()

ou_grade_band_wise_filter_subject_rdf <- event_data_2 %>%
  group_by(subject,grade_band,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  left_join(ou_subject_wise_rdf %>% select(subject, userroletype, tot_users = n_users, tot_sessions = n_sessions),
            by = c('subject','userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>%
  ungroup()


ou_product_group_wise_filter_none_rdf <- event_data_2 %>%
  group_by(product_group,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  left_join(ou_all_users_data_rdf, by = 'userroletype') %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>% 
  ungroup()


ou_product_group_wise_filter_subject_rdf <- event_data_2 %>%
  group_by(subject,product_group,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  left_join(ou_subject_wise_rdf %>% select(subject, userroletype, tot_users = n_users, tot_sessions = n_sessions),
            by = c('subject','userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>%
  ungroup()


ou_product_group_wise_filter_grade_band_rdf <- event_data_2 %>%
  group_by(grade_band,product_group,userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  left_join(ou_grade_band_wise_filter_none_rdf %>% select(grade_band, userroletype, tot_users = n_users, tot_sessions = n_sessions),
            by = c('grade_band','userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         perc_sessions = round((100*(n_sessions/tot_sessions)),2)) %>%
  ungroup()

#active weeks data
ou_info_box_rdf$count[ou_info_box_rdf$variable == 'n_students'] -> n_students
ou_info_box_rdf$count[ou_info_box_rdf$variable == 'n_teachers'] -> n_teachers

ou_active_weeks_abs_rdf <- event_data_2 %>%
  group_by(userroletype, userid) %>%
  summarise(active_weeks = n_distinct(date_binned_by_week)) %>%
  ungroup() %>%
  mutate(summariser = case_when(active_weeks<=8 ~ '2 month or less',
                                between(active_weeks,9,12) ~ '2 to 3 months',
                                between(active_weeks,13,17) ~ '3 to 4 months',
                                between(active_weeks,18,25) ~ '4 to 6 months',
                                active_weeks>=26 ~ 'More than 6 months')) %>%
  group_by(userroletype,summariser) %>%
  summarise(filler = n()) %>%
  ungroup() %>%
  mutate(filler = if_else(userroletype == 'Student',
                          round(100*(filler/n_students),2),
                          round(100*(filler/n_teachers),2)),
         color = case_when(summariser == '2 month or less' ~ '#ff2e2e',
                           summariser == '2 to 3 months' ~ '#ff8d30',
                           summariser == '3 to 4 months' ~ '#fff240',
                           summariser == '4 to 6 months' ~ '#ccff40',
                           summariser == 'More than 6 months' ~ '#4bff2b'),
         summariser = factor(summariser,
                             levels = c('2 month or less','2 to 3 months','3 to 4 months',
                                        '4 to 6 months','More than 6 months'))) %>%
  arrange(summariser)


ou_active_weeks_perc_rdf <- event_data_2 %>%
  group_by(userroletype, userid) %>%
  summarise(active_weeks = n_distinct(date_binned_by_week)) %>%
  ungroup() %>%
  mutate(perc_active_weeks = round(100*(active_weeks/53),2),
         summariser = case_when(perc_active_weeks<=20 ~ '20 percent or less',
                                between(perc_active_weeks,20.01,30) ~ '20 to 30 percent',
                                between(perc_active_weeks,30.01,40) ~ '30 to 40 percent',
                                between(perc_active_weeks,40.01,50) ~ '40 to 50 percent',
                                perc_active_weeks>=50.01 ~ 'More than 50 percent')) %>%
  group_by(userroletype,summariser) %>%
  summarise(filler = n()) %>%
  ungroup() %>%
  mutate(filler = if_else(userroletype == 'Student',
                          round(100*(filler/n_students),2),
                          round(100*(filler/n_teachers),2)),
         color = case_when(summariser == '20 percent or less' ~ '#ff2e2e',
                           summariser == '20 to 30 percent' ~ '#ff8d30',
                           summariser == '30 to 40 percent' ~ '#fff240',
                           summariser == '40 to 50 percent' ~ '#ccff40',
                           summariser == 'More than 50 percent' ~ '#4bff2b'),
         summariser = factor(summariser,
                             levels = c('20 percent or less','20 to 30 percent','30 to 40 percent',
                                        '40 to 50 percent','More than 50 percent'))) %>%
  arrange(summariser)


save(ou_info_box_rdf,
     ou_subject_wise_rdf,
     ou_grade_band_wise_filter_none_rdf,
     ou_grade_band_wise_filter_subject_rdf,
     ou_product_group_wise_filter_none_rdf,
     ou_product_group_wise_filter_subject_rdf,
     ou_product_group_wise_filter_grade_band_rdf,
     ou_active_weeks_abs_rdf,
     ou_active_weeks_perc_rdf,
     file = file.path(folder_path, 'overall_usage_tab_data.RData'))

#----------USAGE OVER TIME TAB DATA PREP--------
#usage over time plot
#usage over month

uot_overall_by_month_rdf <- event_data_2 %>%
  mutate(month_year = paste0(month.abb[date_month],'-',year(date))) %>%
  group_by(userid) %>%
  mutate(first = (date_month == month(min(date))),
         last = (date_month == month(max(date)))) %>%
  ungroup() %>%
  group_by(userroletype, month_year) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_subject_by_month_rdf <- event_data_2 %>%
  mutate(month_year = paste0(month.abb[date_month],'-',year(date))) %>%
  group_by(subject, userid) %>%
  mutate(first = (date_month == month(min(date))),
         last = (date_month == month(max(date)))) %>%
  ungroup() %>%
  group_by(subject, userroletype, month_year) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_grade_band_by_month_rdf <- event_data_2 %>%
  mutate(month_year = paste0(month.abb[date_month],'-',year(date))) %>%
  group_by(grade_band, userid) %>%
  mutate(first = (date_month == month(min(date))),
         last = (date_month == month(max(date)))) %>%
  ungroup() %>%
  group_by(grade_band, userroletype, month_year) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_top_4_product_group_by_month_rdf <- event_data_2 %>%
  filter(product_group %in% top_4_product_groups) %>%
  mutate(month_year = paste0(month.abb[date_month],'-',year(date))) %>%
  group_by(product_group, userid) %>%
  mutate(first = (date_month == month(min(date))),
         last = (date_month == month(max(date)))) %>%
  ungroup() %>%
  group_by(product_group, userroletype, month_year) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

#usage over week

uot_overall_by_week_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_subject_by_week_rdf <- event_data_2 %>%
  group_by(subject, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(subject, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_grade_band_by_week_rdf <- event_data_2 %>%
  group_by(grade_band, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(grade_band, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_top_4_product_group_by_week_rdf <- event_data_2 %>%
  filter(product_group %in% top_4_product_groups) %>%
  group_by(product_group, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(product_group, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

#usage over day

uot_overall_by_day_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>%
  ungroup() %>%
  group_by(userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_subject_by_day_rdf <- event_data_2 %>%
  group_by(subject, userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>%
  ungroup() %>%
  group_by(subject, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_grade_band_by_day_rdf <- event_data_2 %>%
  group_by(grade_band, userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>% 
  ungroup() %>%
  group_by(grade_band, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_top_4_product_group_by_day_rdf <- event_data_2 %>%
  filter(product_group %in% top_4_product_groups) %>%
  group_by(product_group, userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>%
  ungroup() %>%
  group_by(product_group, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup()

uot_overall_rdf <- bind_rows(uot_overall_by_day_rdf %>% mutate(res = 'Day') %>% rename(dateid = date),
                             uot_overall_by_month_rdf %>%
                               mutate(res = 'Month', month_year = as.Date(paste0('01-',month_year), format = '%d-%b-%Y')) %>% rename(dateid = month_year),
                             uot_overall_by_week_rdf %>%
                               mutate(res = 'Week', date_binned_by_week = date(date_binned_by_week)) %>% rename(dateid = date_binned_by_week))


uot_subject_rdf <- bind_rows(uot_subject_by_day_rdf %>% mutate(res = 'Day') %>% rename(dateid = date),
                             uot_subject_by_month_rdf %>%
                               mutate(res = 'Month', month_year = as.Date(paste0('01-',month_year), format = '%d-%b-%Y')) %>% rename(dateid = month_year),
                             uot_subject_by_week_rdf %>%
                               mutate(res = 'Week', date_binned_by_week = date(date_binned_by_week)) %>% rename(dateid = date_binned_by_week))

uot_grade_band_rdf <- bind_rows(uot_grade_band_by_day_rdf %>% mutate(res = 'Day') %>% rename(dateid = date),
                                uot_grade_band_by_month_rdf %>%
                                  mutate(res = 'Month', month_year = as.Date(paste0('01-',month_year), format = '%d-%b-%Y')) %>% rename(dateid = month_year),
                                uot_grade_band_by_week_rdf %>%
                                  mutate(res = 'Week', date_binned_by_week = date(date_binned_by_week)) %>% rename(dateid = date_binned_by_week))


uot_top_4_product_group_rdf <- bind_rows(uot_top_4_product_group_by_day_rdf %>% mutate(res = 'Day') %>% rename(dateid = date),
                                         uot_top_4_product_group_by_month_rdf %>%
                                           mutate(res = 'Month', month_year = as.Date(paste0('01-',month_year), format = '%d-%b-%Y')) %>% rename(dateid = month_year),
                                         uot_top_4_product_group_by_week_rdf %>%
                                           mutate(res = 'Week', date_binned_by_week = date(date_binned_by_week)) %>% rename(dateid = date_binned_by_week))


#avg session duration over time plot
#overall data prep

load('sim_event_data.RData')

session_duration_rdf <- event_data_2 %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(sessionid) %>%
  mutate(session_duration = as.numeric(difftime(max(timestamp), min(timestamp), unit = 'mins'))) %>%
  ungroup() %>%
  distinct(sessionid, session_duration, subject, grade_band, product_group, userroletype, date, date_binned_by_week, date_month)

uot_asd_overall_by_month_rdf <- session_duration_rdf %>%
  group_by(userroletype, date_month) %>%
  summarise(avg_duration = round(mean(session_duration),2), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  rename(dateid = date_month)

uot_asd_overall_by_week_rdf <- session_duration_rdf %>%
  group_by(userroletype, date_binned_by_week) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  mutate(date_binned_by_week = date(date_binned_by_week)) %>%
  rename(dateid = date_binned_by_week)

uot_asd_overall_by_day_rdf <- session_duration_rdf %>%
  group_by(userroletype, date) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  rename(dateid = date)

#subject wise data prep
uot_asd_subject_by_month_rdf <- session_duration_rdf %>%
  group_by(subject, userroletype, date_month) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  rename(dateid = date_month)

uot_asd_subject_by_week_rdf <- session_duration_rdf %>%
  group_by(subject, userroletype, date_binned_by_week) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  mutate(date_binned_by_week = date(date_binned_by_week)) %>%
  rename(dateid = date_binned_by_week)

uot_asd_subject_by_day_rdf <- session_duration_rdf %>%
  group_by(subject, userroletype, date) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  rename(dateid = date)

#grade band wise data prep

uot_asd_grade_band_by_month_rdf <- session_duration_rdf %>%
  group_by(grade_band, userroletype, date_month) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  rename(dateid = date_month)

uot_asd_grade_band_by_week_rdf <- session_duration_rdf %>%
  group_by(grade_band, userroletype, date_binned_by_week) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  mutate(date_binned_by_week = date(date_binned_by_week)) %>%
  rename(dateid = date_binned_by_week)

uot_asd_grade_band_by_day_rdf <- session_duration_rdf %>%
  group_by(grade_band, userroletype, date) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  rename(dateid = date)

#top 4 product group wise data prep

uot_asd_top_4_product_group_by_month_rdf <- session_duration_rdf %>%
  filter(product_group %in% top_4_product_groups) %>%
  group_by(product_group, userroletype, date_month) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  rename(dateid = date_month)

uot_asd_top_4_product_group_by_week_rdf <- session_duration_rdf %>%
  filter(product_group %in% top_4_product_groups) %>%
  group_by(product_group, userroletype, date_binned_by_week) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  mutate(date_binned_by_week = date(date_binned_by_week)) %>%
  rename(dateid = date_binned_by_week)

uot_asd_top_4_product_group_by_day_rdf <- session_duration_rdf %>%
  filter(product_group %in% top_4_product_groups) %>%
  group_by(product_group, userroletype, date) %>%
  summarise(avg_duration = mean(session_duration), n_sessions = n()) %>%
  ungroup() %>%
  mutate(perc_sessions = if_else(userroletype == 'Student',
                                 round(100*n_sessions/n_student_sessions,2),
                                 round(100*n_sessions/n_teacher_sessions,2))) %>%
  rename(dateid = date)

uot_asd_overall_rdf <- bind_rows(uot_asd_overall_by_day_rdf %>% mutate(res = 'Day'),
                                 uot_asd_overall_by_month_rdf %>% mutate(res = 'Month'),
                                 uot_asd_overall_by_week_rdf %>% mutate(res = 'Week'))


uot_asd_subject_rdf <- bind_rows(uot_asd_subject_by_day_rdf %>% mutate(res = 'Day'),
                                 uot_asd_subject_by_month_rdf %>% mutate(res = 'Month'),
                                 uot_asd_subject_by_week_rdf %>% mutate(res = 'Week'))

uot_asd_grade_band_rdf <- bind_rows(uot_asd_grade_band_by_day_rdf %>% mutate(res = 'Day'),
                                    uot_asd_grade_band_by_month_rdf %>% mutate(res = 'Month'),
                                    uot_asd_grade_band_by_week_rdf %>% mutate(res = 'Week'))


uot_asd_top_4_product_group_rdf <- bind_rows(uot_asd_top_4_product_group_by_day_rdf %>% mutate(res = 'Day'),
                                             uot_asd_top_4_product_group_by_month_rdf %>% mutate(res = 'Month'),
                                             uot_asd_top_4_product_group_by_week_rdf %>% mutate(res = 'Week'))

uot_asd_overall_rdf$avg_duration <- round(uot_asd_overall_rdf$avg_duration,2)
uot_asd_subject_rdf$avg_duration <- round(uot_asd_subject_rdf$avg_duration,2)
uot_asd_grade_band_rdf$avg_duration <- round(uot_asd_grade_band_rdf$avg_duration,2)
uot_asd_top_4_product_group_rdf$avg_duration <- round(uot_asd_top_4_product_group_rdf$avg_duration,2)



#avg events per user plot
#overall data prep

uot_aeu_overall_by_month_rdf <- event_data_2 %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2))) %>%
  rename(dateid = date_month)

uot_aeu_overall_by_week_rdf <- event_data_2 %>%
  group_by(userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2)),
         date_binned_by_week = date(date_binned_by_week)) %>%
  rename(dateid = date_binned_by_week)

uot_aeu_overall_by_day_rdf <- event_data_2 %>%
  group_by(userroletype, date) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2))) %>%
  rename(dateid = date)

#subject wise data prep

uot_aeu_subject_by_month_rdf <- event_data_2 %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(subject, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2))) %>%
  rename(dateid = date_month)

uot_aeu_subject_by_week_rdf <- event_data_2 %>%
  group_by(subject, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2)),
         date_binned_by_week = date(date_binned_by_week)) %>%
  rename(dateid = date_binned_by_week)

uot_aeu_subject_by_day_rdf <- event_data_2 %>%
  group_by(subject, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2))) %>%
  rename(dateid = date)

#grade_band wise data prep

uot_aeu_grade_band_by_month_rdf <- event_data_2 %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(grade_band, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2))) %>%
  rename(dateid = date_month)

uot_aeu_grade_band_by_week_rdf <- event_data_2 %>%
  group_by(grade_band, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2)),
         date_binned_by_week = date(date_binned_by_week)) %>%
  rename(dateid = date_binned_by_week)

uot_aeu_grade_band_by_day_rdf <- event_data_2 %>%
  group_by(grade_band, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2))) %>%
  rename(dateid = date)

#top 4 product group wise data prep

uot_aeu_top_4_product_group_by_month_rdf <- event_data_2 %>%
  filter(product_group %in% top_4_product_groups) %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(product_group, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2))) %>%
  rename(dateid = date_month)

uot_aeu_top_4_product_group_by_week_rdf <- event_data_2 %>%
  filter(product_group %in% top_4_product_groups) %>%
  group_by(product_group, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2)),
         date_binned_by_week = date(date_binned_by_week)) %>%
  rename(dateid = date_binned_by_week)

uot_aeu_top_4_product_group_by_day_rdf <- event_data_2 %>%
  filter(product_group %in% top_4_product_groups) %>%
  group_by(product_group, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(perc_users = if_else(userroletype == 'Student',
                              round(100*n_users/n_students,2),
                              round(100*n_users/n_teachers,2))) %>%
  rename(dateid = date)


uot_aeu_overall_rdf <- bind_rows(uot_aeu_overall_by_day_rdf %>% mutate(res = 'Day'),
                                 uot_aeu_overall_by_month_rdf %>% mutate(res = 'Month'),
                                 uot_aeu_overall_by_week_rdf %>% mutate(res = 'Week'))


uot_aeu_subject_rdf <- bind_rows(uot_aeu_subject_by_day_rdf %>% mutate(res = 'Day'),
                                 uot_aeu_subject_by_month_rdf %>% mutate(res = 'Month'),
                                 uot_aeu_subject_by_week_rdf %>% mutate(res = 'Week'))

uot_aeu_grade_band_rdf <- bind_rows(uot_aeu_grade_band_by_day_rdf %>% mutate(res = 'Day'),
                                    uot_aeu_grade_band_by_month_rdf %>% mutate(res = 'Month'),
                                    uot_aeu_grade_band_by_week_rdf %>% mutate(res = 'Week'))


uot_aeu_top_4_product_group_rdf <- bind_rows(uot_aeu_top_4_product_group_by_day_rdf %>% mutate(res = 'Day'),
                                             uot_aeu_top_4_product_group_by_month_rdf %>% mutate(res = 'Month'),
                                             uot_aeu_top_4_product_group_by_week_rdf %>% mutate(res = 'Week'))


save(
  uot_overall_rdf,
  uot_grade_band_rdf,
  uot_subject_rdf,
  uot_top_4_product_group_rdf,
  uot_asd_overall_rdf,
  uot_asd_grade_band_rdf,
  uot_asd_subject_rdf,
  uot_asd_top_4_product_group_rdf,
  uot_aeu_overall_rdf,
  uot_aeu_grade_band_rdf,
  uot_aeu_subject_rdf,
  uot_aeu_top_4_product_group_rdf,
  file = file.path(folder_path,'usage_over_time_tab_data.RData')
)

#--------USAGE BY GEOGRAPHY-----------
#heat map data prep
ubg_overall_rdf <- event_data_2 %>%
  group_by(state) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_sessions = n_distinct(sessionid),
            n_events_per_student = round(n()/n_students,0),
            n_events_per_teacher = round(n()/n_teachers,0),
            n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname)) %>%
  ungroup()

ubg_subject_rdf <- event_data_2 %>%
  group_by(subject, state) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_sessions = n_distinct(sessionid),
            n_events_per_student = round(n()/n_students,0),
            n_events_per_teacher = round(n()/n_teachers,0),
            n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname)) %>%
  ungroup()

ubg_grade_band_rdf <- event_data_2 %>%
  group_by(grade_band, state) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_sessions = n_distinct(sessionid),
            n_events_per_student = round(n()/n_students,0),
            n_events_per_teacher = round(n()/n_teachers,0),
            n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname)) %>%
  
  ungroup()

ubg_product_group_rdf <- event_data_2 %>%
  group_by(product_group, state) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_sessions = n_distinct(sessionid),
            n_events_per_student = round(n()/n_students,0),
            n_events_per_teacher = round(n()/n_teachers,0),
            n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname)) %>%
  ungroup()

#dot chart

district_lat_long_rdf <- tibble(
  districtname = paste0('districtname_',1:458),
  lat = maps::us.cities$lat[1:458],
  long = maps::us.cities$long[1:458]
)

ubg_dc_overall_rdf <- event_data_2 %>%
  group_by(districtname) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_sessions = n_distinct(sessionid),
            n_events_per_student = round(n()/n_students,0),
            n_events_per_teacher = round(n()/n_teachers,0),
            n_schools = n_distinct(schoolname)) %>%
  ungroup() %>%
  mutate(n_events_per_student = if_else(is.infinite(n_events_per_student),0,n_events_per_student),
         n_events_per_teacher = if_else(is.infinite(n_events_per_teacher),0,n_events_per_teacher)) %>%
  left_join(y = district_lat_long_rdf, by = 'districtname')

ubg_dc_subject_rdf <- event_data_2 %>%
  group_by(subject, districtname) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_sessions = n_distinct(sessionid),
            n_events_per_student = round(n()/n_students,0),
            n_events_per_teacher = round(n()/n_teachers,0),
            n_schools = n_distinct(schoolname)) %>%
  ungroup() %>%
  mutate(n_events_per_student = if_else(is.infinite(n_events_per_student),0,n_events_per_student),
         n_events_per_teacher = if_else(is.infinite(n_events_per_teacher),0,n_events_per_teacher)) %>%
  left_join(y = district_lat_long_rdf, by = 'districtname')

ubg_dc_grade_band_rdf <- event_data_2 %>%
  group_by(grade_band, districtname) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_sessions = n_distinct(sessionid),
            n_events_per_student = round(n()/n_students,0),
            n_events_per_teacher = round(n()/n_teachers,0),
            n_schools = n_distinct(schoolname)) %>%
  ungroup() %>%
  mutate(n_events_per_student = if_else(is.infinite(n_events_per_student),0,n_events_per_student),
         n_events_per_teacher = if_else(is.infinite(n_events_per_teacher),0,n_events_per_teacher)) %>%
  left_join(y = district_lat_long_rdf, by = 'districtname')

ubg_dc_product_group_rdf <- event_data_2 %>%
  group_by(product_group, districtname) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_sessions = n_distinct(sessionid),
            n_events_per_student = round(n()/n_students,0),
            n_events_per_teacher = round(n()/n_teachers,0),
            n_schools = n_distinct(schoolname)) %>%
  ungroup() %>%
  mutate(n_events_per_student = if_else(is.infinite(n_events_per_student),0,n_events_per_student),
         n_events_per_teacher = if_else(is.infinite(n_events_per_teacher),0,n_events_per_teacher)) %>%
  left_join(y = district_lat_long_rdf, by = 'districtname')





save(ubg_grade_band_rdf,
     ubg_overall_rdf,
     ubg_product_group_rdf,
     ubg_subject_rdf,
     ubg_dc_grade_band_rdf,
     ubg_dc_overall_rdf,
     ubg_dc_product_group_rdf,
     ubg_dc_subject_rdf,
     file = file.path(folder_path,'usage_by_geography_tab_data.RData'))

#---------USAGE BY PRODUCTS-----------

#infobox data
ubp_info_box_data_rdf <- event_data_2 %>%
  group_by(product_group) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_products = n_distinct(product)) %>%
  ungroup()

#horizontal bar data
ubp_product_data_rdf <- event_data_2 %>%
  group_by(product_group, product, userroletype) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup()

#product group usage over time data
ubp_pg_usage_over_time_rdf <- event_data_2 %>%
  group_by(product_group, userid) %>%
  mutate(first_week = (date_binned_by_week == min(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(product_group, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first_week]),
            returning = n_distinct(userid[!first_week])) %>%
  ungroup() %>%
  mutate(date_binned_by_week = date(date_binned_by_week))

ubp_products_usage_over_time_rdf <- event_data_2 %>%
  group_by(product, userid) %>%
  mutate(first_week = (date_binned_by_week == min(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(product, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first_week]),
            returning = n_distinct(userid[!first_week])) %>%
  ungroup() %>%
  mutate(date_binned_by_week = date(date_binned_by_week))

#user behaviour data
ubp_user_act_days_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, userid) %>%
  summarise(act_days = n_distinct(date)) %>%
  ungroup() %>%
  group_by(product_group, userroletype, act_days) %>%
  summarise(n_users = n()) %>%
  ungroup()

ubp_user_act_weeks_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, userid) %>%
  summarise(act_weeks = n_distinct(date_binned_by_week)) %>%
  ungroup() %>%
  group_by(product_group, userroletype, act_weeks) %>%
  summarise(n_users = n()) %>%
  ungroup()

ubp_user_session_time_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, sessionid) %>%
  summarise(session_time = round(as.numeric(difftime(max(timestamp), min(timestamp), units = 'mins')),0)) %>%
  ungroup() %>%
  group_by(product_group,userroletype,session_time) %>%
  summarise(n_users = n()) %>%
  ungroup()


#compare product data
ubp_compare_product_data_rdf <- event_data_2 %>%
  group_by(product_group, product) %>%
  summarise(n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname),
            n_sessions = n_distinct(sessionid),
            n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_stud_events = sum(userroletype=='Student'),
            n_teacher_events = sum(userroletype=='Teacher'),
            total_events = n()) %>%
  ungroup()

#usage by content area
ubp_content_area_usage_rdf <- event_data_2 %>%
  group_by(product_group, product, content_area, userroletype) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup()



save(ubp_info_box_data_rdf,
     ubp_product_data_rdf,
     ubp_pg_usage_over_time_rdf,
     ubp_products_usage_over_time_rdf,
     ubp_user_act_days_rdf,
     ubp_user_act_weeks_rdf,
     ubp_user_session_time_rdf,
     ubp_compare_product_data_rdf,
     ubp_content_area_usage_rdf,
     file = file.path(folder_path,'usage_by_products_tab_data.RData'))

#-------------USAGE BY CONTENT TYPE TAB DATA PREP---------
#plot 1

ubct_sub_data <- event_data_2 %>%
  group_by(subject, userroletype) %>%
  summarise(total_users = n_distinct(userid)) %>%
  ungroup()

ubct_gb_data <- event_data_2 %>%
  group_by(grade_band, userroletype) %>%
  summarise(total_users = n_distinct(userid)) %>%
  ungroup()

ubct_pg_data <- event_data_2 %>%
  group_by(product_group, userroletype) %>%
  summarise(total_users = n_distinct(userid)) %>%
  ungroup()

ubct_by_sub_rdf <- event_data_2 %>%
  group_by(subject, userroletype, contenttype) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  left_join(y = ubct_sub_data, by = c('subject','userroletype')) %>%
  mutate(perc_users = round(100*n_users/total_users,0))

ubct_by_gb_rdf <- event_data_2 %>%
  group_by(grade_band, userroletype, contenttype) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  left_join(y = ubct_gb_data, by = c('grade_band','userroletype')) %>%
  mutate(perc_users = round(100*n_users/total_users,0))

ubct_by_pg_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, contenttype) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  left_join(y = ubct_pg_data, by = c('product_group','userroletype')) %>%
  mutate(perc_users = round(100*n_users/total_users,0))

#plot2

ubct_plot_2_by_sub_rdf <- event_data_2 %>%
  group_by(subject, contenttype) %>%
  summarise(`# Students` = n_distinct(userid[userroletype == 'Student']),
            `# Teachers` = n_distinct(userid[userroletype == 'Teacher']),
            `# Sessions` = n_distinct(sessionid)) %>%
  ungroup()

ubct_plot_2_by_gb_rdf <- event_data_2 %>%
  group_by(grade_band, contenttype) %>%
  summarise(`# Students` = n_distinct(userid[userroletype == 'Student']),
            `# Teachers` = n_distinct(userid[userroletype == 'Teacher']),
            `# Sessions` = n_distinct(sessionid)) %>%
  ungroup()


ubct_plot_2_by_pg_rdf <- event_data_2 %>%
  group_by(product_group, contenttype) %>%
  summarise(`# Students` = n_distinct(userid[userroletype == 'Student']),
            `# Teachers` = n_distinct(userid[userroletype == 'Teacher']),
            `# Sessions` = n_distinct(sessionid)) %>%
  ungroup()

#table data

ubct_table_by_sub_rdf <- event_data_2 %>%
  group_by(subject, contenttype) %>%
  summarise(n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname),
            n_sessions = n_distinct(sessionid),
            n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            student_events = sum(userroletype == 'Student'),
            teacher_events = sum(userroletype == 'Teacher')) %>%
  ungroup()

ubct_table_by_gb_rdf <- event_data_2 %>%
  group_by(grade_band, contenttype) %>%
  summarise(n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname),
            n_sessions = n_distinct(sessionid),
            n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            student_events = sum(userroletype == 'Student'),
            teacher_events = sum(userroletype == 'Teacher')) %>%
  ungroup()

ubct_table_by_pg_rdf <- event_data_2 %>%
  group_by(product_group, contenttype) %>%
  summarise(n_districts = n_distinct(districtname),
            n_schools = n_distinct(schoolname),
            n_sessions = n_distinct(sessionid),
            n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            student_events = sum(userroletype == 'Student'),
            teacher_events = sum(userroletype == 'Teacher')) %>%
  ungroup()



save(ubct_by_gb_rdf,
     ubct_by_pg_rdf,
     ubct_by_sub_rdf,
     ubct_plot_2_by_gb_rdf,
     ubct_plot_2_by_pg_rdf,
     ubct_plot_2_by_sub_rdf,
     ubct_table_by_gb_rdf,
     ubct_table_by_pg_rdf,
     ubct_table_by_sub_rdf,
     file = file.path(folder_path,'usage_by_content_type_tab_data.RData'))


#--------LEADERBOARD TAB DATA PREP---------
#overall

dl_overall_rdf <- event_data_2 %>%
  group_by(districtname, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(product_id, content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(districtname) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student']),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher']),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0))

#subject wise

dl_subject_wise_rdf <- event_data_2 %>%
  group_by(subject, districtname, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(product_id, content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(subject, districtname) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents,na.rm = TRUE),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student'],na.rm = TRUE),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher'], na.rm = TRUE),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0))


#grade band wise

dl_grade_band_wise_rdf <- event_data_2 %>%
  group_by(grade_band, districtname, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(product_id, content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(grade_band, districtname) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents,na.rm = TRUE),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student'],na.rm = TRUE),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher'], na.rm = TRUE),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0))

#product group wise

dl_product_group_wise_rdf <- event_data_2 %>%
  group_by(product_group, districtname, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(product_id, content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(product_group, districtname) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents,na.rm = TRUE),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student'],na.rm = TRUE),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher'], na.rm = TRUE),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0))

#new vs returning plot

dl_nvr_overall_rdf <- event_data_2 %>%
  group_by(userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(districtname, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]),
            returning = n_distinct(userid[!first])) %>%
  ungroup() %>%
  mutate(date_binned_by_week = date(date_binned_by_week))

#events per user over time

dl_eot_overall_rdf <- event_data_2 %>%
  group_by(districtname, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid),
            n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(date_binned_by_week = date(date_binned_by_week))

save(
  dl_overall_rdf,
  dl_nvr_overall_rdf,
  dl_eot_overall_rdf,
  file = file.path(folder_path,'leaderboard_tab_data.RData')
)

file.copy(file.path(folder_path, list.files(folder_path)), file.path(getActiveProject(), 'platform_analytics', 'data'),
          overwrite = TRUE)
file.copy(file.path(getActiveProject(), 'data_prep', 'us_states_metadata.RData'),
          file.path(getActiveProject(), 'platform_analytics', 'data'), overwrite = TRUE)
