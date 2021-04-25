library(tidyverse)
library(lubridate)
library(magrittr)
library(ltm)
library(rstudioapi)

load('raw_event_data.RData')
load('raw_item_response_data.RData')
folder_path <- file.path(getActiveProject(), 'data_prep', 'curriculum_analytics_data_files')

#----GENERAL DATA PREP-------

product_groups <- event_data_2 %$%
  unique(product_group)

product_group_products_rdf <- event_data_2 %>%
  distinct(product_group, product)


save(product_groups,
     product_group_products_rdf,
     file = file.path(folder_path,'general_data.RData'))

#-------OVERALL USAGE TAB DATA PREP------

#infobox data
ou_infobox_rdf <- event_data_2 %>%
  group_by(product_group) %>%
  summarise(n_products = n_distinct(product),
            n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher']),
            n_contents_accessed = n_distinct(product, content),
            n_schools = n_distinct(schoolname),
            n_districts = n_distinct(districtname)) %>%
  ungroup()

#plot1 and table data
ou_productwise_rdf <- event_data_2 %>%
  group_by(product_group, product, userroletype) %>%
  summarise(n_users = n_distinct(userid),
            n_sessions = n_distinct(sessionid)) %>%
  ungroup()

#active weeks plot data
ou_pg_total_users_rdf <- event_data_2 %>%
  group_by(product_group, userroletype) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()


ou_act_weeks_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, userid) %>%
  summarise(act_weeks = n_distinct(date_binned_by_week)) %>%
  ungroup() %>%
  mutate(act_weeks_label = case_when(act_weeks<=4 ~ '1 month or less',
                                     between(act_weeks,5,8) ~ '1 to 2 months',
                                     between(act_weeks,9,12) ~ '2 to 3 months',
                                     between(act_weeks,13,17) ~ '3 to 4 months',
                                     act_weeks>=18 ~ 'More than 4 months')) %>%
  group_by(product_group, userroletype, act_weeks_label) %>%
  summarise(n_users = n()) %>%
  ungroup() %>%
  left_join(y = ou_pg_total_users_rdf, by = c('product_group', 'userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users), 2),
         color = case_when(act_weeks_label == '1 month or less' ~ '#ff2e2e',
                           act_weeks_label == '1 to 2 months' ~ '#ff8d30',
                           act_weeks_label == '2 to 3 months' ~ '#fff240',
                           act_weeks_label == '3 to 4 months' ~ '#ccff40',
                           act_weeks_label == 'More than 4 months' ~ '#4bff2b'),
         act_weeks_label = factor(act_weeks_label,
                                  levels = c('1 month or less','1 to 2 months','2 to 3 months',
                                             '3 to 4 months','More than 4 months')))


ou_perc_act_weeks_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, userid) %>%
  summarise(act_weeks = n_distinct(date_binned_by_week)) %>%
  ungroup() %>%
  mutate(perc_act_weeks = round(100*(act_weeks/52),1),
         act_weeks_label = case_when(perc_act_weeks<=10 ~ '10 percent or less',
                                     between(perc_act_weeks,10.1,20) ~ '10 to 20 percent',
                                     between(perc_act_weeks,20.1,30) ~ '20 to 30 percent',
                                     between(perc_act_weeks,30.1,40) ~ '30 to 40 percent',
                                     perc_act_weeks>=40.1 ~ 'More than 40 percent')) %>%
  group_by(product_group, userroletype, act_weeks_label) %>%
  summarise(n_users = n()) %>%
  ungroup() %>%
  left_join(y = ou_pg_total_users_rdf, by = c('product_group', 'userroletype')) %>%
  mutate(perc_users = round(100*(n_users/tot_users),2),
         color = case_when(act_weeks_label == '10 percent or less' ~ '#ff2e2e',
                           act_weeks_label == '10 to 20 percent' ~ '#ff8d30',
                           act_weeks_label == '20 to 30 percent' ~ '#fff240',
                           act_weeks_label == '30 to 40 percent' ~ '#ccff40',
                           act_weeks_label == 'More than 40 percent' ~ '#4bff2b'),
         act_weeks_label = factor(act_weeks_label,
                             levels = c('10 percent or less','10 to 20 percent','20 to 30 percent',
                                        '30 to 40 percent','More than 40 percent'))) 





save(ou_infobox_rdf,
     ou_productwise_rdf,
     ou_perc_act_weeks_rdf,
     ou_act_weeks_rdf,
     file = file.path(folder_path,'overall_usage_tab_data.RData'))

#--------USAGE OVER TIME TAB DATA PREP-------
#usage over time plot
#Res : Month

uot_overall_by_month_rdf <- event_data_2 %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(product_group, userid) %>%
  mutate(first = (date_month == floor_date(min(date), unit = 'month')),
         last = (date_month == floor_date(max(date), unit = 'month'))) %>%
  ungroup() %>%
  group_by(product_group, userroletype, date_month) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Month')

uot_by_product_by_month_rdf <- event_data_2 %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(product, userid) %>%
  mutate(first = (date_month == floor_date(min(date), unit = 'month')),
         last = (date_month == floor_date(max(date), unit = 'month'))) %>%
  ungroup() %>%
  group_by(product, userroletype, date_month) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Month')

#Res : Week

# check <- event_data_2 %>%
#   filter(product %in% 'Literacy Grades 3-5 Grade 3', userroletype == 'Student') %>%
#   group_by(userid) %>%
#   summarise(last_week)

uot_overall_by_week_rdf <- event_data_2 %>%
  group_by(product_group, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(product_group, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Week')

uot_by_product_by_week_rdf <- event_data_2 %>%
  group_by(product, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week)),
         last = (date_binned_by_week == max(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(product, userroletype, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Week')

#Res : Day

uot_overall_by_day_rdf <- event_data_2 %>%
  group_by(product_group, userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>%
  ungroup() %>%
  group_by(product_group, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Day')

uot_by_product_by_day_rdf <- event_data_2 %>%
  group_by(product, userid) %>%
  mutate(first = (date == min(date)),
         last = (date == max(date))) %>%
  ungroup() %>%
  group_by(product, userroletype, date) %>%
  summarise(new = n_distinct(userid[first]), return = n_distinct(userid[!first]),
            leaving = n_distinct(userid[!first & last]), bouncing = n_distinct(userid[first & last])) %>%
  ungroup() %>%
  mutate(res = 'Day')

uot_overall_rdf <- bind_rows(uot_overall_by_day_rdf %>% rename(dateid = date),
                             uot_overall_by_month_rdf %>% rename(dateid = date_month),
                             uot_overall_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  mutate(tot_users = new + return)

uot_by_product_rdf <- bind_rows(uot_by_product_by_day_rdf %>% rename(dateid = date),
                             uot_by_product_by_month_rdf %>% rename(dateid = date_month),
                             uot_by_product_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  mutate(tot_users = new + return)

#avg session duration over time plot

session_duration_rdf <- event_data_2 %>%
  mutate(date_month = floor_date(date, unit = 'month')) %>%
  group_by(sessionid) %>%
  mutate(session_duration = as.numeric(difftime(max(timestamp), min(timestamp), unit = 'mins'))) %>%
  ungroup() %>%
  distinct(sessionid, session_duration, product_group, product, userroletype, date, date_binned_by_week, date_month)

uot_product_group_wise_tot_users_rdf <- event_data_2 %>%
  group_by(product_group, userroletype) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()

uot_product_wise_tot_users_rdf <- event_data_2 %>%
  group_by(product, userroletype) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()

# save(session_duration_rdf, file = 'CA_sim_session_duration_data.RData')

#Res : Month
uot_asd_overall_by_month_rdf <- session_duration_rdf %>%
  group_by(product_group, userroletype, date_month) %>%
  summarise(avg_session_duration = mean(session_duration, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(res = 'Month')

uot_asd_by_product_by_month_rdf <- session_duration_rdf %>%
  group_by(product, userroletype, date_month) %>%
  summarise(avg_session_duration = mean(session_duration, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(res = 'Month')

#Res : Week
uot_asd_overall_by_week_rdf <- session_duration_rdf %>%
  group_by(product_group, userroletype, date_binned_by_week) %>%
  summarise(avg_session_duration = mean(session_duration, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(res = 'Week')

uot_asd_by_product_by_week_rdf <- session_duration_rdf %>%
  group_by(product, userroletype, date_binned_by_week) %>%
  summarise(avg_session_duration = mean(session_duration, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(res = 'Week')

#Res : Day
uot_asd_overall_by_day_rdf <- session_duration_rdf %>%
  group_by(product_group, userroletype, date) %>%
  summarise(avg_session_duration = mean(session_duration, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(res = 'Day')

uot_asd_by_product_by_day_rdf <- session_duration_rdf %>%
  group_by(product, userroletype, date) %>%
  summarise(avg_session_duration = mean(session_duration, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(res = 'Day')


uot_asd_overall_rdf <- bind_rows(uot_asd_overall_by_day_rdf %>% rename(dateid = date),
                             uot_asd_overall_by_month_rdf %>% rename(dateid = date_month),
                             uot_asd_overall_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  left_join(y = uot_overall_rdf %>%
              dplyr::select(product_group, userroletype, dateid, res, tot_users),
            by = c('product_group', 'userroletype', 'dateid', 'res')) %>%
  rename(n_users = tot_users) %>%
  left_join(y = uot_product_group_wise_tot_users_rdf, by = c('product_group', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2),
         avg_session_duration = round(avg_session_duration,2))

uot_asd_by_product_rdf <- bind_rows(uot_asd_by_product_by_day_rdf %>% rename(dateid = date),
                                uot_asd_by_product_by_month_rdf %>% rename(dateid = date_month),
                                uot_asd_by_product_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  left_join(y = uot_by_product_rdf %>%
              dplyr::select(product, userroletype, dateid, res, tot_users),
            by = c('product', 'userroletype', 'dateid', 'res')) %>%
  rename(n_users = tot_users) %>%
  left_join(uot_product_wise_tot_users_rdf, by = c('product', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2),
         avg_session_duration = round(avg_session_duration,2))

#avg events per user

#Res : Month
uot_aeu_overall_by_month_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), avg_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(res = 'Month')

uot_aeu_by_product_by_month_rdf <- event_data_2 %>%
  group_by(product, userroletype, date_month) %>%
  summarise(n_users = n_distinct(userid), avg_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(res = 'Month')

#Res : Week
uot_aeu_overall_by_week_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), avg_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(res = 'Week')

uot_aeu_by_product_by_week_rdf <- event_data_2 %>%
  group_by(product, userroletype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid), avg_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(res = 'Week')

#Res : Day
uot_aeu_overall_by_day_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), avg_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(res = 'Day')

uot_aeu_by_product_by_day_rdf <- event_data_2 %>%
  group_by(product, userroletype, date) %>%
  summarise(n_users = n_distinct(userid), avg_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(res = 'Day')


uot_aeu_overall_rdf <- bind_rows(uot_aeu_overall_by_day_rdf %>% rename(dateid = date),
                                 uot_aeu_overall_by_month_rdf %>% rename(dateid = date_month),
                                 uot_aeu_overall_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  left_join(y = uot_product_group_wise_tot_users_rdf, by = c('product_group', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2))

uot_aeu_by_product_rdf <- bind_rows(uot_aeu_by_product_by_day_rdf %>% rename(dateid = date),
                                    uot_aeu_by_product_by_month_rdf %>% rename(dateid = date_month),
                                    uot_aeu_by_product_by_week_rdf %>% rename(dateid = date_binned_by_week)) %>%
  left_join(uot_product_wise_tot_users_rdf, by = c('product', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2))




save(uot_overall_rdf,
     uot_by_product_rdf,
     uot_asd_by_product_rdf,
     uot_asd_overall_rdf,
     uot_aeu_by_product_rdf,
     uot_aeu_overall_rdf,
     file = file.path(folder_path,'usage_over_time_tab_data.RData'))

#---------CONTENT USAGE TAB DATA PREP-------
#overall usage type

cu_overall_ci_rdf <- event_data_2 %>%
  group_by(product_group) %>%
  summarise(n_contents = n_distinct(product, content)) %>%
  ungroup()

cu_overall_avgs_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, userid) %>%
  summarise(n_contents = n_distinct(product, content)) %>%
  ungroup() %>%
  group_by(product_group) %>%
  summarise(avg_contents_per_student = round(mean(n_contents[userroletype == 'Student']),0),
            avg_contents_per_teacher = round(mean(n_contents[userroletype == 'Teacher']),0)) %>%
  ungroup()

cu_overall_rdf <- cu_overall_ci_rdf %>%
  inner_join(y = cu_overall_avgs_rdf, by = c('product_group'))

#by product usage type

cu_by_product_ci_rdf <- event_data_2 %>%
  group_by(product) %>%
  summarise(n_contents = n_distinct(content)) %>%
  ungroup()

cu_by_product_avgs_rdf <- event_data_2 %>%
  group_by(product, userroletype, userid) %>%
  summarise(n_contents = n_distinct(content)) %>%
  ungroup() %>%
  group_by(product) %>%
  summarise(avg_contents_per_student = round(mean(n_contents[userroletype == 'Student']),0),
            avg_contents_per_teacher = round(mean(n_contents[userroletype == 'Teacher']),0)) %>%
  ungroup()

cu_by_product_rdf <- cu_by_product_ci_rdf %>%
  inner_join(y = cu_by_product_avgs_rdf, by = c('product')) 

#pie chart data prep
event_data_2 <- event_data_2 %>%
  mutate(contenttype_2 = if_else(contenttype %in% c('Quiz','Test'), 'Assessment', 'Instruction'))

cu_product_group_wise_tot_users_rdf <- event_data_2 %>%
  group_by(product_group, userroletype) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()

cu_product_wise_tot_users_rdf <- event_data_2 %>%
  group_by(product, userroletype) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()

#overall usage type
cu_overall_both_users_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, userid) %>%
  summarise(both_types = (n_distinct(contenttype_2) == 2)) %>%
  summarise(Both = sum(both_types)) %>%
  ungroup()
  
cu_overall_assess_users_rdf <- event_data_2 %>%
  filter(contenttype_2 == 'Assessment') %>%
  group_by(product_group, userroletype) %>%
  summarise(Assessment = n_distinct(userid)) %>%
  ungroup()

cu_overall_instruct_users_rdf <- event_data_2 %>%
  filter(contenttype_2 == 'Instruction') %>%
  group_by(product_group, userroletype) %>%
  summarise(Instruction = n_distinct(userid)) %>%
  ungroup()


cu_overall_users_rdf <- cu_overall_both_users_rdf %>%
  left_join(y = cu_overall_instruct_users_rdf, by = c('product_group','userroletype')) %>%
  left_join(y = cu_overall_assess_users_rdf, by = c('product_group', 'userroletype')) %>%
  replace_na(list(Assessment = 0)) %>%
  pivot_longer(cols = c(Both, Assessment, Instruction), names_to = 'feature', values_to = 'n_users') %>%
  left_join(y = cu_product_group_wise_tot_users_rdf, by = c('product_group', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2),
         color = case_when(feature == 'Both' ~ '#c2cfce',
                           feature == 'Assessment' ~ '#5efff4',
                           feature == 'Instruction' ~ '#edf57a'))

#by product usage type

cu_by_product_both_users_rdf <- event_data_2 %>%
  group_by(product, userroletype, userid) %>%
  summarise(both_types = (n_distinct(contenttype_2) == 2)) %>%
  summarise(Both = sum(both_types)) %>%
  ungroup()


cu_by_product_assess_users_rdf <- event_data_2 %>%
  filter(contenttype_2 == 'Assessment') %>%
  group_by(product, userroletype) %>%
  summarise(Assessment = n_distinct(userid)) %>%
  ungroup()

cu_by_product_instruct_users_rdf <- event_data_2 %>%
  filter(contenttype_2 == 'Instruction') %>%
  group_by(product, userroletype) %>%
  summarise(Instruction = n_distinct(userid)) %>%
  ungroup()


cu_by_product_users_rdf <- cu_by_product_both_users_rdf %>%
  left_join(y = cu_by_product_instruct_users_rdf, by = c('product','userroletype')) %>%
  left_join(y = cu_by_product_assess_users_rdf, by = c('product', 'userroletype')) %>%
  replace_na(list(Assessment = 0)) %>%
  pivot_longer(cols = c(Both, Assessment, Instruction), names_to = 'feature', values_to = 'n_users') %>%
  left_join(y = cu_product_wise_tot_users_rdf, by = c('product', 'userroletype')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2),
         color = case_when(feature == 'Both' ~ '#c2cfce',
                           feature == 'Assessment' ~ '#5efff4',
                           feature == 'Instruction' ~ '#edf57a'))

#content popularity data prep

cu_product_wise_tot_contents_rdf <- event_data_2 %>%
  group_by(product) %>%
  summarise(tot_contents = n_distinct(content)) %>%
  ungroup()


cu_overall_content_pop_rdf <- event_data_2 %>%
  group_by(product_group, product, userroletype, userid) %>%
  summarise(n_contents = n_distinct(content)) %>%
  ungroup() %>%
  left_join(y = cu_product_wise_tot_contents_rdf, by = c('product')) %>%
  mutate(perc_contents = round(100*n_contents/tot_contents, 2),
         content_pop_labels = case_when(between(perc_contents,0,10) ~ '0% to 10%',
                                        between(perc_contents,10.01,20) ~ '10% to 20%',
                                        between(perc_contents,20.01,30) ~ '20% to 30%',
                                        between(perc_contents,30.01,40) ~ '30% to 40%',
                                        between(perc_contents,40.01,50) ~ '40% to 50%',
                                        between(perc_contents,50.01,60) ~ '50% to 60%',
                                        between(perc_contents,60.01,70) ~ '60% to 70%',
                                        between(perc_contents,70.01,80) ~ '70% to 80%',
                                        between(perc_contents,80.01,90) ~ '80% to 90%',
                                        between(perc_contents,90.01,100) ~ '90% to 100%') %>%
           factor(levels = c('0% to 10%','10% to 20%','20% to 30%','30% to 40%','40% to 50%',
                             '50% to 60%','60% to 70%','70% to 80%','80% to 90%','90% to 100%'))) %>%
  group_by(product_group, userroletype, content_pop_labels) %>%
  summarise(n_users = n()) %>%
  ungroup()

cu_by_product_content_pop_rdf <- event_data_2 %>%
  group_by(product, userroletype, userid) %>%
  summarise(n_contents = n_distinct(content)) %>%
  ungroup() %>%
  left_join(y = cu_product_wise_tot_contents_rdf, by = c('product')) %>%
  mutate(perc_contents = round(100*n_contents/tot_contents, 2),
         content_pop_labels = case_when(between(perc_contents,0,10) ~ '0% to 10%',
                                        between(perc_contents,10.01,20) ~ '10% to 20%',
                                        between(perc_contents,20.01,30) ~ '20% to 30%',
                                        between(perc_contents,30.01,40) ~ '30% to 40%',
                                        between(perc_contents,40.01,50) ~ '40% to 50%',
                                        between(perc_contents,50.01,60) ~ '50% to 60%',
                                        between(perc_contents,60.01,70) ~ '60% to 70%',
                                        between(perc_contents,70.01,80) ~ '70% to 80%',
                                        between(perc_contents,80.01,90) ~ '80% to 90%',
                                        between(perc_contents,90.01,100) ~ '90% to 100%') %>%
           factor(levels = c('0% to 10%','10% to 20%','20% to 30%','30% to 40%','40% to 50%',
                             '50% to 60%','60% to 70%','70% to 80%','80% to 90%','90% to 100%'))) %>%
  group_by(product, userroletype, content_pop_labels) %>%
  summarise(n_users = n()) %>%
  ungroup()

#content type usage

cu_overall_ct_usage_rdf <- event_data_2 %>%
  group_by(product_group, userroletype, contenttype) %>%
  summarise(n_users = n_distinct(userid),
            n_events_per_user = round(n()/n_users,0),
            n_contents = n_distinct(product, content)) %>%
  ungroup() %>%
  mutate(contenttype = factor(contenttype,
                              levels = c('Video', 'Lesson', 'Quiz', 'Audio', 'Interactive Media', 'Document', 'Test')))
  
cu_by_product_ct_usage_rdf <- event_data_2 %>%
  group_by(product, userroletype, contenttype) %>%
  summarise(n_users = n_distinct(userid),
            n_events_per_user = round(n()/n_users,0),
            n_contents = n_distinct(content)) %>%
  ungroup() %>%
  mutate(contenttype = factor(contenttype,
                              levels = c('Video', 'Lesson', 'Quiz', 'Audio', 'Interactive Media', 'Document', 'Test')))

#content type usage over time

cu_ct_uot_pg_tot_users <- event_data_2 %>%
  group_by(product_group) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()

cu_ct_uot_product_tot_users <- event_data_2 %>%
  group_by(product) %>%
  summarise(tot_users = n_distinct(userid)) %>%
  ungroup()


cu_overall_ct_uot_rdf <- event_data_2 %>%
  group_by(product_group, contenttype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  left_join(y = cu_ct_uot_pg_tot_users, by = c('product_group')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2)) %>%
  dplyr::select(-tot_users)

cu_by_product_ct_uot_rdf <- event_data_2 %>%
  group_by(product, contenttype, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  left_join(y = cu_ct_uot_product_tot_users, by = c('product')) %>%
  mutate(perc_users = round(100*n_users/tot_users,2)) %>%
  dplyr::select(-tot_users)

#----NEW DATA APPLIED FROM HERE----
#(continued) content usage table
set.seed(17)
at <- c('Math' = 9, 'Literacy' = 7, 'Science' = 6, 'Social Studies' = 4)
sdt <- c('Math' = 2, 'Literacy' = 1.5, 'Science' = 1.25, 'Social Studies' = 1)

cu_table_data_rdf <- event_data_2 %>%
  group_by(subject, product_group, product, content, title, contenttype) %>%
  summarise(n_students = n_distinct(userid[userroletype == 'Student']),
            n_teachers = n_distinct(userid[userroletype == 'Teacher'])) %>%
  ungroup() %>%
  mutate(avg_time = round(rnorm(n = n(), mean = at[subject], sd = 1),2),
         sd_time = round(rnorm(n = n(), mean = sdt[subject], sd = 0.25),2),
         content = str_to_title(content) %>%
           str_replace(pattern = '_',replacement = ' '),
         title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = ' ')) %>%
  dplyr::select(-subject)


save(cu_overall_rdf,
     cu_by_product_rdf,
     cu_overall_users_rdf,
     cu_by_product_users_rdf,
     cu_overall_content_pop_rdf,
     cu_by_product_content_pop_rdf,
     cu_overall_ct_usage_rdf,
     cu_by_product_ct_usage_rdf,
     cu_by_product_ct_uot_rdf,
     cu_overall_ct_uot_rdf,
     cu_table_data_rdf,
     file = file.path(folder_path,'content_usage_tab_data.RData'))

#-----ASSESSMENT ANALYSIS TAB DATA PREP-----
#all assessment names

aa_product_assess_data <- item_response_data %>%
  distinct(product, test_id) %>%
  mutate(test_id = str_to_upper(test_id) %>%
           factor(levels = gtools::mixedsort(.))) %>%
  arrange(test_id) %>%
  mutate(test_id = as.character(test_id))


#infobox data

aa_avg_assess_per_student_rdf <- item_response_data %>%
  group_by(product, userid) %>%
  summarise(n_assess = n_distinct(test_id)) %>% 
  summarise(avg_assess_per_student = round(mean(n_assess, na.rm = TRUE)))

aa_assess_ir_rdf <- item_response_data %>%
  group_by(product) %>%
  summarise(n_assess = n_distinct(test_id),
            n_item_responses = n())

aa_infobox_data_rdf <- aa_avg_assess_per_student_rdf %>%
  inner_join(y = aa_assess_ir_rdf, by = 'product')

#assessment alpha calculation

test <- data.frame(c1 = c(3,3,3,1,1,1,2,2,2), c2 = c('i1','i2','i3','i1','i2','i3','i1','i2','i3'), c3 = c(1,0,0,0,0,1,1,1,0))

test %>%
  pivot_wider(names_from = c2,values_from = c3)

aa_assessments_rdf <- item_response_data %>%
  distinct(product, test_id)

aa_assess_alpha_values <- map_dbl(1:nrow(aa_assessments_rdf), function(x){
  
  df <- item_response_data %>%
    filter(test_id %in% aa_assessments_rdf$test_id[x]) %>%
    dplyr::select(userid, item_id, score, timestamp) %>%
    group_by(userid, item_id) %>%
    mutate(first_attempt = (timestamp == min(timestamp))) %>%
    ungroup() %>%
    filter(first_attempt) %>%
    dplyr::select(-first_attempt, -timestamp) %>%
    pivot_wider(names_from = item_id, values_from = score) %>%
    dplyr::select(-userid)
  
  ltm::cronbach.alpha(df)$alpha
  
})

aa_assessments_alpha_rdf <- aa_assessments_rdf %>%
  mutate(test_id = str_to_upper(test_id),
         alpha = aa_assess_alpha_values)

#assessment summmary table data

aa_assess_table_data_1_rdf <- item_response_data %>%
  group_by(product, test_id) %>%
  summarise(n_items = n_distinct(item_id),
            n_skills = str_split(skill_id, pattern = ',') %>%
              unlist() %>%
              n_distinct()) %>%
  ungroup() %>%
  mutate(assess_type = if_else(n_items == 5, 'Quiz','Test'))

aa_assess_table_data_2_rdf <- item_response_data %>%
  group_by(product,test_id,userid) %>%
  summarise(user_score = round(mean(score)*100,2)) %>%
  summarise(n_students = n(),
            avg_score = round(mean(user_score),2),
            sd_score = round(sd(user_score),2)) %>%
  ungroup()

aa_assess_table_data_rdf <- aa_assess_table_data_1_rdf %>%
  inner_join(aa_assess_table_data_2_rdf, by = c('product','test_id')) %>%
  dplyr::select(product, test_id, assess_type, n_items, n_skills, n_students, avg_score, sd_score) %>%
  mutate(test_id = str_to_upper(test_id) %>%
           factor(levels = gtools::mixedsort(.))) %>%
  arrange(test_id) %>%
  mutate(test_id = as.character(test_id)) %>%
  inner_join(y = aa_assessments_alpha_rdf, by = c('product', 'test_id'))

#assessment score distribution

aa_assess_score_dist_rdf <- item_response_data %>%
  group_by(product, test_id, userid) %>%
  summarise(user_score = round(mean(score)*100,0)) %>%
  ungroup() %>%
  mutate(perc_score_levels = case_when(user_score<=10 ~ '0% - 10%',
                                       between(user_score,11,20) ~ '11% - 20%',
                                       between(user_score,21,30) ~ '21% - 30%',
                                       between(user_score,31,40) ~ '31% - 40%',
                                       between(user_score,41,50) ~ '41% - 50%',
                                       between(user_score,51,60) ~ '51% - 60%',
                                       between(user_score,61,70) ~ '61% - 70%',
                                       between(user_score,71,80) ~ '71% - 80%',
                                       between(user_score,81,90) ~ '81% - 90%',
                                       between(user_score,91,100) ~ '91% - 100%') %>%
           factor(levels = c('0% - 10%','11% - 20%','21% - 30%','31% - 40%','41% - 50%',
                             '51% - 60%','61% - 70%','71% - 80%','81% - 90%','91% - 100%'))) %>%
  group_by(product, test_id, perc_score_levels) %>%
  summarise(n_users = n()) %>%
  ungroup() %>%
  mutate(test_id = str_to_upper(test_id))


#biserial correlation calculation

item_response_data_2 <- item_response_data %>%
  group_by(userid, test_id, item_id) %>%
  mutate(first_attempt = (timestamp == min(timestamp))) %>%
  ungroup() %>%
  filter(first_attempt) %>%
  dplyr::select(-first_attempt) %>%
  group_by(test_id, userid) %>%
  mutate(test_score = round(mean(score*100),2)) %>%
  ungroup() %>%
  split(list(.$test_id, .$item_id))

item_response_data_3 <- keep(item_response_data_2, function(x){
  nrow(x)>0
})

aa_item_biserial_corr_rdf <- map_dfr(item_response_data_3, function(df){

  corr <- ltm::biserial.cor(df$test_score, df$score, level = 2)
  data.frame(test_id = df$test_id[1], item_id = df$item_id[1], correlation = corr)
}) %>%
  mutate(test_id = str_to_upper(test_id),
         item_id = str_to_title(item_id))

#item summary data

set.seed(6)
aa_avg_med_time <- c(200,150,100,50)

aa_item_table_data_rdf <- item_response_data %>%
  group_by(product, test_id, item_id) %>%
  summarise(n_skills = str_split(skill_id, pattern = ',') %>%
              unlist() %>%
              n_distinct(), 
            skills = str_split(skill_id, pattern = ',') %>%
              unlist() %>%
              unique() %>%
              paste(collapse = ','),
            n_students = n_distinct(userid),
            n_responses = n(),
            avg_score = round(mean(score)*100,2),
            sd_score = round(sd(score)*100,2)) %>%
  ungroup() %>%
  mutate(test_id = str_to_upper(test_id) %>%
           factor(levels = gtools::mixedsort(unique(.))),
         item_id = str_to_title(item_id) %>%
           factor(levels = gtools::mixedsort(unique(.))),
         skills = str_to_title(skills),
         median_time = rpois(n = n(),
                             lambda = aa_avg_med_time[sample(x = c(1,2,3,4),
                                                          size = n(),
                                                          prob = c(0.15,0.25,0.5,0.1),
                                                          replace = TRUE)])) %>%
  arrange(test_id, item_id) %>%
  mutate(test_id = as.character(test_id),
         item_id = as.character(item_id)) %>%
  inner_join(y = aa_item_biserial_corr_rdf, by = c('test_id','item_id'))

aa_assess_table_data_rdf <- aa_assess_table_data_rdf %>%
  mutate(alpha = round(alpha,2))

aa_item_table_data_rdf <- aa_item_table_data_rdf %>%
  mutate(correlation = round(correlation, 2))

save(aa_product_assess_data,
     aa_infobox_data_rdf,
     aa_assess_table_data_rdf,
     aa_assess_score_dist_rdf,
     aa_item_table_data_rdf,
     file = file.path(folder_path,'assessment_analysis_tab_data.RData'))

#------SKILL PERFORMANCE TAB DATA PREP-----

#infobox data prep
sp_product_max_skills_rdf <- item_response_data %>%
  group_by(product) %>%
  summarise(max_skills = str_split(skill_id, pattern = ',') %>%
              unlist() %>%
              n_distinct())

sp_product_avg_items_per_skill_rdf <- item_response_data %>%
  distinct(product, test_id, item_id, skill_id) %>%
  separate_rows(skill_id, sep = ',') %>%
  group_by(product, skill_id) %>%
  summarise(n_items = n()) %>%
  summarise(avg_items_per_skill = round(mean(n_items),1),
            med_items_per_skill = median(n_items),
            sd_items_per_skill = round(sd(n_items),1))


sp_item_skills_rdf <- item_response_data %>%
  distinct(product, test_id, item_id, skill_id) %>%
  separate_rows(skill_id, sep = ',') %>%
  rename(skill_id_v2 = skill_id)

item_response_data_2 <- item_response_data %>%
  left_join(y = sp_item_skills_rdf, by = c('product','test_id','item_id'))


sp_avg_students_per_skill_rdf <- item_response_data_2 %>%
  group_by(product, skill_id_v2) %>%
  summarise(n_students = n_distinct(userid)) %>%
  summarise(avg_students_per_skill = round(mean(n_students),0),
            med_students_per_skill = round(median(n_students),0),
            sd_students_per_skill = round(sd(n_students),0))


sp_avg_skills_per_student <- item_response_data %>%
  group_by(product, userid) %>%
  summarise(n_skills = str_split(skill_id, pattern = ',') %>%
              unlist() %>%
              n_distinct()) %>%
  summarise(avg_skills_per_student = round(mean(n_skills),1),
            med_skills_per_student = median(n_skills),
            sd_skills_per_student = round(sd(n_skills),1))


sp_infobox_rdf <- sp_product_max_skills_rdf %>%
  inner_join(sp_product_avg_items_per_skill_rdf, by = 'product') %>%
  inner_join(sp_avg_students_per_skill_rdf, by = 'product') %>%
  inner_join(sp_avg_skills_per_student, by = 'product')


#skill pacing data prep

sp_skill_pacing_data_rdf <- item_response_data_2 %>%
  mutate(date_binned_by_week = ceiling_date(date(timestamp), unit = 'week')) %>%
  group_by(product, skill_id_v2, date_binned_by_week) %>%
  summarise(n_students = n_distinct(userid),
            avg_score = round(mean(score)*100,2)) %>%
  ungroup() %>%
  mutate(skill_id_v2 = str_to_title(skill_id_v2)) %>%
  rename(skill_id = skill_id_v2)

sp_skill_start_end_week_rdf <- sp_skill_pacing_data_rdf %>%
  group_by(product, skill_id) %>%
  summarise(start_week = min(date_binned_by_week),
            end_week = max(date_binned_by_week)) %>%
  ungroup()

#skill performance table data prep

sp_skills_table_rdf <- item_response_data_2 %>%
  group_by(product, skill_id_v2) %>%
  summarise(n_responses = n(),
            n_items = n_distinct(test_id, item_id),
            n_students = n_distinct(userid),
            avg_score = round(mean(score)*100,2),
            sd_score = round(sd(score)*100,2)) %>%
  ungroup() %>%
  mutate(skill_id_v2 = str_to_title(skill_id_v2) %>%
           factor(levels = gtools::mixedsort(.))) %>%
  rename(skill_id = skill_id_v2) %>%
  arrange(product, skill_id)

save(sp_product_max_skills_rdf,
     sp_infobox_rdf,
     sp_skill_pacing_data_rdf,
     sp_skill_start_end_week_rdf,
     sp_skills_table_rdf,
     file = file.path(folder_path,'skill_performance_tab_data.RData'))

#------CURRICULUM PACING DATA PREP------
#labels data

cp_topic_lesson_rdf <- event_data_2 %>%
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

cp_topic_content_rdf <- event_data_2 %>%
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

cp_usage_heatmap_rdf <- event_data_2 %>%
  group_by(product, userroletype, title, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid),
            n_events = n(),
            n_events_per_user = round(n_events/n_users,2)) %>%
  ungroup() %>%
  mutate(title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = '') %>%
           str_replace(pattern = 'test', replacement = ' Test') %>%
           factor(levels = gtools::mixedsort(unique(.)))) %>%
  arrange(product, title) %>%
  mutate(title = as.character(title))

#assessment score pacing

cp_assess_score_rdf <- item_response_data %>%
  mutate(date_binned_by_week = ceiling_date(date(timestamp),unit = 'week')) %>%
  group_by(product, test_id, date_binned_by_week, userid) %>%
  summarise(user_score = mean(score*100)) %>%
  summarise(avg_score = round(mean(user_score),2)) %>%
  ungroup()


#performance heatmap data prep

cp_perf_heatmap_rdf <- event_data_2 %>%
  filter(contenttype %in% c('Quiz','Test'), userroletype %in% c('Student')) %>%
  group_by(product, product_id, title, content, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  mutate(test_id = paste0(product_id,'_',content)) %>%
  inner_join(y = cp_assess_score_rdf, by = c('product','test_id','date_binned_by_week')) %>%
  mutate(title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = '') %>%
           str_replace(pattern = 'test', replacement = ' Test') %>%
           factor(levels = gtools::mixedsort(unique(.)))) %>%
  arrange(product, title) %>%
  mutate(title = as.character(title)) %>%
  dplyr::select(-product_id,-test_id,-content)

#performance dotchart data prep

cp_perf_dotchart_rdf <- event_data_2 %>%
  group_by(product, product_id, title, content, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid)) %>%
  ungroup() %>%
  mutate(test_id = paste0(product_id,'_',content)) %>%
  left_join(y = cp_assess_score_rdf, by = c('product','test_id','date_binned_by_week')) %>%
  mutate(title = str_to_title(title) %>%
           str_replace_all(pattern = '_', replacement = '') %>%
           str_replace(pattern = 'test', replacement = ' Test') %>%
           factor(levels = gtools::mixedsort(unique(.))),
         content = str_to_title(content) %>%
           str_replace(pattern = '_', replacement = ' ') %>%
           factor(levels = gtools::mixedsort(unique(.)))) %>%
  arrange(product, title, content) %>%
  mutate(title = as.character(title),
         content = as.character(content)) %>%
  dplyr::select(-product_id, -test_id)

save(cp_usage_heatmap_rdf,
     cp_topic_lesson_rdf,
     cp_topic_content_rdf,
     cp_perf_heatmap_rdf,
     cp_perf_dotchart_rdf,
     file = file.path(folder_path,'curriculum_pacing_tab_data.RData'))

#------LEADERBOARD TAB DATA PREP-------
#productwsie leaderboard data

dl_product_wise_rdf <- event_data_2 %>%
  group_by(product, districtname, userroletype, userid) %>%
  summarise(n_active_days = n_distinct(date),
            n_contents = n_distinct(content),
            n_events = n()) %>%
  ungroup() %>%
  group_by(product, districtname) %>%
  summarise(avg_active_days = round(mean(n_active_days,na.rm = TRUE),0),
            avg_contents = round(mean(n_contents,na.rm = TRUE),0),
            n_students = sum(userroletype == 'Student'),
            n_teachers = sum(userroletype == 'Teacher'),
            avg_student_events = round(mean(n_events[userroletype == 'Student'],na.rm = TRUE),0),
            avg_teacher_events = round(mean(n_events[userroletype == 'Teacher'], na.rm = TRUE),0)) %>%
  ungroup() %>%
  replace_na(list(avg_active_days = 0, avg_contents = 0, avg_student_events = 0, avg_teacher_events = 0)) %>%
  mutate(districtname = str_to_title(districtname))


#product wise new vs returning plot

dl_product_wise_nvr_rdf <- event_data_2 %>%
  group_by(product, userid) %>%
  mutate(first = (date_binned_by_week == min(date_binned_by_week))) %>%
  ungroup() %>%
  group_by(product ,districtname, date_binned_by_week) %>%
  summarise(new = n_distinct(userid[first]),
            returning = n_distinct(userid[!first])) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname))

#product wise events per user over time

dl_product_wise_eot_rdf <- event_data_2 %>%
  group_by(product, districtname, date_binned_by_week) %>%
  summarise(n_users = n_distinct(userid),
            n_events_per_user = round(n()/n_users,0)) %>%
  ungroup() %>%
  mutate(districtname = str_to_title(districtname))

save(
  dl_product_wise_rdf,
  dl_product_wise_nvr_rdf,
  dl_product_wise_eot_rdf,
  file = file.path(folder_path,'leaderboard_tab_data.RData')
)


file.copy(file.path(folder_path, list.files(folder_path)), file.path(getActiveProject(), 'curriculum_analytics', 'data'),
          overwrite = TRUE)

#-----

