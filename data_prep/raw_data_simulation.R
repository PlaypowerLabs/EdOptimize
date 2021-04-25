library(tidyverse)
library(lubridate)
library(magrittr)
library(rstudioapi)

load('us_states_metadata.RData')
folder_path <- file.path(getActiveProject(), 'data_prep')

#-------DISTRICT DATA PREP---------
set.seed(18)
avg_n_districts <- c('lev_1' = 3, 'lev_2' = 7, 'lev_3' = 11, 'lev_4' = 15, 'lev_5' = 21)
# states_names_df <- data.frame(states_names = states_names,
#                               n_districts = c(rep('lev_5',3), rep('lev_4', 5), rep('lev_3', 10),
#                                               rep('lev_2',22), rep('lev_1',16)))
states_names_df <- data.frame(states_names = states_names[1:5],
                              n_districts = c(rep('lev_5',1), rep('lev_4', 1), rep('lev_3', 1),
                                              rep('lev_2',1), rep('lev_1',1)))
states_names_df$states_names <- as.character(states_names_df$states_names)
states_names_df$n_districts <- as.character(states_names_df$n_districts)
n_districts <- rpois(n = nrow(states_names_df), lambda = avg_n_districts[states_names_df$n_districts])
n_districts[n_districts == 0] <- 1
tot_districts <- sum(n_districts)

states_districts_df <- tibble(state = rep(states_names_df$states_names, times = n_districts),
                              districtname = paste0('districtname_',1:tot_districts))

#-----------SCHOOL DATA PREP---------
set.seed(17)
avg_n_schools <- c('low' = 2, 'med' = 4, 'high' = 6)
samples <- sample(x = c('high', 'med', 'low'),
                  size = nrow(states_districts_df),
                  prob = c(0.12,0.37,0.51),
                  replace = TRUE)
n_schools <- rpois(n = nrow(states_districts_df), lambda = avg_n_schools[samples])
n_schools[n_schools == 0] <- 1
tot_schools <- sum(n_schools)

states_schools_df <- tibble(state = rep(states_districts_df$state, times = n_schools),
                            districtname = rep(states_districts_df$districtname, times = n_schools),
                            schoolname = paste0('schoolname_', 1:tot_schools))

#-------CLASS DATA PREP-------
grades <- c('K', as.character(1:12))
len <- length(grades)
tot_classes <- nrow(states_schools_df)*len


class_data <- tibble(state = rep(states_schools_df$state, each = len),
                     districtname = rep(states_schools_df$districtname, each = len), 
                     schoolname = rep(states_schools_df$schoolname, each = len),
                     grade = rep(grades, times = nrow(states_schools_df)),
                     classname = paste0('classname_', 1:tot_classes))

#-------USER DATA PREP---------
set.seed(19)
avg_n_students <- c(3,5,7,9,10,11,13,18,14,16,15,12,8)
names(avg_n_students) <- grades
   
n_students <- rpois(n = nrow(class_data), lambda = avg_n_students[class_data$grade])
n_students[n_students == 0] <- 1
tot_students <- sum(n_students)

student_data <- tibble(state = rep(class_data$state, times = n_students),
                       districtname = rep(class_data$districtname, times = n_students),
                       schoolname = rep(class_data$schoolname, times = n_students),
                       grade = rep(class_data$grade, times = n_students),
                       classname = rep(class_data$classname, times = n_students),
                       userroletype = rep('Student', times = tot_students),
                       userid = paste0('userid_', 1:tot_students),
                       subject = NA,
                       ability = runif(n = tot_students, min = 0.05, max = 0.95))


set.seed(27)
n_teachers <- sample(x = c(1,2,3,4),
                     size = nrow(class_data),
                     prob = c(0.14,0.28,0.22,0.26),
                     replace = TRUE)
tot_teachers <- sum(n_teachers)
next_uid <- tot_students+1

subject <- rep(NA, times = tot_teachers)
iter <- 1

for(i in seq(from = 1, to = length(n_teachers))){
  
  subjects <- sample(c('Math', 'Science', 'Literacy', 'Social Studies'),
                     size = n_teachers[i],
                     prob = c(0.43, 0.27, 0.23, 0.17),
                     replace = FALSE)
  
  subject[iter:(iter+n_teachers[i]-1)] <- subjects
  iter <- iter + n_teachers[i]
}


teacher_data <- tibble(state = rep(class_data$state, times = n_teachers),
                       districtname = rep(class_data$districtname, times = n_teachers),
                       schoolname = rep(class_data$schoolname, times = n_teachers),
                       grade = rep(class_data$grade, times = n_teachers),
                       classname = rep(class_data$classname, times = n_teachers),
                       userroletype = rep('Teacher', times = tot_teachers),
                       userid = paste0('userid_', next_uid:(next_uid+tot_teachers-1)),
                       subject = subject)

user_data <- list(Student = student_data, Teacher = teacher_data)


#-------COURSE METADATA PREP--------
set.seed(27)

avg_n_topic <- data.frame(lower = c(3,3,4,4,4,4,5,5,5,5,5,5,5), upper = c(5,5,6,6,6,6,8,8,8,8,8,8,8))
rownames(avg_n_topic) <- grades
n_lessons_per_topic <- c(lower = 3, upper = 6)
n_contents_per_lesson <- c(lower = 5, upper = 10)
content_type_df <- data.frame(content_type = c('Interactive Media','Audio','Document','Lesson','Video'),
                                           probability = c(1,0.23,0.34,1.59,0.73))
content_levels <- c('Video', 'Lesson', 'Audio', 'Interactive Media', 'Document','Quiz','Test')

programs <- c(rep('K-2',3), rep('3-5', 3), rep('6-8',3), rep('9-12',4))
names(programs) <- grades

           
           

pr_id <- 1
ct_area <- 1
skill_id <- 1
create_course_metadata <- function(subject){
  
  final_list <- list()
  
  for(grade in grades){
    
    product_group <- paste(subject,'Grades',programs[grade],sep = ' ')
    product <- paste(product_group,'Grade',grade, sep = ' ')
    product_id <- paste0('product_id_',pr_id)
    n_topic <- rdunif(n = 1, b = avg_n_topic[grade,'upper'], a = avg_n_topic[grade,'lower'])
    topics <- paste0('topic_',1:n_topic)
    content_area <- paste0('content_area_',ct_area:(ct_area+n_topic-1))
    n_lessons <- rdunif(n = n_topic, b = n_lessons_per_topic['upper'], a = n_lessons_per_topic['lower']) + 1
    tot_lessons <- sum(n_lessons)
    intermediate_df <- tibble(product_group = rep(product_group, tot_lessons),
                              product = rep(product, tot_lessons),
                              product_id = rep(product_id, tot_lessons),
                              product_content = rep(topics, times = n_lessons),
                              content_area = rep(content_area, times = n_lessons))
                             
    
    intermediate_df_2 <- intermediate_df %>%
      group_by(product_content) %>%
      mutate(topic_content = c(paste0('lesson_',1:(n()-1)), 'topic_test'),
             skill_id = NA)
    
    intermediate_df_2$skill_id[intermediate_df_2$topic_content != 'topic_test'] <- paste0('skill_',skill_id:(skill_id + tot_lessons - length(n_lessons) - 1))
    
    topic_test_skills <- intermediate_df_2 %>%
      filter(!(topic_content == 'topic_test')) %>%
      group_by(product_content) %>%
      summarise(x = paste(skill_id, collapse = ',')) %>%
      .$x
    
    intermediate_df_2$skill_id[intermediate_df_2$topic_content == 'topic_test'] <- topic_test_skills
    
    topic_test_indexes <- which(intermediate_df_2$topic_content == 'topic_test')
    
    n_contents <- rdunif(n = nrow(intermediate_df_2), b = n_contents_per_lesson['upper'], a = n_contents_per_lesson['lower']) + 1
    n_contents[topic_test_indexes] <- 1
    tot_contents <- sum(n_contents)
    ct <- sample(x = content_type_df$content_type,
                          size = tot_contents,
                          prob = content_type_df$probability,
                          replace = TRUE)
    
    final_df <- tibble(product_group = rep(product_group, tot_contents),
                       product = rep(product, tot_contents),
                       product_id = rep(product_id, tot_contents),
                       product_content = rep(intermediate_df_2$product_content, times = n_contents),
                       content_area = rep(intermediate_df_2$content_area, times = n_contents),
                       topic_content = rep(intermediate_df_2$topic_content, times = n_contents),
                       contenttype = as.character(ct),
                       skill_id = rep(intermediate_df_2$skill_id, times = n_contents))
                       
    
    final_df <- final_df %>%
      mutate(contenttype = if_else(topic_content == 'topic_test', 'Test', contenttype)) %>%
      group_by(product_content, topic_content) %>%
      mutate(contenttype = if_else(row_number() == n() & topic_content != 'topic_test',
                                   'Quiz',contenttype) %>%
               factor(levels = content_levels) %>%
               sort()) %>%
      ungroup() %>%
      mutate(content = paste0('content_', 1:nrow(final_df)),
             contenttype = as.character(contenttype))
    
    final_list[[grade]] <- final_df
    pr_id <<- pr_id+1
    ct_area <<- ct_area+n_topic
    skill_id <<- skill_id + tot_lessons - length(n_lessons)
    
  }
  
  final_list
  
}

math_metadata <- create_course_metadata('Math')
literacy_metadata <- create_course_metadata('Literacy')
science_metadata <- create_course_metadata('Science')
social_studies_metadata <- create_course_metadata('Social Studies')

all_subject_course_metadata <- list(
  Math = math_metadata,
  Literacy = literacy_metadata,
  Science = science_metadata,
  'Social Studies' = social_studies_metadata
)

rm(math_metadata, literacy_metadata, science_metadata, social_studies_metadata)


#-----ASSESSMENT ITEM DATA PREP---------
set.seed(19)

create_assessment_item_data <- function(subject){
  
  final_list <- list()
  
  for(grade in grades){
    
    course_metadata <- all_subject_course_metadata[[subject]][[grade]]
    
    quiz_data <- course_metadata %>%
      filter(contenttype %in% 'Quiz') %>%
      select(product, product_id, content, skill_id) %>%
      unite(col = 'test_id', c(product_id,content))
    
    quiz_item_data <- tibble(product = rep(quiz_data$product, each = 5),
                             test_id = rep(quiz_data$test_id, each = 5),
                             item_id = rep(paste0('item_',1:5), times = nrow(quiz_data)),
                             skill_id = rep(quiz_data$skill_id, each = 5))
    
    test_data <- course_metadata %>%
      filter(contenttype %in% 'Test') %>%
      select(product, product_id, content, skill_id) %>%
      unite(col = 'test_id', c(product_id,content))
    
    test_item_data <- tibble(product = rep(test_data$product, each = 10),
                             test_id = rep(test_data$test_id, each = 10),
                             item_id = rep(paste0('item_',1:10), times = nrow(test_data)),
                             skill_id = rep(test_data$skill_id, each = 10)) %>%
      mutate(skill_id = str_split(skill_id, pattern = ',') %>%
               map_chr(function(x){
                 sample(x, size = sample(1:length(x), size = 1)) %>%
                   paste(collapse = ',')
               }))
    
    final_item_data <- bind_rows(quiz_item_data,
                                 test_item_data)
    
    final_list[[grade]] <- final_item_data
      
  }
 final_list 
}

math_assessment_item_data <- create_assessment_item_data('Math')
literacy_assessment_item_data <- create_assessment_item_data('Literacy')
science_assessment_item_data <- create_assessment_item_data('Science')
social_studies_assessment_item_data <- create_assessment_item_data('Social Studies')

all_subject_assessment_item_data <- list(
  Math = math_assessment_item_data,
  Literacy = literacy_assessment_item_data,
  Science = science_assessment_item_data,
  'Social Studies' = social_studies_assessment_item_data
)

#----------EVENT DATA PREP-----------
set.seed(9)

avg_sessions_per_user <- data.frame(Student = c(30,31,32,35,36,37,38,40,43,47,41,39,42),
                                Teacher = c(50,49,47,46,48,45,44,41,43,42,40,39,37))
rownames(avg_sessions_per_user) <- grades
avg_events_per_session <- c('Math' = 3, 'Literacy' = 2.67, 'Science' = 2.33, 'Social Studies' = 2)
column_order <- c('sessionid','state','districtname','schoolname','classname','subject','product_group','product','product_id',
                  'userroletype','userid','content','contenttype','content_area','eventtype','timestamp')
subjects <- c('Math', 'Literacy', 'Science','Social Studies')
n_district_coverage <- data.frame(lower = c(0.1,0.1,0.1,0.1), upper = c(0.47,0.38,0.32,0.27))
rownames(n_district_coverage) <- subjects
bouncing_user_prob <- data.frame(Student = c(0.25,0.29,0.33,0.37), Teacher = c(0.14,0.17,0.2,0.27))
rownames(bouncing_user_prob) <- subjects
n_assess_contents <- list(
  Math = data.frame(lower = seq(from = 0.05, to = 0.15, length.out = 13),
                    upper = seq(from = 0.3, to = 0.5, length.out = 13)),
  Science = data.frame(lower = seq(from = 0.04, to = 0.12, length.out = 13),
                       upper = seq(from = 0.25, to = 0.4, length.out = 13)),
  Literacy = data.frame(lower = seq(from = 0.03, to = 0.1, length.out = 13),
                        upper = seq(from = 0.2, to = 0.35, length.out = 13)),
  'Social Studies' = data.frame(lower = seq(from = 0.02, to = 0.09, length.out = 13),
                                upper = seq(from = 0.15, to = 0.3, length.out = 13))
)
 
n_assess_contents <- lapply(n_assess_contents, function(x){
  rownames(x) <- grades
  x
})


start_date <- as.Date('2018/8/1')
end_date <- as.Date('2019/7/31')
start_week <- ceiling_date(start_date,unit = 'week')
end_week <- ceiling_date(end_date, unit = 'week')
probs <- c('8' = 0.78, '9' = 1.25, '10' = 1.27, '11' = 1.09, '12' = 0.35, '1' = 0.97, '2' = 1.05, '3' = 1.25, '4' = 0.94, '5' = 1.03, '6' = 0.95, '7' = 0.91)
weeks <- seq.Date(from = start_week, to = end_week, by = 'week')
weekly_usage_probs <- data.frame(week = weeks,
                                 probability = rnorm(n = length(weeks),
                                                     mean = probs[as.character(month(weeks))],
                                                     sd = 0.2) %>%
                                   round(digits = 2)) %>%
  mutate(probability = if_else(probability<0, 0, probability))
 

dates <- seq.Date(from = start_date, to = end_date, by = 'day')
date_probs_df <- data.frame(date = dates,
                            week = ceiling_date(dates, unit = 'week')) %>%
  left_join(weekly_usage_probs, by = 'week') %>%
  mutate(probability = rnorm(n = length(dates),
                             mean = probability,
                             sd = 0.1) %>%
           round(digits = 2))

avg_hour <- c('Morning' = 10, 'Evening' = 15)

max_session_duration <- data.frame(Math = rpois(n = length(dates), lambda = 35),
                               Literacy = rpois(n = length(dates), lambda = 30),
                               Science = rpois(n = length(dates), lambda = 25),
                               `Social Studies` = rpois(n = length(dates), lambda = 20))
colnames(max_session_duration) <- subjects
rownames(max_session_duration) <- as.character(dates)

student_start_prob <- c('8' = 2.07, '9' = 1.45, '10' = 1.23, '11' = 1.08, '12' = 0.23, '1' = 1.34, '2' = 0.99, '3' = 0, '4' = 0, '5' = 0, '6' = 0, '7' = 0)
student_start_week_df <- data.frame(week = weeks,
                                     probability = rnorm(n = length(weeks),
                                                         mean = student_start_prob[as.character(month(weeks))],
                                                         sd = 0.2)) %>%
  mutate(probability = if_else((probability<0 | week>as.Date('2019/3/1')),0,probability))
student_start_week_df[nrow(student_start_week_df),'probability'] <- 0

n_session_reduction <- c('8' = 100, '9' = 92, '10' = 84, '11' = 75, '12' = 68, '1' = 60, '2' = 55)/100
content_access_reduction <- c('8' = 0, '9' = 7, '10' = 15, '11' = 23, '12' = 30, '1' = 37, '2' = 45)/100


productwise_districts <- function(){
  data <- list()
  
  for(sub in subjects){
    for (gr in grades) {
      
      n_districts <- round(runif(n = 1,
                                 min = n_district_coverage[sub,'lower'],
                                 max = n_district_coverage[sub,'upper'])*tot_districts, 0)
      
      districts <- sample(x = states_districts_df$districtname,
                          size = n_districts)
      
      df <- tibble(subject = sub,
                   grade = gr,
                   district = districts)
      
      element_id <- paste0(sub,'_',gr)
      data[[element_id]] <- df
    }
  }
  
  data
}

productwise_districts_rdf <- productwise_districts() %>%
  bind_rows()

next_session <- 1
create_event_data <- function(userroletype){
  
  event_data <- list()
  
  for(sub in subjects){
    
    for (gr in grades) {
      
      districts <- productwise_districts_rdf %>%
        filter(subject == sub, grade == gr) %>%
        .$district
        
      data <- user_data[[userroletype]] %>%
        filter(grade == gr, districtname %in% districts)
      
      if(userroletype == 'Teacher'){
        data <- data %>%
          filter(subject %in% sub)
      }
      
      product_group_grade <- programs[gr]
      pr_id <- all_subject_course_metadata[[sub]][[gr]]$product_id[1]
      n_users <- nrow(data)
      
      n_sessions <- rpois(n = n_users,
                          lambda = avg_sessions_per_user[data$grade,userroletype])
      
      student_start_week <- sample(x = student_start_week_df$week,
                                    size = n_users,
                                    prob = student_start_week_df$probability,
                                    replace = TRUE)
      student_start_month <- month(student_start_week)
      
      n_sessions <- round(n_sessions*n_session_reduction[as.character(student_start_month)],0)
      n_sessions[n_sessions == 0] <- 1
      
      bouncing_user <- sample(x = c(TRUE,FALSE),
                              size = n_users,
                              prob = c(bouncing_user_prob[sub, userroletype], 1-bouncing_user_prob[sub, userroletype]),
                              replace = TRUE)
      
      n_sessions[bouncing_user] <- 1
      tot_sessions <- sum(n_sessions)
      
      n_events <- rpois(n = tot_sessions,
                        lambda = avg_events_per_session[sub])
      n_events[n_events<=1] <- 2
      
      tot_events <- sum(n_events)
      
      content <- rep(NA, tot_events)
      timestamp <- rep(NA, tot_events)
      contenttype <- rep(NA, tot_events)
      content_area <- rep(NA, tot_events)
      state <- rep(NA,tot_events)
      districtname <- rep(NA,tot_events)
      schoolname <- rep(NA, tot_events)
      classname <- rep(NA, tot_events)
      userid <- rep(NA, tot_events)
      iter <- 1
      event_iter <- 1
      
      for(user in seq(from = 1, to = n_users)){
        
        print(user)
        
        if(user == 1){
          instruct_contents <- all_subject_course_metadata[[sub]][[data$grade[user]]] %>%
            filter(!contenttype %in% c('Test', 'Quiz')) %>%
            .$content %>%
            str_extract(pattern = '[[:digit:]]+') %>%
            as.numeric()
          
          assessment_contents <- all_subject_course_metadata[[sub]][[data$grade[user]]] %>%
            filter(contenttype %in% c('Test', 'Quiz')) %>%
            .$content %>%
            str_extract(pattern = '[[:digit:]]+') %>%
            as.numeric()
          
          n_user_contents <- nrow(all_subject_course_metadata[[sub]][[data$grade[user]]])
        }
        
        content_start <- round((content_access_reduction[as.character(student_start_month[user])])*n_user_contents,0)
        content_start[content_start == 0] <- 1
        
        
        
        week_selection <- weekly_usage_probs %>%
          filter(week>=student_start_week[user])
        
        weeks <- sample(x = week_selection$week,
                        size = round((nrow(week_selection)*0.48),0),
                        prob = week_selection$probability)
        
        date_selection <- date_probs_df %>%
          filter(week %in% weeks)
        
        n_user_sessions <- n_sessions[user]
        n_contents <- sum(n_events[iter:(iter+n_user_sessions-1)])
        n_assessments <- round(n_contents*runif(n = 1,
                                                min = n_assess_contents[[sub]][gr,'lower'],
                                                max = n_assess_contents[[sub]][gr,'upper']),0)
        n_instructs <- n_contents-n_assessments
        
        user_instruct_contents <- sample(x = instruct_contents[instruct_contents>=content_start],
                                         size = n_instructs,
                                         replace = TRUE)
        
        user_assessment_contents <- sample(x = assessment_contents[assessment_contents>=content_start],
                                         size = n_assessments,
                                         replace = TRUE)
        
        
        content_num <- c(user_instruct_contents, user_assessment_contents) %>%
          sort()
        
        dates <- sample(x = date_selection$date,
                        size = n_user_sessions,
                        prob = date_selection$probability,
                        replace = TRUE)
        dates <- sort(dates)
        session_start_timestamp <- as.POSIXct(dates)
        
        max_duration <- max_session_duration[as.character(dates),sub] %>%
          rep(times = n_events[iter:(iter+n_user_sessions-1)])
        
        day_time <- sample(x = c('Morning', 'Evening'),
                           size = n_user_sessions,
                           replace = TRUE)
        
        hours <- rpois(n = n_user_sessions,
                       lambda = avg_hour[day_time])
        
        hours[hours>=22] <- 21
        hours[hours<7] <- 7
        
        mins <- sample(x = c(0:60),
                       size = n_user_sessions,
                       replace = TRUE)
        
        secs <- sample(x = c(0:60),
                       size = n_user_sessions,
                       replace = TRUE)
        
        hour(session_start_timestamp) <- hours
        minute(session_start_timestamp) <- mins
        second(session_start_timestamp) <- secs
         
        event_start_timestamp <- rep(session_start_timestamp, times = n_events[iter:(iter+n_user_sessions-1)])
        event_timestamp <- runif(n = length(event_start_timestamp),
                                  min = as.numeric(event_start_timestamp),
                                  max = as.numeric(event_start_timestamp + minutes(max_duration)))
        event_start_index <- cumsum(c(1,n_events[iter:(iter+n_user_sessions-1)]))
        event_timestamp <- round(event_timestamp,0)
        class(event_timestamp) <- c("POSIXct","POSIXt")
        event_timestamp[event_start_index[-length(event_start_index)]] <- session_start_timestamp
        
        content[event_iter:(event_iter+n_contents-1)] <- paste0('content_',content_num)
        timestamp[event_iter:(event_iter+n_contents-1)] <- event_timestamp
        contenttype[event_iter:(event_iter+n_contents-1)] <- all_subject_course_metadata[[sub]][[data$grade[user]]]$contenttype[content_num]
        content_area[event_iter:(event_iter+n_contents-1)] <- all_subject_course_metadata[[sub]][[data$grade[user]]]$content_area[content_num]
        state[event_iter:(event_iter+n_contents-1)] <- rep(data$state[user], times = n_contents)
        districtname[event_iter:(event_iter+n_contents-1)] <- rep(data$districtname[user], times = n_contents)
        schoolname[event_iter:(event_iter+n_contents-1)] <- rep(data$schoolname[user], times = n_contents)
        classname[event_iter:(event_iter+n_contents-1)] <- rep(data$classname[user], times = n_contents)
        userid[event_iter:(event_iter+n_contents-1)] <- rep(data$userid[user], times = n_contents)
        
        iter <- iter + n_user_sessions
        event_iter <- event_iter + n_contents
      }
      
      df <- tibble(sessionid = rep(paste0('session_',next_session:(next_session+tot_sessions-1)), times = n_events),
                   state = state,
                   districtname = districtname,
                   schoolname = schoolname,
                   subject = rep(sub, tot_events),
                   product_group = rep(paste(sub, 'Grades', product_group_grade,sep = ' '), times = tot_events),
                   product = rep(paste(sub, 'Grades', product_group_grade, 'Grade', gr, sep = ' '), times = tot_events),
                   product_id = rep(pr_id, times = tot_events),
                   classname = classname,
                   userroletype = rep(userroletype, times = tot_events),
                   userid = userid,
                   content = content,
                   contenttype = contenttype,
                   content_area = content_area,
                   timestamp = timestamp)
      
      element_name <- paste0(sub,'_',gr)
      event_data[[element_name]] <- df
      next_session <<- next_session+tot_sessions
      
    }
    
  }
  
  event_data
  
}

student_event_data <- create_event_data('Student') %>%
  bind_rows()

teacher_event_data <- create_event_data('Teacher') %>%
  bind_rows()


class(student_event_data$timestamp) <- c("POSIXct","POSIXt")
class(teacher_event_data$timestamp) <- c("POSIXct","POSIXt")

event_data_2 <- bind_rows(student_event_data, teacher_event_data) %>%
  mutate(eventtype = 'Attempted') %>%
  select(column_order) %>%
  mutate(grade = str_extract(string = product, pattern = "[[:alnum:]]+$"),
         grade_band = str_extract(string = product_group, pattern = "[[:alnum:]]-[[:digit:]]+$"),
         date = date(timestamp),
         date_binned_by_week = ceiling_date(date,unit = 'week'),
         date_month = floor_date(date, unit = 'month')) 

all_subject_course_metadata_2 <- lapply(all_subject_course_metadata, function(x){
  df <- x %>%
    bind_rows() %>%
    select(product, product_content, topic_content, content) %>%
    unite(col = 'title', product_content, topic_content, sep = ':')
  
  df
}) %>% bind_rows()

event_data_2 <- event_data_2 %>%
  left_join(y = all_subject_course_metadata_2, by = c('product','content'))

save(event_data_2, file = file.path(folder_path, 'raw_event_data.RData'))

rem <- ls()
rem <- rem[!(rem %in% c('event_data_2','item_response_data','folder_path','item_response_data_2', 'user_data',
                        'all_subject_assessment_item_data'))]
rm(list = c(rem,'rem'))


#-----ITEM RESPONSE TABLE PREP--------

student_assessment_event_data <- event_data_2 %>%
  filter(contenttype %in% c('Quiz','Test'), userroletype == 'Student') %>%
  select(product, product_id, subject, grade, userid, content, contenttype, timestamp) %>%
  left_join(y = user_data[['Student']] %>%
              select(userid, ability), by = 'userid')

n_quiz <- nrow(student_assessment_event_data %>%
                 filter(contenttype == 'Quiz'))

n_test <- nrow(student_assessment_event_data %>%
                 filter(contenttype == 'Test'))

tot_responses <- (5*n_quiz) + (10*n_test)

product <- rep(NA, times = tot_responses)
test_id <- rep(NA, times = tot_responses)
item_id <- rep(NA, times = tot_responses)
skill_id <- rep(NA, times = tot_responses)
score <- rep(NA, times = tot_responses)
timestamp <- rep(NA, times = tot_responses)


set.seed(30)
assess_items <- c('Quiz' = 5, 'Test' = 10)
max_assess_time <- c('Quiz' = 10, 'Test' = 20)

iter <- 1
create_item_response_data <- function(){
  for (assessment in seq(from = 1, to = nrow(student_assessment_event_data))) {
    
    print(assessment)
    
    assess_type <- student_assessment_event_data$contenttype[assessment]
    tid <- paste0(student_assessment_event_data$product_id[assessment],'_',student_assessment_event_data$content[assessment])
    item_skill_df <- all_subject_assessment_item_data[[student_assessment_event_data$subject[assessment]]][[student_assessment_event_data$grade[assessment]]] %>%
      filter(test_id %in% tid) %>%
      select(item_id, skill_id)
    student_ability <- student_assessment_event_data$ability[assessment]
    assess_score <- sample(x = c(1,0),
                           size = assess_items[assess_type],
                           prob = c(student_ability, 1 - student_ability),
                           replace = TRUE)
    start_timestamp <- student_assessment_event_data$timestamp[assessment]
    response_timestamp <- runif(n = assess_items[assess_type],
                                min = as.numeric(start_timestamp),
                                max = as.numeric(start_timestamp + minutes(max_assess_time[assess_type]))) %>%
      sort()
    
    product[iter:(iter+assess_items[assess_type]-1)] <- rep(student_assessment_event_data$product[assessment], times = assess_items[assess_type])
    test_id[iter:(iter+assess_items[assess_type]-1)] <- rep(tid, times = assess_items[assess_type])
    item_id[iter:(iter+assess_items[assess_type]-1)] <- item_skill_df$item_id
    skill_id[iter:(iter+assess_items[assess_type]-1)] <- item_skill_df$skill_id
    score[iter:(iter+assess_items[assess_type]-1)] <- assess_score
    timestamp[iter:(iter+assess_items[assess_type]-1)] <- response_timestamp
    
    iter <<- iter + assess_items[assess_type]
  }
  
  df <- tibble(
    product = product,
    test_id = test_id,
    item_id = item_id,
    skill_id = skill_id,
    score = score,
    timestamp = timestamp
  )
  
  df
}


item_response_data <- create_item_response_data()
class(item_response_data$timestamp) <- c("POSIXct","POSIXt")

userid <- rep(student_assessment_event_data$userid, times = assess_items[student_assessment_event_data$contenttype])

item_response_data <- item_response_data %>%
  bind_cols(tibble(userid = userid)) %>%
  select(product,test_id,userid,item_id,skill_id,score,timestamp)

save(item_response_data, file = file.path(folder_path, 'raw_item_response_data.RData'))


