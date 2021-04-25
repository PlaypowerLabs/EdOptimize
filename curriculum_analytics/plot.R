#--------OVERALL USAGE TAB PLOTS-------

product_wise_vertical_plot <- function(df, color_pal, x_label){
  
  df %>%
    ggplot(aes(x = x_axis_col, y = product_labels)) +
    geom_bar(aes(fill = product), stat = 'identity') +
    facet_wrap(~userroletype, ncol = 2, scales = 'free_x') +
    scale_fill_manual('', values = color_pal) +
    scale_y_discrete('Product') +
    scale_x_continuous(x_label, labels = comma_format()) +
    theme(legend.position = 'none',
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
}

active_weeks_pie_chart <- function(df, user){
  
  df_2 <- df %>%
    filter(userroletype == user) %>% 
    mutate(summariser_2 = str_wrap(paste0(summariser, " (", filler, "%)"), width = 20))
  
  old.par <- par(no.readonly = TRUE)
  par(cex = 1, family = "sans", font.main = 4, mar = c(4, 6, 1, 8))
  pie(df_2$filler, border = NA, radius = 1, clockwise = FALSE, 
      col = df_2$color, labels = df_2$summariser_2, init.angle = 90)
  title(paste0(user, " Active Weeks"), line = -25)
  par(old.par)
  
}

#-----USAGE OVER TIME TAB PLOT--------

new_vs_returning_users_plot <- function(df, res, fix_y){
  
  date_labs <- c(Day = '%d-%b', Week = '%d-%b', Month = '%b-%y')
  axis_labels <- c(Day = 'Date', Week = 'Date(binned by week)', Month = 'Month')
  y_scales <- c(True = 'fixed', False = 'free_y')
  
  create_breaks <- function(){
    
    dates <- sort(unique(df$dateid))
    
    if(length(dates)<=8 || res == 'Month'){
      brks <- dates
    }
    
    else{
      brks <- dates[round(seq(1,length(dates), length.out = 8))]
    }
    
    brks
  }
  
  df %>%
    mutate(feature = factor(feature, levels = c('Returning User', 'New User', 'Leaving User', 'Bouncing User'))) %>%
    ggplot(aes(x = dateid, y = value)) +
    geom_bar(aes(fill = feature), stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = y_scales[fix_y]) +
    scale_fill_manual('', values = c('Bouncing User' = '#d60000', 'Leaving User' = '#fc7e7e', 'New User' = '#13bf00', 'Returning User' = '#8ef283')) +
    scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
    scale_y_continuous(name = 'No of Users',label = comma_format()) +
    theme(legend.position = 'top')
  
}

avg_session_duration_over_time_plot <- function(df, res, fix_y){
  
  date_labs <- c(Day = '%d-%b', Week = '%d-%b', Month = '%b-%y')
  axis_labels <- c(Day = 'Date', Week = 'Date(binned by week)', Month = 'Month')
  y_scales <- c(True = 'fixed', False = 'free_y')
  
  create_breaks <- function(){
    dates <- sort(unique(df$dateid))
    
    if(length(dates)<=8 || res == 'Month'){
      brks <- dates
    }
    
    else{
      brks <- dates[round(seq(1,length(dates), length.out = 8))]
    }
    
    brks
  }

  
  df %>%
    ggplot(aes(x = dateid, y = avg_session_duration)) +
    geom_bar(aes(alpha = perc_users), fill = '#3692c9', stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = y_scales[fix_y]) +
    scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
    scale_y_continuous(name = 'Avg Session Duration(min)') +
    scale_alpha_continuous(name = '% Users')
  
}

avg_events_per_user_over_time_plot <- function(df, res, fix_y){
  
  date_labs <- c(Day = '%d-%b', Week = '%d-%b', Month = '%b-%y')
  axis_labels <- c(Day = 'Date', Week = 'Date(binned by week)', Month = 'Month')
  y_scales <- c(True = 'fixed', False = 'free_y')
  
  create_breaks <- function(){
    dates <- sort(unique(df$dateid))
    
    if(length(dates)<=8 || res == 'Month'){
      brks <- dates
    }
    
    else{
      brks <- dates[round(seq(1,length(dates), length.out = 8))]
    }
    brks
  }
  
  df %>%
    ggplot(aes(x = dateid, y = avg_events_per_user)) +
    geom_bar(aes(alpha = perc_users), fill = '#ba2d2b', stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = y_scales[fix_y]) +
    scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
    scale_y_continuous(name = 'Avg Events per User') +
    scale_alpha_continuous(name = '% Users')
  
}

#------CONTENT USAGE TAB PLOTS-----

content_usage_summary_pie_plot <- function(df, user){
  
  df_2 <- df %>%
    filter(userroletype == user) %>% 
    mutate(summariser_2 = str_wrap(paste0(summariser, " (", filler, "%)"), width = 15))
  
  old.par <- par(no.readonly = TRUE)
  par(cex = 1, family = "sans", font.main = 4, mar = c(4, 6, 1, 6))
  pie(df_2$filler, border = NA, radius = 1, clockwise = FALSE, 
             col = df_2$color, labels = df_2$summariser_2, init.angle = 90)
  title(paste0(user, " Content Usage"), line = -25)
  par(old.par)
  
}

content_popularity_plot <- function(df){
  
  df %>%
    mutate(content_pop_labels_2 = str_wrap(content_pop_labels, width = 8) %>%
             factor(levels = str_wrap(c('0% to 10%','10% to 20%','20% to 30%','30% to 40%','40% to 50%',
                                        '50% to 60%','60% to 70%','70% to 80%','80% to 90%','90% to 100%'),width = 8))) %>%
    ggplot(aes(x = content_pop_labels_2, y = n_users)) +
    geom_bar(aes(fill = userroletype), stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = 'free_y') +
    scale_fill_manual('', values = c('Student' = '#7adceb','Teacher' = '#e68177')) +
    scale_x_discrete('% Contents Accessed') +
    scale_y_continuous('# Users', labels = label_comma()) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'top')
  
}

content_type_usage_plot <- function(df, color_pal, metric){
  
  y_label <- c('# of users' = '# Users','# events per user' = 'Avg Events per User','Unique content items' = '# Content Items')
  
  df %>%
    ggplot(aes(x = contenttype, y = y_axis_col)) +
    geom_bar(aes(fill = contenttype), stat = 'identity') +
    facet_wrap(~userroletype, scales = 'free_y', nrow = 1) +
    scale_fill_manual('',values = color_pal) +
    scale_y_continuous(y_label[metric], labels = label_comma()) +
    scale_x_discrete('Content Type') +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 30, vjust = 0.5),
          legend.position = 'none')
}

content_type_uot_tile_plot <- function(df, fill_label){
  
  create_breaks <- function(){
    dates <- sort(unique(df$date_binned_by_week))
    
    if(length(dates)<=8 || res == 'Month'){
      brks <- dates
    }
    
    else{
      brks <- dates[round(seq(1,length(dates), length.out = 8))]
    }
    brks
  }
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = contenttype)) +
    geom_tile(aes(fill = fill_col), color = 'white') +
    scale_fill_gradient(fill_label, low = '#dff2f7', high = '#289ab8', labels = label_comma()) +
    scale_x_date('Date(binned by week)', date_breaks = '6 weeks', date_labels = '%d-%b') +
    scale_y_discrete('Content Type') +
    theme(legend.key.height = unit(1, 'cm'))
}

#----ASSESSMENT ANALYSIS TAB PLOTS-----

assessment_score_distribution_plot <- function(df){
  
  df %>%
    ggplot(aes(x = perc_score_levels, y = n_users)) +
    geom_bar(stat = 'identity', fill = '#91dcff') +
    scale_x_discrete('% Score') +
    scale_y_continuous('# Students', labels = label_comma()) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(r = 5)))
  
  
}

#--------SKILL PERFORMANCE TAB PLOTS-----

skill_pacing_plot <- function(df, segment_df){
  
  mid_value <- mean(df$avg_score)
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = skill_id)) +
    geom_segment(data = segment_df, mapping = aes(x = start_week, y = skill_id, xend = end_week, yend = skill_id), linetype = 'dashed') +
    geom_point(aes(size = n_students, color = avg_score)) +
    scale_color_gradient2('Average Score', low = '#f22e2e', mid = '#fcf62d', high = '#2eff2e', midpoint = mid_value) +
    scale_size_continuous('# Students', range = c(2,6)) +
    scale_x_date('Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_discrete('Skill') +
    theme(axis.title.y = element_text(margin = margin(r = 5)),
          legend.spacing.y = unit(0.5,'cm'))
}

#------CURRICULUM PACING TAB PLOTS----

usage_heatmap_pacing_plot <- function(df,y_labels_df,userrole, fill_by_title){
  
  col_pal <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5',
               '#08519c','#08306b')
  
  df %>%
    filter(userroletype %in% userrole) %>%
    ggplot(aes(x = date_binned_by_week, y = title)) +
    geom_tile(aes(fill = fill_by), color = 'white', size = 0.4) +
    scale_fill_gradientn(fill_by_title, colors = col_pal) +
    scale_y_discrete('Curriculum', labels = y_labels_df$title_labels, limits = y_labels_df$title) +
    scale_x_date('Date(binned by week)', date_breaks = '1 month', date_labels = '%b-%y') +
    labs(
      subtitle = paste0('User Role: ', userrole)
    )
}


performance_heatmap_pacing_plot <- function(df, y_labels_df){
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = title)) +
    geom_tile(aes(fill = score_levels, alpha = n_users), color = 'white', size = 0.4) +
    scale_fill_manual('Avg Percent Score', values = c('0-35' = '#f22424', '36-70' = '#ffd70f', '71-100' = '#1bfa27')) +
    scale_alpha_continuous('# Students', breaks = integer_breaks(), range = c(0.25,1)) +
    scale_y_discrete('Curriculum', labels = y_labels_df$title_labels, limits = y_labels_df$title) +
    scale_x_date('Date(binned by week)', date_breaks = '1 month', date_labels = '%b-%y')
}

performance_dotchart_pacing_plot <- function(df, y_labels_df){
  
  assess_df <- df %>%
    filter(!is.na(score_levels))
  
  non_assess_df <- df %>%
    filter(is.na(score_levels))
  
  non_assess_df %>%
    ggplot(aes(x = date_binned_by_week, y = content)) +
    geom_jitter(aes(size = n_users), alpha = 0.25, color = '#525252') +
    geom_jitter(data = assess_df, aes(color = score_levels, size = n_users), alpha = 0.7) +
    scale_color_manual('Avg Percent Score', values = c('0-35' = '#f22424', '36-70' = '#ffd70f', '71-100' = '#1bfa27')) +
    scale_size_continuous('# Students', breaks = integer_breaks(), labels = label_comma(), range = c(2,6)) +
    scale_y_discrete('Curriculum', labels = y_labels_df$content_labels, limits = y_labels_df$content) +
    scale_x_date('Date(binned by week)', date_breaks = '1 month', date_labels = '%b-%y') +
    theme(axis.ticks.y = element_blank(),
          panel.grid = element_blank())
  
}


#------LEADERBOARD TAB PLOTS-----

district_new_vs_returning_users_plot <- function(df){
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = value)) +
    geom_bar(aes(fill = feature), stat = 'identity') +
    scale_fill_manual('', values = c('New User' = '#4aa8a8', 'Returning User' = '#67e0e0')) +
    scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_continuous(name = 'No of Users',label = comma_format()) +
    theme(legend.position = 'right',
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          plot.title = element_text(size = 14))
}


district_events_per_user_over_time_plot <- function(df){
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = n_events_per_user)) +
    geom_bar(aes(alpha = n_users), fill = '#67e0e0', stat = 'identity') +
    scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_continuous(name = 'Avg Events per User') +
    scale_alpha_continuous(name = '# Users') +
    theme(legend.position = 'right',
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          plot.title = element_text(size = 14))
}
