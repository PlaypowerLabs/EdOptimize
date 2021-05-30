#------OVERALL USAGE TAB PLOTS------- 

overall_summary_pie_plot <- function(df, user, col_pal){
  
  df_2 <- df %>%
    filter(userroletype == user) %>% 
    mutate(summariser_2 = str_wrap(paste0(summariser, " (", filler, "%)"), width = 15))
  
  old.par <- par(no.readonly = TRUE)
  par(cex = 1.05, family = "sans", font.main = 4, mar = c(4, 6, 1, 6))
  pie(df_2$filler, border = NA, radius = 1, clockwise = TRUE, 
      col = col_pal[df_2$summariser], labels = df_2$summariser_2)
  title(paste0(user, " Usage"), line = -24)
  par(old.par)
  
}

active_weeks_pie_plot <- function(df, user){
  
  df_2 <- df %>%
    filter(userroletype == user) %>% 
    mutate(summariser_2 = str_wrap(paste0(act_weeks_label, " (", perc_users, "%)"), width = 15))
  
  old.par <- par(no.readonly = TRUE)
  par(cex = 1.05, family = "sans", font.main = 4, mar = c(4, 6, 1, 6))
  pie(df_2$perc_users, border = NA, radius = 1, clockwise = FALSE, 
      col = df_2$color, labels = df_2$summariser_2, init.angle = 90)
  title(paste0(user, " Active Weeks"), line = -24)
  par(old.par)
  
}

#-------USAGE OVER TIME TAB PLOTS------
usage_over_time_plot <- function(df, res, facet, fix_y){
  
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
  
  if(facet == 'True'){
    df %>%
      mutate(feature = factor(feature, levels = c('Returning User', 'New User', 'Leaving User', 'Bouncing User'))) %>%
      ggplot(aes(x = dateid, y = value)) +
      geom_bar(aes(fill = feature), stat = 'identity') +
      facet_wrap(~facet_col, ncol = 2, scales = y_scales[fix_y]) +
      scale_fill_manual('', values = c('Bouncing User' = '#d60000', 'Leaving User' = '#fc7e7e', 'New User' = '#13bf00', 'Returning User' = '#8ef283')) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'No of Users',label = comma_format()) +
      theme(legend.position = 'top',
            axis.text.x = element_text(angle = 45, vjust = 0.5))
  }
  else{
    df %>%
      mutate(feature = factor(feature, levels = c('Returning User', 'New User', 'Leaving User', 'Bouncing User'))) %>%
      ggplot(aes(x = dateid, y = value)) +
      geom_bar(aes(fill = feature), stat = 'identity') +
      facet_wrap(~facet_col, ncol = 1, scales = y_scales[fix_y]) +
      scale_fill_manual('', values = c('Bouncing User' = '#d60000', 'Leaving User' = '#fc7e7e', 'New User' = '#13bf00', 'Returning User' = '#8ef283')) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'No of Users',label = comma_format()) +
      theme(legend.position = 'top')
  }
}


avg_session_duration_over_time_plot <- function(df, res, facet, fix_y){
  
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
  
  if(facet == 'True'){
    df %>%
      ggplot(aes(x = dateid, y = avg_duration)) +
      geom_bar(aes(alpha = perc_users), fill = '#3692c9', stat = 'identity') +
      facet_wrap(~facet_col, ncol = 2, scales = y_scales[fix_y]) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'Avg Session Duration(min)') +
      scale_alpha_continuous(name = '% Users') +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  }
  else{
    df %>%
      ggplot(aes(x = dateid, y = avg_duration)) +
      geom_bar(aes(alpha = perc_users), fill = '#3692c9', stat = 'identity') +
      facet_wrap(~facet_col, ncol = 1, scales = y_scales[fix_y]) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'Avg Session Duration(min)') +
      scale_alpha_continuous(name = '% Users')
  }
  
}


avg_events_per_user_over_time_plot <- function(df, res, facet, fix_y){
  
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
  
  if(facet == 'True'){
    df %>%
      ggplot(aes(x = dateid, y = n_events_per_user)) +
      geom_bar(aes(alpha = perc_users), fill = '#ba2d2b', stat = 'identity') +
      facet_wrap(~facet_col, ncol = 2, scales = y_scales[fix_y]) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'Avg Events per User') +
      scale_alpha_continuous(name = '% Users') +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  }
  else{
    df %>%
      ggplot(aes(x = dateid, y = n_events_per_user)) +
      geom_bar(aes(alpha = perc_users), fill = '#ba2d2b', stat = 'identity') +
      facet_wrap(~facet_col, ncol = 1, scales = y_scales[fix_y]) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'Avg Events per User') +
      scale_alpha_continuous(name = '% Users')
  }
  
}

#-----CURRICULUM INSIGHTS TAB PLOTS-----

district_product_new_vs_returning_users_plot <- function(df){
  
  df %>%
    mutate(feature = factor(feature, levels = c('Returning User', 'New User', 'Leaving User', 'Bouncing User'))) %>%
    ggplot(aes(x = date_binned_by_week, y = value)) +
    geom_bar(aes(fill = feature), stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = 'free_y') +
    scale_fill_manual('', values = c('Bouncing User' = '#d60000', 'Leaving User' = '#fc7e7e', 'New User' = '#13bf00', 'Returning User' = '#8ef283')) +
    scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_continuous(name = '# Users', breaks =  integer_breaks(), label = label_number(accuracy = 1, big.mark = ',')) +
    theme(legend.position = 'top')
}


district_product_avg_session_duration_over_time_plot <- function(df){
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = avg_duration)) +
    geom_bar(aes(alpha = perc_users), fill = '#3692c9', stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = 'free_y') +
    scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_continuous(name = 'Avg Session Duration(min)') +
    scale_alpha_continuous(name = '% Users')
  
}

district_product_avg_events_per_user_over_time_plot <- function(df){
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = n_events_per_user)) +
    geom_bar(aes(alpha = perc_users), fill = '#3692c9', stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = 'free_y') +
    scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_continuous(name = 'Avg Events per User', breaks =  integer_breaks(), label = label_number(accuracy = 1, big.mark = ',')) +
    scale_alpha_continuous(name = '% Users')
  
}

usage_heatmap_pacing_plot <- function(df, y_labels_df, product, userrole, fill_by_title, sch_year){
  
  col_pal <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5',
               '#08519c','#08306b')
  
  df %>%
    filter(userroletype %in% userrole) %>%
    ggplot(aes(x = date_binned_by_week, y = title)) +
    geom_tile(aes(fill = fill_by), color = 'white', size = 0.4) +
    scale_fill_gradientn(fill_by_title, colors = col_pal, breaks =  integer_breaks(), label = label_number(accuracy = 1, big.mark = ',')) +
    scale_y_discrete('Curriculum', labels = y_labels_df$title_labels, limits = y_labels_df$title) +
    scale_x_date('Date(binned by week)', date_breaks = '1 month', date_labels = '%b-%y') +
    labs(
      subtitle = paste0('Product: ', product,'\n',
                        'User Role: ', userrole, '\n',
                        sch_year)
    )
}

performance_heatmap_pacing_plot <- function(df, y_labels_df, product, sch_year){
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = title)) +
    geom_tile(aes(fill = score_levels, alpha = n_users), color = 'white', size = 0.4) +
    scale_fill_manual('Avg Percent Score', values = c('0-35' = '#f22424', '36-70' = '#ffd70f', '71-100' = '#1bfa27')) +
    scale_alpha_continuous('# Students', breaks = integer_breaks(), label = label_number(accuracy = 1, big.mark = ','), range = c(0.25,1)) +
    scale_y_discrete('Curriculum', labels = y_labels_df$title_labels, limits = y_labels_df$title) +
    scale_x_date('Date(binned by week)', date_breaks = '1 month', date_labels = '%b-%y') +
    labs(
      subtitle = paste0('Product: ', product,'\n',
                        sch_year)
    )
}

performance_dotchart_pacing_plot <- function(df, y_labels_df, product, sch_year){
  
  assess_df <- df %>%
    filter(!is.na(score_levels))
  
  non_assess_df <- df %>%
    filter(is.na(score_levels))
  
  non_assess_df %>%
    ggplot(aes(x = date_binned_by_week, y = content)) +
    geom_jitter(aes(size = n_users), alpha = 0.25, color = '#525252') +
    geom_jitter(data = assess_df, aes(color = score_levels, size = n_users), alpha = 0.7) +
    scale_color_manual('Avg Percent Score', values = c('0-35' = '#f22424', '36-70' = '#ffd70f', '71-100' = '#1bfa27')) +
    scale_size_continuous('# Students', breaks = integer_breaks(), labels = label_number(accuracy = 1, big.mark = ','), range = c(2,6)) +
    scale_y_discrete('Curriculum', labels = y_labels_df$content_labels, limits = y_labels_df$content) +
    scale_x_date('Date(binned by week)', date_breaks = '1 month', date_labels = '%b-%y') +
    theme(axis.ticks.y = element_blank(),
          panel.grid = element_blank()) +
    labs(
      subtitle = paste0('Product: ', product,'\n',
                        sch_year)
    )
  
}



#------LEADERBOARD TAB PLOTS-----

district_new_vs_returning_users_plot <- function(df){
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = value)) +
    geom_bar(aes(fill = feature), stat = 'identity') +
    scale_fill_manual('', values = c('New User' = '#4aa8a8', 'Returning User' = '#67e0e0')) +
    scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_continuous(name = '# Users', breaks =  integer_breaks(), label = label_number(accuracy = 1, big.mark = ',')) +
    theme(legend.position = 'right',
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          plot.title = element_text(size = 14),
          axis.text.x = element_text(size = 10))
}


district_events_per_user_over_time_plot <- function(df){
  
  df %>%
    ggplot(aes(x = date_binned_by_week, y = n_events_per_user)) +
    geom_bar(aes(alpha = n_users), fill = '#67e0e0', stat = 'identity') +
    scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_continuous(name = 'Avg Events per User', breaks = integer_breaks(), label = label_number(accuracy = 1, big.mark = ',')) +
    scale_alpha_continuous(name = '# Users', breaks = integer_breaks(), label = label_number(accuracy = 1, big.mark = ',')) +
    theme(legend.position = 'right',
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 10)),
          plot.title = element_text(size = 14),
          axis.text.x = element_text(size = 10))
}


