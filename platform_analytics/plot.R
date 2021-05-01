#------OVERALL USAGE TAB PLOTS------- 
overall_summary_pie_plot <- function(df, user, col_pal){
  
  df_2 <- df %>%
    filter(userroletype == user) %>% 
    mutate(summariser_2 = str_wrap(paste0(summariser, " (", filler, "%)"), width = 15))
  
  old.par <- par(no.readonly = TRUE)
  par(cex = 1.2, family = "sans", font.main = 4, mar = c(4, 6, 1, 6))
  pie(df_2$filler, border = NA, radius = 1, clockwise = TRUE, 
      col = col_pal[df_2$summariser], labels = df_2$summariser_2)
  title(paste0(user, " Usage"), line = -27)
  par(old.par)
  
}

active_weeks_pie_plot <- function(df, user){
  
  df_2 <- df %>%
    filter(userroletype == user) %>% 
    mutate(summariser_2 = str_wrap(paste0(summariser, " (", filler, "%)"), width = 15),
           summariser_2 = if_else(row_number()%%2==1,
                                  summariser_2,
                                  paste0('\n','\n',summariser_2)))
  
  old.par <- par(no.readonly = TRUE)
  par(cex = 1, family = "sans", font.main = 4, mar = c(4, 6, 1, 6))
  pie(df_2$filler, border = NA, radius = 1, clockwise = FALSE, 
      col = df_2$color, labels = df_2$summariser_2, init.angle = 90)
  title(paste0(user, " Active Weeks"), line = -35)
  par(old.par)
  
}

#--------USAGE OVER TIME TAB PLOT----------
usage_over_time_plot <- function(df, res, facet, userrole, fix_y){
  
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
      filter(userroletype %in% userrole) %>%
      mutate(feature = factor(feature, levels = c('Returning User', 'New User', 'Leaving User', 'Bouncing User'))) %>%
      ggplot(aes(x = dateid, y = value)) +
      geom_bar(aes(fill = feature), stat = 'identity') +
      facet_wrap(~facet_col, ncol = 2, scales = unname(y_scales[fix_y])) +
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
      facet_wrap(~userroletype, ncol = 1, scales = unname(y_scales[fix_y])) +
      scale_fill_manual('', values = c('Bouncing User' = '#d60000', 'Leaving User' = '#fc7e7e', 'New User' = '#13bf00', 'Returning User' = '#8ef283')) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'No of Users',label = comma_format()) +
      theme(legend.position = 'top')
  }
}

avg_session_duration_over_time_plot <- function(df, res, facet, userrole, fix_y){
  
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
      filter(userroletype %in% userrole) %>%
      ggplot(aes(x = dateid, y = avg_duration)) +
      geom_bar(aes(alpha = perc_sessions), fill = '#3692c9', stat = 'identity') +
      facet_wrap(~facet_col, ncol = 2, scales = y_scales[fix_y]) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'Avg Session Duration(min)') +
      scale_alpha_continuous(name = '% Sessions') +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  }
  else{
    df %>%
      ggplot(aes(x = dateid, y = avg_duration)) +
      geom_bar(aes(alpha = perc_sessions), fill = '#3692c9', stat = 'identity') +
      facet_wrap(~userroletype, ncol = 1, scales = y_scales[fix_y]) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'Avg Session Duration(min)') +
      scale_alpha_continuous(name = '% Sessions')
  }
  
}


avg_events_per_user_over_time_plot <- function(df, res, facet, userrole, fix_y){
  
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
      filter(userroletype %in% userrole) %>%
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
      facet_wrap(~userroletype, ncol = 1, scales = y_scales[fix_y]) +
      scale_x_date(name = axis_labels[res], breaks = create_breaks(), date_labels = date_labs[res]) +
      scale_y_continuous(name = 'Avg Events per User') +
      scale_alpha_continuous(name = '% Users')
  }
  
}

#----------USAGE BY PRODUCTS TAB PLOTS-------


product_wise_users_plot <- function(df,color_pal, pg){

  df %>%
    ggplot(aes(x = product_labels, y = n_users)) +
    geom_bar(aes(fill = product), stat = 'identity') +
    facet_wrap(~userroletype, nrow = 1, scales = 'free_x') +
    scale_fill_manual('',values = color_pal) +
    scale_y_continuous('# Users', labels = comma_format()) +
    scale_x_discrete('Product') +
    theme(legend.position = 'none',
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    coord_flip()
}



product_group_usage_over_time_plot <- function(df){

  df %>%
    mutate(usertype = factor(usertype, levels = c('Returning Student', 'Returning Teacher', 'New Student', 'New Teacher'))) %>%
    ggplot(aes(x = dateid, y = value)) +
    geom_bar(aes(fill = usertype), stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = 'free_y') +
    scale_fill_manual('', values = c('Returning Student' = '#baf5ff', 'New Student' = '#00daff', 'Returning Teacher' = '#e2fcae', 'New Teacher' = '#aeff0d')) +
    scale_x_date(name = 'Date(binned by Week)',date_breaks = '4 weeks', date_labels = '%d-%b') +
    scale_y_continuous(name = 'No of Users',label = comma_format()) +
    theme(legend.position = 'top')
  
  
}

user_behav_plot <- function(df, metric){
  
  x_axis_labels <- c('# Active Days' = 'Active Days', '# Active Weeks' = 'Active Weeks', 'Session Time' = 'Session Time (min)')
  y_axis_labels <- c('# Active Days' = '# Users', '# Active Weeks' = '# Users', 'Session Time' = '# Sessions')
  
  df %>%
    ggplot(aes(x = x_axis_col, y = y_axis_col)) +
    geom_bar(aes(fill = userroletype), stat = 'identity') +
    facet_wrap(~userroletype, ncol = 1, scales = 'free_y') +
    scale_fill_manual('', values = c('Student' = '#ffff96', 'Teacher' = '#93cc21')) +
    scale_y_continuous(y_axis_labels[metric], labels = label_comma()) +
    scale_x_continuous(x_axis_labels[metric], breaks = integer_breaks(10)) +
    theme(legend.position = 'none')
}


product_comparison_plot <- function(df,pg,sch_yr,color_pal){
  
  df %>%
    ggplot(aes(x = product_labels, y = value)) +
    geom_bar(aes(fill = product), stat = 'identity') +
    facet_wrap(~Feature, ncol = 3, scales = 'free_x') +
    scale_fill_manual('',values = color_pal) +
    scale_y_continuous('', breaks = integer_breaks(4), labels = label_comma()) +
    scale_x_discrete('Product') +
    coord_flip() +
    theme(legend.position = 'none',
          axis.title.y = element_text(margin = margin(r = 20)),
          axis.text.x = element_text(angle = 30, vjust = 0.5))
}

content_usage_plot <- function(df,cols){

  df %>%
    ggplot(aes(x = content_area_labels, y = n_users)) +
    geom_bar(aes(fill = userroletype), stat = 'identity') +
    facet_wrap(~userroletype, scales = 'free_y', ncol = cols) +
    scale_fill_manual('',values = c('Student' = '#ffc859', 'Teacher' = '#8aff73')) +
    scale_y_continuous('# Users', labels = label_comma()) +
    scale_x_discrete('Content Area') +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 45, vjust = 0.5))
}

#-------------USAGE BY CONTENT TYPE TAB PLOTS------------

content_type_usage_bar_chart <- function(df, color_pal){
  
  df %>%
    mutate(contenttype = factor(contenttype,
                                levels = c('Video', 'Lesson', 'Quiz', 'Audio', 'Interactive Media', 'Document', 'Test'))) %>%
    ggplot(aes(x = contenttype, y = perc_users)) +
    geom_bar(aes(fill = contenttype), stat = 'identity') +
    facet_wrap(~userroletype, scales = 'free_y', ncol = 1) +
    scale_fill_manual('',values = color_pal) +
    scale_y_continuous('% Users') +
    scale_x_discrete('Content Type') +
    theme(legend.position = 'none',
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
}

content_type_usage_tile_plot <- function(df, fill_col, y_col){
  
  df %>%
    mutate(contenttype = factor(contenttype,
                                levels = c('Video', 'Lesson', 'Quiz', 'Audio', 'Interactive Media', 'Document', 'Test'))) %>%
    ggplot(aes(x = contenttype, y = y_axis_col)) +
    geom_tile(aes(fill = value), color = 'white') +
    scale_fill_gradient(fill_col, low = '#f7fbff', high = '#08306b', label = comma) +
    scale_x_discrete('Content Type',position = 'top') +
    scale_y_discrete(y_col) +
    theme(axis.text.x = element_text(angle = 50, hjust = 0), legend.key.height = unit(1, 'cm'))
}



