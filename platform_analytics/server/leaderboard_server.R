
dl_scale_cols <- names(dl_overall_rdf)[-1]

dl_scaled_score_df <- map(dl_scale_cols, function(x){
  scale(dl_overall_rdf[[x]])
}) %>%
  bind_cols()

leaderboard_data <- dl_overall_rdf %>%
  mutate(scaled_score_sum = rowSums(dl_scaled_score_df)) %>%
  arrange(desc(scaled_score_sum)) %>%
  mutate(rank = row_number()) %>%
  select('Rank' = rank, 'District Name' = districtname, 'Avg Active Days per User' = avg_active_days,
         'Avg Contents per User' = avg_contents, '# Students' = n_students, '# Teachers' = n_teachers,
         'Avg Student Events' = avg_student_events, 'Avg Teacher Events' = avg_teacher_events)



options(DT.options = list(pageLength = 15))

output$dl_leaderboard <- renderDataTable({
  
  # cols <- names(dl_overall_rdf)[-1]
  # 
  # scaled_score_df <- map(cols, function(x){
  #   scale(dl_overall_rdf[[x]])
  # }) %>%
  #   bind_cols()
  # 
  # df_2 <- dl_overall_rdf %>%
  #   mutate(scaled_score_sum = rowSums(scaled_score_df)) %>%
  #   arrange(desc(scaled_score_sum)) %>%
  #   mutate(rank = row_number()) %>%
  #   select('Rank' = rank, 'District Name' = districtname, 'Avg Active Days per User' = avg_active_days,
  #          'Avg Contents per User' = avg_contents, '# Students' = n_students, '# Teachers' = n_teachers,
  #          'Avg Student Events' = avg_student_events, 'Avg Teacher Events' = avg_teacher_events)
  # 
  table_width <- input$dimension[1]*.9
  general_col_width <- table_width/ncol(leaderboard_data)
  
  DT::datatable(leaderboard_data, filter = 'none', class = 'cell-border stripe', 
                options(list(scrollX = TRUE, sDom  = '<"top">lrt<"bottom">ip',
                             columnDefs = list(list(className = 'dt-center', targets = 0)))), rownames= FALSE,
                selection = 'single', escape = FALSE) %>% 
    formatStyle(columns = 1, width = general_col_width/10) %>%
    formatStyle(columns = 2, width = general_col_width) %>%
    formatStyle(columns = c(3:8), width = general_col_width)
  
}, server = TRUE)

output$dl_plot_ui <- renderUI({
  
  s <- input$dl_leaderboard_rows_selected
  
  if(length(s)){
    fluidPage(
      h4('District Explorer'),
      hr(style="border-top: 1px solid #000000;"),
      tabsetPanel(id = 'dl_tab_panel',
                  tabPanel(
                    title = 'New vs Returning Users',
                    value = 'nvr',
                    plotOutput(outputId = 'dl_nvr_plot', height = input$dimension[2] * .35) %>%
                      withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
                  ),
                  tabPanel(
                    title = 'Events per User Over Time',
                    value = 'eot',
                    plotOutput(outputId = 'dl_eot_plot', height = input$dimension[2] * .35) %>%
                      withSpinner(type = 3, color = '#2C3E50', color.background = '#ffffff')
                  ))
    )
  }
})


output$dl_nvr_plot <- renderPlot({
  
  s <- input$dl_leaderboard_rows_selected
  
  if(length(s)){
    district <- leaderboard_data[['District Name']][s]
    
    dl_nvr_overall_rdf %>%
      filter(districtname %in% district) %>%
      pivot_longer(cols = c(new, returning), names_to = 'feature', values_to = 'value') %>%
      mutate(feature = factor(feature, levels = c('returning','new'), labels = c('Returning User', 'New User'))) %>%
      ggplot(aes(x = date_binned_by_week, y = value)) +
      geom_bar(aes(fill = feature), stat = 'identity') +
      scale_fill_manual('', values = c('New User' = '#4aa8a8', 'Returning User' = '#67e0e0')) +
      scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
      scale_y_continuous(name = 'No of Users',label = comma_format()) +
      theme(legend.position = 'right',
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(margin = margin(r = 20)))
  }
  
})

output$dl_eot_plot <- renderPlot({
  
  s <- input$dl_leaderboard_rows_selected
  
  if(length(s)){
    district <- leaderboard_data[['District Name']][s]
    
    dl_eot_overall_rdf %>%
      filter(districtname %in% district) %>%
      ggplot(aes(x = date_binned_by_week, y = n_events_per_user)) +
      geom_bar(aes(alpha = n_users), fill = '#67e0e0', stat = 'identity') +
      scale_x_date(name = 'Date(binned by week)', date_breaks = '4 weeks', date_labels = '%d-%b') +
      scale_y_continuous(name = 'Avg Events per User') +
      scale_alpha_continuous(name = '# Users') +
      theme(legend.position = 'right',
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(margin = margin(r = 20)))
  }
})
