output$ou_usage_type_filter_ui <- renderUI({
  req(input$ou_usage_type %in% c('Grade Band Usage', 'Product Group Usage'))
  
  if(input$ou_usage_type == 'Grade Band Usage'){
    radioButtons(inputId = 'ou_usage_type_gb_filter', label = 'Filter Grade Band by:',
                 choices = c('Subject','None'),
                 selected = 'None')
  }
  
  else if(input$ou_usage_type == 'Product Group Usage'){
    radioButtons(inputId = 'ou_usage_type_pg_filter', label = 'Filter Product Group by:',
                 choices = c('Subject','Grade Band','None'),
                 selected = 'None')
  }
  
})


output$ou_usage_type_filter_options_ui <- renderUI({
  req(input$ou_usage_type %in% c('Grade Band Usage', 'Product Group Usage'))
  
  if(input$ou_usage_type == 'Grade Band Usage'){
    req(input$ou_usage_type_gb_filter %in% c('Subject'))
    
    if(input$ou_usage_type_gb_filter == 'Subject'){
      selectInput(inputId = 'ou_usage_type_gb_filter_options_sub', label = 'Subject',
                  choices = subjects)
    }
    
  }
  
  else if(input$ou_usage_type == 'Product Group Usage'){
    req(input$ou_usage_type_pg_filter %in% c('Subject', 'Grade Band'))
    
    if(input$ou_usage_type_pg_filter == 'Subject'){
      selectInput(inputId = 'ou_usage_type_pg_filter_options_sub', label = 'Subject',
                  choices = subjects)
    }
    else if(input$ou_usage_type_pg_filter == 'Grade Band'){
      selectInput(inputId = 'ou_usage_type_pg_filter_options_gb', label = 'Grade Band',
                  choices = grade_bands)
    }
  }
  
})


output$ou_info_boxes_ui <- renderUI({
  
  fluidPage(
    fluidRow(infoBoxOutput("info_n_product_groups"), infoBoxOutput("info_n_districts"), infoBoxOutput("info_n_teachers")), 
    fluidRow(infoBoxOutput("info_n_products"), infoBoxOutput("info_n_schools"), infoBoxOutput("info_n_students"))
  )
  
})


output$ou_plot_ui <- renderUI({
  fluidPage(
    fluidRow(box(title = "Teacher", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_teacher_plot", height = "400px", width = "100%")),
             box(title = "Student", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_student_plot", height = "400px", width = "100%")))
  )
})

output$ou_plot_2_ui <- renderUI({
  fluidPage(
    fluidRow(box(title = "Teacher", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_aw_teacher_plot", height = "400px", width = "100%")),
             box(title = "Student", background = "blue", solidHeader = TRUE,
                 plotOutput("ou_aw_student_plot", height = "400px", width = "100%")))
  )
})


output$info_n_product_groups <- renderInfoBox({
  infoBox(format(as.numeric(ou_info_box_rdf$count[ou_info_box_rdf$variable == "n_product_groups"]), big.mark=","), "Number of Product Groups", 
          color = "teal", icon = icon("fas fa-book"), fill = TRUE)
})

output$info_n_products <- renderInfoBox({
  infoBox(format(as.numeric(ou_info_box_rdf$count[ou_info_box_rdf$variable == "n_products"]), big.mark=","), "Number of Products", 
          color = "teal", icon = icon("fas fa-book-open"), fill = TRUE)
})
output$info_n_districts <- renderInfoBox({
  infoBox(format(as.numeric(ou_info_box_rdf$count[ou_info_box_rdf$variable == "n_districts"]), big.mark=","), "Number of Districts", 
          color = "teal", icon = icon("fas fa-university"), fill = TRUE)
})
output$info_n_schools <- renderInfoBox({
  infoBox(format(as.numeric(ou_info_box_rdf$count[ou_info_box_rdf$variable == "n_schools"]), big.mark=","), "Number of Schools", 
          color = "teal", icon = icon("fas fa-school"), fill = TRUE)
})
output$info_n_teachers <- renderInfoBox({
  infoBox(format(as.numeric(ou_info_box_rdf$count[ou_info_box_rdf$variable == "n_teachers"]), big.mark=","), "Number of Teachers", 
          color = "teal", icon = icon("fas fa-chalkboard-teacher"), fill = TRUE)
})
output$info_n_students <- renderInfoBox({
  infoBox(format(as.numeric(ou_info_box_rdf$count[ou_info_box_rdf$variable == "n_students"]), big.mark=","), "Number of Students", 
          color = "teal", icon = icon("fas fa-users"), fill = TRUE)
})


ou_data <- reactive({
  req(!is.null(input$ou_usage_type), !is.null(input$ou_usage_metric))
  
  if(input$ou_usage_type == 'Subject Usage'){
    df <- ou_subject_wise_rdf %>%
      select(-tot_users,-tot_sessions) %>%
      mutate(summariser = subject) %>%
      select(subject, userroletype, n_users, perc_users, n_sessions, perc_sessions, summariser) %>%
      ungroup()
  }
  
  else if(input$ou_usage_type == 'Grade Band Usage'){

    req(!is.null(input$ou_usage_type_gb_filter))
    
    if(input$ou_usage_type_gb_filter == 'None'){
      df <- ou_grade_band_wise_filter_none_rdf %>%
        select(-tot_users,-tot_sessions) %>%
        mutate(summariser = grade_band) %>%
        select(grade_band, userroletype, n_users, perc_users, n_sessions, perc_sessions, summariser) %>%
        ungroup()
    }
    
    
    else if(input$ou_usage_type_gb_filter == 'Subject'){
      req(!is.null(input$ou_usage_type_gb_filter_options_sub))
      
      df <- ou_grade_band_wise_filter_subject_rdf %>%
        filter(subject == input$ou_usage_type_gb_filter_options_sub) %>%
        select(-tot_users,-tot_sessions) %>%
        mutate(summariser = grade_band) %>%
        select(subject, grade_band, userroletype, n_users, perc_users, n_sessions, perc_sessions, summariser) %>%
        ungroup()
        
    }
  }
  
  else if(input$ou_usage_type == 'Product Group Usage'){
 
    req(!is.null(input$ou_usage_type_pg_filter))
    
    if(input$ou_usage_type_pg_filter == 'None'){
      df <- ou_product_group_wise_filter_none_rdf %>%
        select(-tot_users,-tot_sessions) %>%
        mutate(summariser = product_group) %>%
        select(product_group, userroletype, n_users, perc_users, n_sessions, perc_sessions, summariser) %>%
        ungroup()
    }
    
    
    else if(input$ou_usage_type_pg_filter == 'Subject'){
      req(!is.null(input$ou_usage_type_pg_filter_options_sub))
      
      df <- ou_product_group_wise_filter_subject_rdf %>%
        filter(subject == input$ou_usage_type_pg_filter_options_sub) %>%
        select(-tot_users,-tot_sessions) %>%
        mutate(summariser = product_group) %>%
        select(subject, product_group, userroletype, n_users, perc_users, n_sessions, perc_sessions, summariser) %>%
        ungroup()
      
    }
    
    else if(input$ou_usage_type_pg_filter == 'Grade Band'){
      req(!is.null(input$ou_usage_type_pg_filter_options_gb))
      
      df <- ou_product_group_wise_filter_grade_band_rdf %>%
        filter(grade_band == input$ou_usage_type_pg_filter_options_gb) %>%
        select(-tot_users,-tot_sessions) %>%
        mutate(summariser = product_group) %>%
        select(grade_band, product_group, userroletype, n_users, perc_users, n_sessions, perc_sessions, summariser) %>%
        ungroup()
      
    }
    
  }
  
  if(input$ou_usage_metric == '# Users'){
    df_2 <- df %>% 
      mutate(filler = perc_users)
  }
  
  else if(input$ou_usage_metric == '# Sessions'){
    df_2 <- df %>%
      mutate(filler = perc_sessions)
  }
  
  df_3 <- df_2 %>%
    group_by(userroletype) %>%
    arrange(desc(filler)) %>%
    mutate(summariser = if_else(row_number()>4, 'Other', summariser)) %>%
    ungroup() %>%
    group_by(userroletype, summariser) %>%
    summarise(filler = sum(filler)) %>%
    arrange(desc(filler)) %>%
    ungroup()
  
  col_mappings <- c(subject = 'Subject', grade_band = 'Grade Band', product_group = 'Product Group',
                    userroletype = 'User Role', n_users = '# Users', n_sessions = '# Sessions',
                    perc_users = '% Users', perc_sessions = '% Sessions', summariser = 'summariser')
  
  new_col_names <- col_mappings[names(df)]
  names(df) <- new_col_names
  
  list(plot_data = df_3,
       table_data = df %>%
       select(-summariser) %>%
       arrange(desc(`# Users`)))
  
})

ou_aw_data <- reactive({
  req(!is.null(input$ou_active_weeks_filter))
  
  if(input$ou_active_weeks_filter == 'Total Active Weeks'){
    df <- ou_active_weeks_abs_rdf
  }
  
  else if(input$ou_active_weeks_filter == '% of Active Weeks'){
    df <- ou_active_weeks_perc_rdf
  }
  df
})

my_colors <- reactive({
  req(!is.null(input$ou_usage_type), !is.null(input$ou_usage_metric))
  
  all_summariser <- unique(ou_data()$plot_data$summariser)
  all_summariser_without_other <- all_summariser[all_summariser != 'Other']
  other_col <- 'grey90'
  pal <- c('#fa9191','#87f5ff','#59ffbd','#fffd8c','#ffc966','#ad7aff','#96a3ff','#99fff0')
  
  c(setNames(object = pal[1:length(all_summariser_without_other)], all_summariser_without_other), 'Other' = other_col)
  
})


output$ou_teacher_plot <- renderPlot({
  req(!is.null(input$ou_usage_type), !is.null(input$ou_usage_metric))
  
  overall_summary_pie_plot(ou_data()$plot_data, "Teacher", my_colors())
})

output$ou_student_plot <- renderPlot({
  req(!is.null(input$ou_usage_type), !is.null(input$ou_usage_metric))
  
  overall_summary_pie_plot(ou_data()$plot_data, "Student", my_colors())
})


output$ou_table <- DT::renderDataTable({
  
  req(!is.null(input$ou_usage_type), !is.null(input$ou_usage_metric))
  
  ncols <- ncol(ou_data()$table_data)
  columns <- colnames(ou_data()$table_data)[(ncols-3):ncols]
  break_col <- break_col_func(ou_data()$table_data, columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(ou_data()$table_data, filter = 'top', options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ))
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
  
})

output$ou_aw_teacher_plot <- renderPlot({
  req(!is.null(input$ou_active_weeks_filter))
  
  active_weeks_pie_plot(ou_aw_data(), "Teacher")
})

output$ou_aw_student_plot <- renderPlot({
  req(!is.null(input$ou_active_weeks_filter))
  
  active_weeks_pie_plot(ou_aw_data(), "Student")
})
