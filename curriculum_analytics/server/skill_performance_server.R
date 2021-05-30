#------UI ELEMENTS------
#----select product component-----
output$sp_sel_product_ui <- renderUI({
  req(!is.null(input$ou_select_pg))
  
  x <- product_group_products_rdf %>%
    filter(product_group %in% input$ou_select_pg) %>%
    .$product
  
  selectInput(inputId = 'sp_sel_product', label = 'Select Product:', choices = x)
})

#-----select skills slider-----
output$sp_skills_slider_ui <- renderUI({
  req(!is.null(input$ou_select_pg), !is.null(input$sp_sel_product))
  
  x <- sp_product_max_skills_rdf %>%
    filter(product %in% input$sp_sel_product) %>%
    .$max_skills
  
  sliderInput(inputId = 'sp_n_skills', label = 'Number of Skills:', min = 10, max = x, value = x, step = 1)
})

#-----infobox ui--

output$sp_infobox_ui <- renderUI({
  req(!is.null(input$sp_sel_product))
  
  fluidPage(
    fluidRow(
      infoBoxOutput('sp_n_skills', width = 6),
      infoBoxOutput('sp_avg_items_per_skill', width = 6)
    ),
    fluidRow(
      infoBoxOutput('sp_avg_students_per_skill', width = 6),
      infoBoxOutput('sp_avg_skills_per_student', width = 6)
    )
  )
})

#-----REACTIVE COMPONENTS-----
#-----skill pacing plot data prep----

sp_skill_pacing_data <- reactive({
  req(!is.null(input$sp_sel_product), !is.null(input$sp_n_skills), !is.null(input$ou_select_year))
  
  
  most_used_skills <- sp_skills_table_rdf %>%
    filter(product %in% input$sp_sel_product) %>%
    select(skill_id, n_students) %>%
    arrange(desc(n_students)) %>%
    .$skill_id %>% 
    .[1:input$sp_n_skills]
  
  skills_order <- sp_skill_start_end_week_rdf %>%
    filter(product %in% input$sp_sel_product, skill_id %in% most_used_skills) %>%
    arrange(start_week) %>%
    .$skill_id
  
  df <- sp_skill_pacing_data_rdf %>%
    filter(product %in% input$sp_sel_product, skill_id %in% most_used_skills) %>%
    mutate(skill_id = factor(skill_id, levels = skills_order)) %>%
    select(-product)
  
  df_2 <- sp_skill_start_end_week_rdf %>%
    filter(product %in% input$sp_sel_product, skill_id %in% most_used_skills) %>%
    mutate(skill_id = factor(skill_id, levels = skills_order)) %>%
    select(-product)
  
  label <- labs(
    title = paste0('Product: ', input$sp_sel_product),
    subtitle = input$ou_select_year
  )
  
  list(main_data = df, segment_data = df_2, label = label)
  
})


#skill performace table

sp_table_data <- reactive({
  req(!is.null(input$sp_sel_product))
  
  df <- sp_skills_table_rdf %>%
    filter(product %in% input$sp_sel_product) %>%
    rename('Product' = product, 'Skill ID' = skill_id, '# Item Responses' = n_responses,
           '# Items' = n_items, '# Students' = n_students, 'Avg Score' = avg_score,
           'SD Score' = sd_score)
  
  df
})

#--------OUTPUT ELEMENTS-------
#-----infobox output-----

output$sp_n_skills <- renderInfoBox({
  req(!is.null(input$sp_sel_product))
  
  infoBoxWithoutIcon(title = format(as.numeric(sp_infobox_rdf$max_skills[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                     subtitle = 'Number of Skills',
                     color = 'light-blue',
                     width = 6,
                     fill = TRUE)
})


output$sp_avg_items_per_skill <- renderInfoBox({
  req(!is.null(input$sp_sel_product))
  
  infoBoxWithoutIcon(title = paste0(format(as.numeric(sp_infobox_rdf$avg_items_per_skill[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                    ' (SD = ',
                                    format(as.numeric(sp_infobox_rdf$sd_items_per_skill[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                    ')'),
                     subtitle = paste0('Avg Items per Skill (Median = ',
                                   format(as.numeric(sp_infobox_rdf$med_items_per_skill[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                   ')'),
                     color = 'light-blue',
                     width = 6,
                     fill = TRUE)
})

output$sp_avg_students_per_skill <- renderInfoBox({
  req(!is.null(input$sp_sel_product))
  
  infoBoxWithoutIcon(title = paste0(format(as.numeric(sp_infobox_rdf$avg_students_per_skill[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                    ' (SD = ',
                                    format(as.numeric(sp_infobox_rdf$sd_students_per_skill[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                    ')'),
                     subtitle = paste0('Avg Students per Skill (Median = ',
                                    format(as.numeric(sp_infobox_rdf$med_students_per_skill[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                    ')'),
                     color = 'light-blue',
                     width = 6,
                     fill = TRUE)
})

output$sp_avg_skills_per_student <- renderInfoBox({
  req(!is.null(input$sp_sel_product))
  
  infoBoxWithoutIcon(title = paste0(format(as.numeric(sp_infobox_rdf$avg_skills_per_student[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                    ' (SD = ',
                                    format(as.numeric(sp_infobox_rdf$sd_skills_per_student[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                    ')'),
                     subtitle = paste0('Avg Skills per Student (Median = ',
                                    format(as.numeric(sp_infobox_rdf$med_skills_per_student[sp_infobox_rdf$product == input$sp_sel_product]), big.mark=","),
                                    ')'),
                     color = 'light-blue',
                     width = 6,
                     fill = TRUE)
})

#----skill pacing plot----

output$sp_pacing_plot <- renderPlot({
  req(!is.null(input$sp_sel_product), !is.null(input$sp_n_skills))
  
  skill_pacing_plot(sp_skill_pacing_data()$main_data, sp_skill_pacing_data()$segment_data) +
    sp_skill_pacing_data()$label
})


#-----skill summary table-----
output$sp_table <- DT::renderDataTable({
  req(!is.null(input$sp_sel_product))
  
  ncols <- ncol(sp_table_data())
  columns <- colnames(sp_table_data())[(ncols-4):ncols]
  break_col <- break_col_func(sp_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(sp_table_data(), filter = 'top', options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ), class = 'cell-border stripe')
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
})

