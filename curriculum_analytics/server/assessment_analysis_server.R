#------UI ELEMENTS------
#----select product component-----
output$aa_sel_product_ui <- renderUI({
  req(!is.null(input$ou_select_pg))
  
  x <- product_group_products_rdf %>%
    filter(product_group %in% input$ou_select_pg) %>%
    .$product
  
  selectInput(inputId = 'aa_sel_product', label = 'Select Product:', choices = x)
})

#-----select assessment component---
output$aa_sel_assess_ui <- renderUI({
  req(!is.null(input$aa_sel_product))
  
  x <- aa_product_assess_data %>%
    filter(product %in% input$aa_sel_product) %>%
    .$test_id
  
  s <- input$aa_assess_table_rows_selected
  
  if(length(s)){
    req(!is.null(aa_assess_table_data()))
    
    sel <- aa_assess_table_data()$`Assessment ID`[s]
     aa_assess_table_proxy %>%
       DT::selectRows(selected = NULL)
    selectInput(inputId = 'aa_sel_assess', label = 'Select Assessment:',
                      choices = x, selected = sel)
  }
  
  else{
    selectInput(inputId = 'aa_sel_assess', label = 'Select Assessment:', choices = x,
                selected = input$aa_sel_assess)
  }
  
  
})

#-----infobox ui-----
output$aa_infobox_ui <- renderUI({
  req(!is.null(input$aa_sel_product))
  
  fluidPage(
    fluidRow(infoBoxOutput("aa_n_assessments"),
             infoBoxOutput("aa_n_item_responses"),
             infoBoxOutput("aa_avg_assessments_per_student"))
  )
})

#------REACTIVE COMPONENTS----
#---assessment table data prep-----

aa_assess_table_data <- reactive({
  req(!is.null(input$ou_select_pg), !is.null(input$aa_sel_product))
  
  df <- aa_assess_table_data_rdf %>%
    filter(product %in% input$aa_sel_product) %>%
    rename('Product' = product, 'Assessment ID' = test_id, 'Assessment Type' = assess_type,
           '# Items' = n_items, '# Skills' = n_skills, '# Students' = n_students, 'Avg Score' = avg_score,
           'SD Score' = sd_score, 'Alpha' = alpha)
  
  df
})

#----assessment score distribution data prep----

aa_assess_score_data <- reactive({
  req(!is.null(input$aa_sel_product), !is.null(input$aa_sel_assess), !is.null(input$ou_select_year))
  
  df <- aa_assess_score_dist_rdf %>%
    filter(test_id %in% input$aa_sel_assess) %>%
    select(-product, test_id)
  
  label <- labs(
    title = paste0('Product: ',input$aa_sel_product),
    subtitle = paste0('Assessment ID: ',input$aa_sel_assess,'\n',input$ou_select_year,'\n')
  )
  
  list(data = df, label = label)
})

#------assessment item table data prep-----

aa_assess_item_table_data <- reactive({
  req(!is.null(input$aa_sel_product), !is.null(input$aa_sel_assess))
  
  df <- aa_item_table_data_rdf %>%
    filter(test_id %in% input$aa_sel_assess) %>%
    rename('Product' = product, 'Assessment ID' = test_id, 'Item ID' = item_id,
           '# Skills' = n_skills, 'Skills' = skills, '# Students' = n_students,
           '# Responses' = n_responses, 'Avg Score' = avg_score, 'SD Score' = sd_score,
           'Median Time(sec)' = median_time, 'Biserial Correlation' = correlation)
  
  df
})



#------OUTPUT ELEMENTS------
#----infobox output----

output$aa_n_assessments <- renderInfoBox({
  req(!is.null(input$aa_sel_product))
  
  infoBox(format(as.numeric(aa_infobox_data_rdf$n_assess[aa_infobox_data_rdf$product == input$aa_sel_product]), big.mark=","),
          "Assessments", color = "red", icon = icon("fas fa-book-open"), fill = TRUE)
})

output$aa_n_item_responses <- renderInfoBox({
  req(!is.null(input$aa_sel_product))
  
  infoBox(format(as.numeric(aa_infobox_data_rdf$n_item_responses[aa_infobox_data_rdf$product == input$aa_sel_product]), big.mark=","),
          "Item Responses", color = "red", icon = icon('fas fa-pen'), fill = TRUE)
})

output$aa_avg_assessments_per_student <- renderInfoBox({
  req(!is.null(input$aa_sel_product))
  
  infoBox(format(as.numeric(aa_infobox_data_rdf$avg_assess_per_student[aa_infobox_data_rdf$product == input$aa_sel_product]), big.mark=","),
          "Assessments per Student", color = "red", icon = icon("fas fa-users"), fill = TRUE)
})


#-----assessment table----

output$aa_assess_table <- DT::renderDataTable({
  req(!is.null(input$aa_sel_product))
  
  ncols <- ncol(aa_assess_table_data())
  columns <- colnames(aa_assess_table_data())[(ncols-3):ncols]
  break_col <- break_col_func(aa_assess_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(aa_assess_table_data(), filter = 'top', options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ), class = 'cell-border stripe', selection = 'single')
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
})

aa_assess_table_proxy <- DT::dataTableProxy('aa_assess_table')

#------assessment score distribution-----

output$aa_assess_score_plot <- renderPlot({
  req(!is.null(input$aa_sel_product), !is.null(input$aa_sel_assess))
  
  assessment_score_distribution_plot(aa_assess_score_data()$data) +
    aa_assess_score_data()$label
})


#------assessment item table----

output$aa_assess_item_table <- DT::renderDataTable({
  req(!is.null(input$aa_sel_product), !is.null(input$aa_sel_assess))
  
  ncols <- ncol(aa_assess_item_table_data())
  columns <- colnames(aa_assess_item_table_data())[(ncols-3):ncols]
  break_col <- break_col_func(aa_assess_item_table_data(), columns)
  
  brks <- break_col$brks
  clrs <- break_col$clrs
  
  init_tbl <- DT::datatable(aa_assess_item_table_data(), filter = 'top', options = list(
    pageLength = 10, autoWidth = TRUE, sDom  = '<"top">lrt<"bottom">ip'
  ), class = 'cell-border stripe')
  
  purrr::reduce(columns, function(x, y) {
    DT::formatStyle(x, y, backgroundColor = DT::styleInterval(brks[[y]], clrs[[y]]))
    
  }, .init = init_tbl)
})


