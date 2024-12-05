function(input, output) {
  
  output$information <- renderUI({
    # show text when number of clicks is uneven; hide if even
    if(input$action_info %% 2 == 1){
      updateActionButton(inputId = "action_explain", label = "Got it!")
      renderUI({HTML("The imported data should be a .csv file. You can export it from excel as such. <br>
                     The columns have to be named: <br>
                     'ID', 'jugar_6', 'jugar_5', 'jugar_4', 'jugar_3', 'jugar_2', 'jugar_1'. <br>
                     The file needs to have as many rows as there are people, even if not 
                     all of them filled in the data. <br>
                     Each of the 6 possible categories was transformed into a weight of the connection between the two students. 
                     jugar_6 has the weight 3, jugar_5 the weight 2, jugar_4 the weight 1, jugar_3 the weight -1, jugar_2 the weight -2 and jugar_1 the weight -3. 
                     In this way, a connection can be drawn from each student to all students they noted down with a given connection strength. <br>
                     All values that are not a number will be converted to NA, meaning the corresponding links
                     will not appear in the networks. <br>
                     The checkbox 'Keep layout consistent' ensures that all networks are arranged in the same way. The arrangement is based on the full network. 
                     If unchecked, all networks will be arranged in a spring-like fashion. Nodes with more and stronger connections (it does not matter if positive or negative) 
                     will be more central than nodes with less and weaker connections. <br>
                     The summed network displays the sum of mutual connections. 
                     In this case information might get lost if two people have opposing opinions of each others. <br>
                     The threshold slider lets one exclude values below a certain threshold from the network. 
                     This makes the network less cluttered but one loses information")})
    } else {
      updateActionButton(inputId = "action_info", label = "Information")
      tagList()
    }
  })
  
  
  # Read CSV file
  observeEvent(input$file, {
    inFile <- input$file
    df_names <- read.csv(inFile$datapath)
    
    if (ncol(df_names) == 1){
      df_names <- read.csv2(inFile$datapath)
    }
    
    n_students <- nrow(df_names)
    
    student_names <- df_names[, 1]
    
    updateSelectInput(inputId = "student_id", choices = as.list(student_names))
    
    df_names_na <- df_names %>% mutate_at(vars(jugar_1, jugar_2, jugar_3, jugar_4, jugar_5, jugar_6), as.numeric)
    
    for (name in 1:n_students){
      df_names_na[df_names_na == name] <- student_names[name]
    }
    
    output$data <- renderTable(df_names_na, digits = 0)
    
    # Generate a warning if something is strange
    
    df <- df_names
    
    df[1] <- 1:n_students
    
    df <- df %>% mutate_all(as.numeric)
    
    df_long <- pivot_longer(df, cols = c(2, 3, 4, 5, 6, 7))
    
    df_long <- df_long %>% mutate(weight = gsub(pattern = "jugar_", 
                                                replacement = "", 
                                                name) %>% 
                                    as.numeric())
    
    
    df_long$weight <- ifelse(df_long$weight %in% c(1, 2, 3), 
                             df_long$weight - 1, 
                             df_long$weight) - 3 
    
    connections <- matrix(0, n_students, n_students)
    
    
    for (index in 1:nrow(df_long)){
      if (!is.na(df_long$value[index])){
        connections[df_long$ID[index], df_long$value[index]] <- df_long$weight[index]
      } 
    }
    
    # summation procession stage 
    connections_adding_upper <- matrix(0, n_students, n_students)
    connections_adding_lower_to_upper <- matrix(0, n_students, n_students)
    
    connections_adding_upper[upper.tri(connections_adding_upper)] <- connections[upper.tri(connections)]
    connections_adding_lower_to_upper[upper.tri(connections_adding_lower_to_upper)] <- t(connections)[upper.tri(connections)]
    
    connections_adding <- connections_adding_upper + connections_adding_lower_to_upper
    
    connections_adding <- t(connections_adding) + connections_adding
    
    # other optional processing stages 
    
    connections_final <- reactive({
      if (input$network_type == "raw"){
        connections_final <- connections
      } else if (input$network_type == "summed") {
        connections_final <- connections_adding

      }
    })
    
    #manage display of positive and negative connections
    connections_final_positive <- reactive({
      conns <- connections_final()
      connections_final_positive <- ifelse(conns < 0, 0, conns)
      })

    connections_final_negative <- reactive({
      conns <- connections_final()
      connections_final_negative <- ifelse(conns > 0, 0, conns)
    })
    
    connections_final_null <- reactive({
      connections_final_null <- connections_final() * 0
    })
    
    
    connections_output <- reactive({
      if (input$positive == TRUE & input$negative == FALSE){
        connections_output <- connections_final_positive() 
      } else if (input$positive == FALSE & input$negative == TRUE) {
        connections_output <- connections_final_negative()
      } else if (input$positive == TRUE & input$negative == TRUE) {
        connections_output <- connections_final()
      } else {
        connections_output <- connections_final_null()
      }
    })
    
    m_zero <- matrix(0, n_students, n_students)
    
    adjust_ones_ingoing <- function(student, names, n_students){
      specific_matrix <- matrix(0, n_students, n_students)
      specific_matrix[, which(names == student)] <- 1
      return(specific_matrix)
    }
    
    adjust_ones_outgoing <- function(student, names, n_students){
      specific_matrix <- matrix(0, n_students, n_students)
      specific_matrix[which(names == student), ] <- 1
      return(specific_matrix)
    }
    
    
    student_specific_matrix_ingoing <- reactive(adjust_ones_ingoing(input$student_id, student_names, n_students))
    student_specific_matrix_outgoing <- reactive(adjust_ones_outgoing(input$student_id, student_names, n_students))
    
    connections_output_final <- reactive({
      if (input$individual == TRUE & input$network_type != "summed"){
        if (input$ingoing_conns == TRUE & input$outgoing_conns == FALSE){
          connections_output_final <- connections_output() * student_specific_matrix_ingoing()
        } else if (input$outgoing_conns == TRUE & input$ingoing_conns == FALSE){
          connections_output_final <- connections_output() * student_specific_matrix_outgoing()
        } else if (input$outgoing_conns == FALSE & input$ingoing_conns == FALSE){
          connections_output_final <- connections_output() * 0
        } else connections_output_final <- connections_output() * (student_specific_matrix_outgoing() + student_specific_matrix_ingoing())
      } else if (input$individual == TRUE & input$network_type == "summed"){
        connections_output_final <- connections_output() * (student_specific_matrix_outgoing() + student_specific_matrix_ingoing())
      } else connections_output_final <- connections_output()
    })
    
    
    warn <- "test"
    output$warning <- renderUI({HTML(warn)})

    
    # Determine layout of full graph 
    original_layout <- qgraph(connections, 
                              threshold = 0)$layout
    
    graph_layout <- reactive({
      if (input$consistent_layout == TRUE){
        graph_layout <- original_layout
      } else {
        graph_layout <- "spring"
      } 
    })
    
    
    graph_threshold <- reactive(input$threshold)
    graph_node_size <- reactive(input$node_size)
    
    output$graph <- renderPlot({
      qgraph(connections_output_final(), 
             threshold = graph_threshold(), layout = graph_layout(), 
             labels = student_names,
             node.width = graph_node_size()
             )
    })

    output$download_image <- downloadHandler(
      filename = function() {
        paste0(gsub(pattern = ".csv", 
                    replacement = "", 
                    input$file), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 8)
        qgraph(connections_output_final(), 
               threshold = graph_threshold(), layout = graph_layout(), 
               labels = student_names,
               node.width = graph_node_size()
        )
        dev.off()
      }
    )
    
  })
  
}