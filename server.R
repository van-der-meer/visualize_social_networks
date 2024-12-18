function(input, output) {
  
  output$information <- renderUI({
    if(input$action_info %% 2 == 1){
      updateActionButton(inputId = "action_explain", label = "¡Entendido!")
      HTML("
        <p>El archivo importado debe ser un archivo <strong>.csv</strong>. Puedes exportarlo desde Excel en este formato.</p>
        <p>Las columnas deben estar nombradas de la siguiente manera:</p>
        <ul>
          <li><strong>ID</strong>, <strong>jugar_6</strong>, <strong>jugar_5</strong>, <strong>jugar_4</strong>, <strong>jugar_3</strong>, <strong>jugar_2</strong>, <strong>jugar_1</strong></li>
        </ul>
        <p>El archivo debe tener tantas filas como personas haya, incluso si no todas están llenas con datos.</p>
        <p>Cada una de las 6 posibles categorías se transformó en un peso de la conexión entre dos estudiantes:</p>
        <ul>
          <li><strong>jugar_6</strong> tiene el peso 3</li>
          <li><strong>jugar_5</strong> tiene el peso 2</li>
          <li><strong>jugar_4</strong> tiene el peso 1</li>
          <li><strong>jugar_3</strong> tiene el peso -1</li>
          <li><strong>jugar_2</strong> tiene el peso -2</li>
          <li><strong>jugar_1</strong> tiene el peso -3</li>
        </ul>
        <p>De esta forma, se puede dibujar una conexión entre cada estudiante y todos aquellos con los que se hayan anotado, con una fuerza de conexión dada.</p>
        <p>Todos los valores que no sean numéricos serán convertidos en <strong>NA</strong>, lo que significa que los enlaces correspondientes no aparecerán en la red.</p>
        <p>La opción <strong>'Mantener el diseño consistente'</strong> asegura que todas las redes se organicen de la misma manera. El diseño se basa en la red completa. Si no se marca, todas las redes se organizarán de forma similar a un resorte. Los nodos con más y más fuertes conexiones (sin importar si son positivas o negativas) serán más centrales que aquellos con menos y más débiles conexiones.</p>
        <p>La red sumada muestra la suma de las conexiones mutuas. En este caso, se puede perder información si dos personas tienen opiniones opuestas entre sí.</p>
        <p>El control deslizante del umbral permite excluir valores por debajo de un cierto umbral de la red. Esto hace que la red esté menos desordenada, pero se pierde información.</p>
      ")
    } else {
      updateActionButton(inputId = "action_info", label = "Información")
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