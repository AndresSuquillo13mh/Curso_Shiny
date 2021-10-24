library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
load("movies.Rdata")

# Definir la interfaz de usuario para la aplicación que traza las características de las películas -----------
ui <- fluidPage(
  
  # Titulo de la aplicación -----------------------------------------------
  titlePanel(
    fluidRow(
      column(4, tags$strong("Navegador de películas")), 
      column(4, img(src="cde_horizontal.png", align = "center",height = 50, width = 200))
    )),
  
  # Diseño de barra lateral con definiciones de entrada y salida --------------
  sidebarLayout(
    
    # Inputs: Seleccionar variables para trazar ------------------------------
    sidebarPanel(
      
      # Seleccionar variable para el eje y ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("IMDB rating" = "imdb_rating", 
                              "IMDB number of votes" = "imdb_num_votes", 
                              "Critics Score" = "critics_score", 
                              "Audience Score" = "audience_score", 
                              "Runtime" = "runtime"), 
                  selected = "audience_score"),
      
      # Seleccionar variable para el eje x ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("IMDB rating" = "imdb_rating", 
                              "IMDB number of votes" = "imdb_num_votes", 
                              "Critics Score" = "critics_score", 
                              "Audience Score" = "audience_score", 
                              "Runtime" = "runtime"), 
                  selected = "critics_score"),
      
      # Seleccionar variable de color -----------------------------------
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Title Type" = "title_type", 
                              "Genre" = "genre", 
                              "MPAA Rating" = "mpaa_rating", 
                              "Critics Rating" = "critics_rating", 
                              "Audience Rating" = "audience_rating"),
                  selected = "mpaa_rating"),
      
      # Establecer nivel alfa ---------------------------------------------
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      # Establecer tamaño de punto ----------------------------------------------
      sliderInput(inputId = "size", 
                  label = "Tamaño:", 
                  min = 0, max = 5, 
                  value = 2),
      
      # Mostrar tabla de datos ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Mostrar tabla de datos",
                    value = TRUE),
      
      # Ingrese texto para el título del gráfico ---------------------------------------------
      textInput(inputId = "plot_title", 
                label = "Título del gráfico", 
                placeholder = "Ingrese el texto que se utilizará como título del gráfico"),
      
      # Línea horizontal para separación visual -----------------------
      hr(),
      
      # Seleccione qué tipos de películas a graficar ------------------------
      checkboxGroupInput(inputId = "selected_type",
                         label = "Seleccionar tipo de película:",
                         choices = c("Documentary" = "Documentary", 
                                     "Feature Film" = "Feature Film",
                                     "TV Movie" = "TV Movie"),
                         selected = "Feature Film"),
      
      # Seleccione el tamaño de la muestra ----------------------------------------------------
      numericInput(inputId = "n_samp", 
                   label = "Tamaño de la muestra:", 
                   min = 1, max = nrow(movies), 
                   value = 50),
      
      # Escribir datos de muestra como csv ------------------------------------------
      actionButton(inputId = "write_csv", 
                   label = "Escribir CSV")
      
    ),
    
    # Output: -------------------------------------------------------
    mainPanel(
      
      # Mostrar diagrama de dispersión --------------------------------------------
      plotOutput(outputId = "scatterplot"),
      br(),          # un poco de separación visual
      
      # Número de impresión de obs trazadas ---------------------------------
      uiOutput(outputId = "n"),
      br(), br(),    # un poco de separación visual

      # Mostrar tabla de datos ---------------------------------------------
      DT::dataTableOutput(outputId = "moviestable")
    )
  )
)

# Definir la función del servidor necesaria para crear el diagrama de dispersión ---------
server <- function(input, output, session) {
  
  # Cree un subconjunto de filtrado de datos para tipos de títulos seleccionados ------
  movies_subset <- reactive({
    req(input$selected_type) # garantizar la disponibilidad de valor antes de continuar
    filter(movies, title_type %in% input$selected_type)
  })
  
  # Actualice el n_samp máximo permitido para películas de tipo seleccionado ------
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(50, nrow(movies_subset())),
                       max = nrow(movies_subset())
    )
  })
  
  # Cree un nuevo df que sea n_samp obs de las películas de tipo seleccionado ------
  movies_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(movies_subset(), input$n_samp)
  })
  
  # Convertir plot_title enTitleCase ----------------------------------
  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
  
  # Crear un objeto de diagrama de dispersión que espera la función plotOutput --
  output$scatterplot <- renderPlot({
    ggplot(data = movies_sample(), aes_string(x = input$x, y = input$y,
                                              color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = isolate({ pretty_plot_title() })
      )
  })
  
  # Imprimir número de películas trazadas ----------------------------------
  output$n <- renderUI({
    types <- movies_sample()$title_type %>% 
      factor(levels = input$selected_type) 
    counts <- table(types)
    
    HTML(paste("Existen", counts, input$selected_type, "películas en este conjunto de datos. <br>"))
  })
  
  # Imprimir tabla de datos si está marcado -------------------------------------
  output$moviestable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = movies_sample()[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  # Escribir datos de muestra como csv ---------------------------------------
  observeEvent(eventExpr = input$write_csv, 
               handlerExpr = {
                 filename <- paste0("movies_", str_replace_all(Sys.time(), ":|\ ", "_"), ".csv")
                 write.csv(movies_sample(), file = filename, row.names = FALSE) 
                 }
  )
  
}

# Ejecuta la aplicación -----------------------------------------------
shinyApp(ui = ui, server = server)
