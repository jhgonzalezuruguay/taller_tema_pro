library(shiny)

ui <- navbarPage("Taller de Tema y Problema",
                                  
                                  tags$head(
                                    tags$style(HTML("
                     
                     /* Botones normales */
                     .btn,
                     .btn-default,
                     .btn-primary {
                       background-color: #90EE90 !important;
                       border-color: #90EE90 !important;
                       color: black !important;
                       background-image: none !important;
                     }
                     
                     /* Estados hover, focus y active */
                     .btn:hover,
                     .btn:focus,
                     .btn:active,
                     .btn-default:hover,
                     .btn-default:focus,
                     .btn-default:active,
                     .btn-primary:hover,
                     .btn-primary:focus,
                     .btn-primary:active {
                       background-color: #90EE90 !important;
                       border-color: #90EE90 !important;
                       color: black !important;
                       box-shadow: none !important;
                       outline: none !important;
                     }
                     
                   "))
                                  ),                 
                 # --------------------------
                 # CAPÍTULO II
                 # --------------------------
                 tabPanel("Tema y Problema: Acceso a la Justicia",
                          h3("El tema y el problema de investigación"),
                          br(),
                          h4("Conceptos clave"),br(),
                          tags$ul(
                            tags$li("Tema: 'Acceso a la Justicia'."),
                            tags$li("Problema: pregunta concreta que guía la investigación."),
                            tags$li("Ejemplo: ¿Qué obstáculos enfrentan las personas de bajos ingresos para acceder a la justicia de familia en Montevideo entre 2018 y 2023?"),
                            tags$li("Esquema: Tema amplio → Preguntas iniciales → Problematización → Formulación del problema."),
                            tags$li("Un buen problema debe ser original, factible y verificable.")
                          ),
                          br(),
                          # --- Quiz ---
                          h4("Quiz"),
                          radioButtons("qTema", "1) ¿Cuál de las siguientes opciones es un problema de investigación?",
                                       choices = list(
                                         "Acceso a la Justicia en Uruguay" = "a",
                                         "¿Qué barreras enfrentan las mujeres migrantes para acceder a la justicia laboral en Montevideo entre 2018 y 2023?" = "b",
                                         "El sistema judicial existe" = "c"
                                       ), selected = character(0)),
                          radioButtons("qProb", "2) ¿Qué característica NO debe tener un problema de investigación?",
                                       choices = list(
                                         "Ser verificable" = "a",
                                         "Ser relevante" = "b",
                                         "Ser completamente subjetivo" = "c"
                                       ), selected = character(0)),
                          actionButton("submitQuiz2", "Responder"),
                          textOutput("feedbackQuiz2"),
                          br(),
                          # --- Actividad práctica individual ---
                          h4("Actividad práctica individual"),
                          p("Ejercicio: Formula un problema de investigación vinculado al tema 'Acceso a la Justicia'."),
                          textAreaInput("problemaAcceso", "Escribe tu problema de investigación:", width = "100%", height = "80px"),
                          actionButton("solAcceso", "Ver respuesta sugerida"),br(),
                          textOutput("respAcceso"),br(),br(),
                          downloadButton("downloadProblema", "Descargar mi problema en .txt"),
                          br(),br(),
                          # --- Verdadero/Falso ---
                          h4("Verdadero/Falso"),
                          checkboxGroupInput("vfAcceso", "Marca las afirmaciones correctas:",
                                             choices = list(
                                               "El tema es amplio y general" = "a",
                                               "El problema delimita y hace viable el estudio" = "b",
                                               "El problema puede ser completamente subjetivo" = "c",
                                               "El problema debe ser verificable" = "d"
                                             )),
                          actionButton("submitVFAcceso", "Verificar"),
                          textOutput("feedbackVFAcceso"),
                          br(),
                          # --- Asociación de conceptos ---
                          h4("Asociación de conceptos"),
                          selectInput("asocTema", "El tema se caracteriza por:", 
                                      choices = c("", "Ser amplio y general", "Ser una pregunta concreta", "Ser verificable")),
                          selectInput("asocProblema", "El problema se caracteriza por:", 
                                      choices = c("", "Delimitar y guiar la investigación", "Ser un campo general", "Ser irrelevante")),
                          actionButton("submitAsocAcceso", "Verificar respuestas"),
                          textOutput("feedbackAsocAcceso"),
                          br(),
                          # --- Mini caso crítico ---
                          h4("Mini caso crítico"),
                          p("Un investigador afirma que su estudio sobre 'Acceso a la Justicia' es científico porque entrevistó a 10 personas sin definir un problema claro."),
                          radioButtons("casoAcceso", "¿Qué problema aparece?",
                                       choices = list(
                                         "Falta de coherencia entre tema y problema" = "a",
                                         "Exceso de validez" = "b",
                                         "Uso correcto del problema" = "c"
                                       ), selected = character(0)),
                          actionButton("submitCasoAcceso", "Responder"),
                          textOutput("feedbackCasoAcceso"),
                          br(),
                          # --- Actividad de reflexión ---
                          h4("Actividad de reflexión"),
                          p("Escribe una breve reflexión sobre la importancia de formular correctamente un problema de investigación en torno al 'Acceso a la Justicia'."),
                          textAreaInput("reflexionAcceso", "Mi reflexión:", width = "100%", height = "100px"),
                          downloadButton("downloadReflexion", "Descargar mi reflexión en .txt"),
                          br(),br(),
                          # --- Actividad de comparación ---
                          h4("Actividad de comparación"),
                          
                          p("Compara dos problemas de investigación sobre 'Acceso a la Justicia'. Identifica cuál está mejor formulado y por qué. Ejemplo de respuesta esperada: El Problema A es demasiado general y no define población, tiempo ni contexto. El Problema B está mejor formulado porque delimita población (migrantes), ámbito (justicia laboral), lugar (Montevideo) y tiempo (2018-2023). Por eso es más viable y científico."),
                          textAreaInput("comparacionAcceso", "Escribe tu comparación:", width = "100%", height = "100px"),
                          downloadButton("downloadComparacion", "Descargar mi comparación en .txt"),
                          br(),br(),
                          # --- Flashcards ---
                          h4("Flashcards: Tema y Problema"),
                          p("Usa estas tarjetas para repasar conceptos clave."),
                          actionButton("flashTema", "Ver definición de Tema"),
                          textOutput("cardTema"),br(),br(),
                          actionButton("flashProblema", "Ver definición de Problema"),
                          textOutput("cardProblema")
                 ),br(),br()
)

server <- function(input, output, session) {
  
  # --- Quiz feedback ---
  observeEvent(input$submitQuiz2, {
    if (!is.null(input$qTema) && !is.null(input$qProb)) {
      if (input$qTema == "b" && input$qProb == "c") {
        output$feedbackQuiz2 <- renderText("¡Correcto! Has identificado bien qué es un problema de investigación y qué característica no debe tener.")
      } else {
        output$feedbackQuiz2 <- renderText("Revisa tus respuestas: recuerda que el problema debe ser concreto, verificable y relevante.")
      }
    } else {
      output$feedbackQuiz2 <- renderText("Por favor selecciona una opción en ambas preguntas antes de responder.")
    }
  })
  
  # --- Actividad práctica individual ---
  observeEvent(input$solAcceso, {
    output$respAcceso <- renderText("Respuesta sugerida: ¿Qué obstáculos enfrentan las personas de bajos ingresos para acceder a la justicia de familia en Montevideo entre 2018 y 2023?")
  })
  
  output$downloadProblema <- downloadHandler(
    filename = function() { "mi_problema.txt" },
    content = function(file) {
      writeLines(input$problemaAcceso, file)
    }
  )
  
  # --- Verdadero/Falso ---
  observeEvent(input$submitVFAcceso, {
    correct <- c("a","b","d")
    if (all(input$vfAcceso %in% correct) && length(input$vfAcceso) == length(correct)) {
      output$feedbackVFAcceso <- renderText("¡Correcto! Has identificado bien las afirmaciones.")
    } else {
      output$feedbackVFAcceso <- renderText("Revisa tus respuestas: recuerda que el tema es amplio y el problema debe ser concreto, verificable y relevante.")
    }
  })
  
  # --- Asociación de conceptos ---
  observeEvent(input$submitAsocAcceso, {
    if (input$asocTema == "Ser amplio y general" && input$asocProblema == "Delimitar y guiar la investigación") {
      output$feedbackAsocAcceso <- renderText("¡Correcto! Tema = amplio y general; Problema = delimita y guía la investigación.")
    } else {
      output$feedbackAsocAcceso <- renderText("Revisa tus respuestas: recuerda que el tema es amplio y el problema delimita y guía.")
    }
  })
  
  # --- Mini caso crítico ---
  observeEvent(input$submitCasoAcceso, {
    if (input$casoAcceso == "a") {
      output$feedbackCasoAcceso <- renderText("¡Correcto! El problema es la falta de coherencia entre tema y problema.")
    } else {
      output$feedbackCasoAcceso <- renderText("Revisa: el problema central es la incoherencia entre tema y problema.")
    }
  })
  
  # --- Reflexión descargable ---
  output$downloadReflexion <- downloadHandler(
    filename = function() { "mi_reflexion.txt" },
    content = function(file) {
      writeLines(input$reflexionAcceso, file)
    }
  )
  
  # --- Comparación descargable ---
  output$downloadComparacion <- downloadHandler(
    filename = function() { "mi_comparacion.txt" },
    content = function(file) {
      writeLines(input$comparacionAcceso, file)
    }
  )
  
  # --- Flashcards ---
  observeEvent(input$flashTema, {
    output$cardTema <- renderText("Tema: punto de partida amplio y general. Ejemplo: 'Acceso a la Justicia'.")
  })
  
  observeEvent(input$flashProblema, {
    output$cardProblema <- renderText("Problema: pregunta concreta que delimita y guía la investigación. Ejemplo: '¿Qué obstáculos enfrentan las personas de bajos ingresos para acceder a la justicia de familia en Montevideo entre 2018 y 2023?'")
  })
}

shinyApp(ui, server)
                 
