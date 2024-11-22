library(shiny)
library(readxl)
library(forestdynR)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggplot2)
library(factoextra)


ui <- fluidPage(
    dashboardPage(
        skin = "blue",
        dashboardHeader(title = "forestdynR-Web",
                        titleWidth = 320
                        ),
        dashboardSidebar(
            width = 320,
            sidebarMenu(
                menuItem("Upload", tabName = "upload", icon = icon("upload")),
                menuItem("Report", tabName = "report", icon = icon("chart-bar")),
                menuItem("PCA", tabName = "pca", icon = icon("chart-line"))
            )
        ),
        dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
            ),
            tabItems(
                # Aba Upload
                tabItem(
                    tabName = "upload",
                    sidebarLayout(
                        sidebarPanel(
                            fileInput("file1", "Escolha um arquivo (.csv ou .xlsx)",
                                      accept = c(".csv", ".xlsx")),
                            numericInput("latitude", "Latitude:", value = 0, step = 0.0001),
                            numericInput("longitude", "Longitude:", value = 0, step = 0.0001),
                            numericInput("inv_time", "Intervalo de Tempo (inteiro):", value = 1, min = 1, step = 1),
                            actionButton("process", "Processar Dados"),
                            actionButton("plot_map", "Plotar Mapa"),
                            br(),
                            uiOutput("progress_ui")
                        ),
                        mainPanel(
                            DTOutput("contents"),
                            br(),
                            leafletOutput("map", height = 500),
                            br(),
                            verbatimTextOutput("dynOutput")
                        )
                    )
                ),
                # Aba Report
                tabItem(
                    tabName = "report",
                    fluidRow(
                        uiOutput("value_boxes")
                    )
                ),
                # Aba PCA
                tabItem(
                    tabName = "pca",
                    fluidRow(
                        column(4,
                               selectizeInput("pca_columns_n_species",
                                              "Selecione colunas de dynamics$n_species:",
                                              choices = c("death_rate", "recruitment_rate",
                                                          "net_change_rate", "turn"),
                                              selected = NULL,
                                              multiple = TRUE),
                               selectizeInput("pca_columns_basal_area_species",
                                              "Selecione colunas de dynamics$basal_area_species:",
                                              choices = c("BA_loss_rate", "BA_gain_rate",
                                                          "BA_net_change_rate", "BA_turn"),
                                              selected = NULL,
                                              multiple = TRUE),
                               radioButtons("pca_scale",
                                            "Escalonamento dos Dados:",
                                            choices = list(
                                                "Com escalonamento (recomendado para variáveis com unidades diferentes)" = TRUE,
                                                "Sem escalonamento (preserva unidades originais)" = FALSE
                                            ),
                                            inline = FALSE),
                               sliderInput("ellipse_conf",
                                           "Nível de Confiança da Elipse:",
                                           min = 0.5, max = 0.99, value = 0.95, step = 0.01),
                               selectInput("ellipse_type",
                                           "Tipo de Elipse:",
                                           choices = c("confidence" = "confidence", "convex" = "convex")),
                               actionButton("plot_pca", "Plotar PCA"),
                               br(), br(),
                               downloadButton("download_pca_csv", "Exportar PCA (.csv)", class = "btn-primary"),
                               downloadButton("download_pca_pdf", "Exportar PCA (.pdf)", class = "btn-success")
                        ),

                        column(8,
                               plotOutput("pca_plot", height = 500),  # Gráfico de PCA (biplot)
                               br(),
                               plotOutput("scree_plot", height = 400),  # Scree Plot
                               br(),
                               plotOutput("var_contrib_plot", height = 400)  # Contribuição das Variáveis
                        )
                        
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {

    filedata <- reactive({
        infile <- input$file1
        if (is.null(infile)) return(NULL)
        ext <- tools::file_ext(infile$name)
        if (ext == "csv") {
            read.csv2(infile$datapath, stringsAsFactors = FALSE)
        } else if (ext == "xlsx") {
            read_excel(infile$datapath)
        } else {
            showNotification("Formato de arquivo não suportado. Use .csv ou .xlsx.", type = "error")
            return(NULL)
        }
    })

    parameters <- reactive({
        list(
            coord = c(input$longitude, input$latitude),
            inv_time = input$inv_time
        )
    })

    dyn_object <- eventReactive(input$process, {
        data <- filedata()
        params <- parameters()
        if (is.null(data)) {
            showNotification("Carregue um arquivo antes de processar.", type = "error")
            return(NULL)
        }

        withProgress(message = 'Processando dados...', value = 0, {
            tryCatch({
                for (i in 1:10) {
                    incProgress(0.1)
                    Sys.sleep(0.2)
                }

                result <- forest_dyn(data, inv_time = params$inv_time, coord = params$coord)
                return(result)
            }, error = function(e) {
                showNotification(paste("Erro ao processar os dados:", e$message), type = "error")
                return(NULL)
            })
        })
    })

    output$contents <- renderDT({
        data <- filedata()
        if (is.null(data)) return(NULL)
        datatable(data, options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100)))
    })

    output$dynOutput <- renderPrint({
        result <- dyn_object()
        if (is.null(result)) {
            "Nenhum resultado disponível. Verifique os dados e tente novamente."
        } else {
            result
        }
    })

    output$value_boxes <- renderUI({
        result <- dyn_object()

        # Verifica se o resultado está disponível
        if (is.null(result)) {
            return(tags$p("Nenhum dado processado. Por favor, vá para a aba Upload e processe os dados."))
        }

        report_df <- result$report_df  # Assume que a função forest_dyn retorna um report_df

        if (is.null(report_df)) {
            return(tags$p("Relatório não disponível nos dados processados."))
        }

        # Organiza as seções
        sections <- unique(report_df$Section)

        vbs <- lapply(sections, function(section) {
            section_data <- report_df[report_df$Section == section, ]

            # Ajusta os valores arredondando
            section_data$Value <- sapply(1:nrow(section_data), function(i) {
                metric <- section_data$Metric[i]
                value <- section_data$Value[i]

                if (metric %in% c("Mortality Rate", "Recruitment Rate", "Net Change Rate in n",
                                  "Turnover Rate in n", "Basal Area Loss Rate", "Basal Area Gain Rate",
                                  "Net Change Rate in BA", "Turnover Rate in BA")) {
                    return(round(value, 3))  # Arredonda para 3 casas decimais
                }

                if (metric %in% c("Basal Area year 1", "Basal Area year 2", "Biomass year 1", "Biomass year 2")) {
                    return(round(value, 3))  # Arredonda para 3 casas decimais
                }

                # Caso contrário, retorna o valor original
                return(value)
            })

            # Cria os "p" com cada valor de Metric, Value e Unit
            metrics_list <- lapply(1:nrow(section_data), function(i) {
                p(paste(section_data$Metric[i], ": ", round(section_data$Value[i], 3), " ", section_data$Unit[i]))
            })

            # Cria o value_box para cada seção
            box_title <- paste(section)  # A Section será o título
            box(
                title = box_title,  # Exibe a Section como o título
                width = 12,         # A largura do box é 12 para ocupar toda a linha
                solidHeader = TRUE,
                status = "primary",
                div(
                    style = "margin-top: 10px; margin-bottom: 10px;",
                    metrics_list  # As métricas são passadas como detalhes
                )
            )
        })

        # Organiza os value-boxes verticalmente com margens entre eles
        fluidRow(
            column(
                width = 12,
                lapply(vbs, function(x) {
                    div(
                        style = "margin-bottom: 20px;",  # Margem entre os cards
                        x
                    )
                })
            )
        )
    })


    observeEvent(input$plot_map, {
        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addMarkers(lng = input$longitude, lat = input$latitude, popup = "Localização")
        })
    })

    output$pca_plot <- renderPlot({
        input$plot_pca  # Reativa o botão para plotar
        
        isolate({
            result <- dyn_object()
            if (is.null(result)) {
                showNotification("Processe os dados antes de executar a PCA.", type = "error")
                return(NULL)
            }
            
            # Seleciona colunas de interesse
            n_species_data <- result$dynamics$n_species
            basal_area_data <- result$dynamics$basal_area_species
            
            selected_columns <- c(
                input$pca_columns_n_species,
                input$pca_columns_basal_area_species
            )
            
            pca_data <- data.frame(n_species_data, basal_area_data)[, selected_columns, drop = FALSE]
            
            if (ncol(pca_data) < 2) {
                showNotification("Selecione ao menos duas colunas para a PCA.", type = "error")
                return(NULL)
            }
            
            # Verifica o método de escalonamento selecionado
            scale_option <- as.logical(input$pca_scale)
            
            # Realiza a PCA com a opção de escalonamento
            pca_result <- prcomp(pca_data, scale. = scale_option)
            
            # Armazena os resultados para exportação
            values$pca_result <- pca_result
            
            # Gráfico PCA com pontos para as espécies, sem rótulos
            fviz_pca_biplot(
                pca_result,
                geom = "point",             # Apenas pontos
                pointshape = 21,           # Forma dos pontos
                pointsize = 4,             # Tamanho dos pontos
                fill.ind = "#b7dfb9",      # Cor padrão para os pontos
                col.var = "cos2",          # Gradiente baseado em cos²
                gradient.cols = c("#FF4500", "#FF8C00", "#FFD700", "#7FFF00", "#00BFFF", "#1E90FF"),
                addEllipses = TRUE,        # Adiciona elipses de confiança
                ellipse.level = input$ellipse_conf,
                ellipse.type = input$ellipse_type,
                title = "Biplot da PCA com Espécies"
            ) +
                theme_minimal(base_size = 15) +  # Melhor qualidade visual
                theme(legend.position = "right")  # Move a legenda para a direita
        })
    })
    
    
    
    
    output$scree_plot <- renderPlot({
        input$plot_pca  # Reativa o botão para plotar
        
        isolate({
            result <- dyn_object()
            if (is.null(result)) {
                showNotification("Processe os dados antes de executar a PCA.", type = "error")
                return(NULL)
            }
            
            # Verifica se a PCA foi realizada
            pca_result <- values$pca_result
            if (is.null(pca_result)) {
                return(NULL)
            }
            
            # Scree Plot
            fviz_eig(pca_result, addlabels = TRUE, barfill = "#b7dfb9", barcolor = "#475957", linecolor = "red") +
                theme_minimal(base_size = 15) +
                ggtitle("Scree Plot: Variância Explicada")
        })
    })
    
    output$var_contrib_plot <- renderPlot({
        input$plot_pca  # Reativa o botão para plotar
        
        isolate({
            result <- dyn_object()
            if (is.null(result)) {
                showNotification("Processe os dados antes de executar a PCA.", type = "error")
                return(NULL)
            }
            
            # Verifica se a PCA foi realizada
            pca_result <- values$pca_result
            if (is.null(pca_result)) {
                return(NULL)
            }
            
            # Gráfico de Contribuição das Variáveis
            fviz_cos2(pca_result, choice = "var", axes = 1:2, fill = "#b7dfb9", color = "#475957") +
                theme_minimal(base_size = 15) +
                ggtitle("Contribuição das Variáveis (Cos²)")
        })
    })
    
    

    # Armazena os dados da PCA para exportação
    values <- reactiveValues(pca_result = NULL)

    # Exporta os resultados da PCA como CSV
    output$download_pca_csv <- downloadHandler(
        filename = function() { "pca_results.csv" },
        content = function(file) {
            pca_result <- values$pca_result
            if (is.null(pca_result)) {
                showNotification("Processe a PCA antes de exportar.", type = "error")
                return(NULL)
            }

            # Salva as componentes principais
            write.csv(pca_result$x, file, row.names = TRUE)
        }
    )

    # Exporta o gráfico da PCA como PDF
    output$download_pca_pdf <- downloadHandler(
        filename = function() { "pca_plots.pdf" },
        content = function(file) {
            pca_result <- values$pca_result
            if (is.null(pca_result)) {
                showNotification("Processe a PCA antes de exportar.", type = "error")
                return(NULL)
            }
            
            pdf(file, width = 10, height = 8)
            
            # Gráfico 1: Scree Plot
            scree_plot <- fviz_eig(pca_result, 
                                   addlabels = TRUE, 
                                   barfill = "#b7dfb9", 
                                   barcolor = "#475957", 
                                   linecolor = "red", 
                                   title = "Scree Plot: Variância Explicada")
            print(scree_plot)
            
            # Gráfico 2: Contribuição das Variáveis
            var_contrib_plot <- fviz_cos2(pca_result, 
                                          choice = "var", 
                                          axes = 1:2, 
                                          fill = "#b7dfb9", 
                                          color = "#475957", 
                                          title = "Contribuição das Variáveis (Cos²)")
            print(var_contrib_plot)
            
            # Gráfico 3: Biplot da PCA
            biplot <- fviz_pca_biplot(pca_result,
                                      geom = "point",
                                      pointshape = 21,
                                      pointsize = 4,
                                      fill.ind = "#b7dfb9",
                                      col.var = "cos2",
                                      gradient.cols = c("#FF4500", "#FF8C00", "#FFD700", "#7FFF00", "#00BFFF", "#1E90FF"),
                                      addEllipses = TRUE,
                                      ellipse.level = input$ellipse_conf,
                                      ellipse.type = input$ellipse_type,
                                      title = "Biplot da PCA com Espécies") +
                theme_minimal(base_size = 15) +
                theme(legend.position = "right")
            print(biplot)
            
            dev.off()
        }
    )
    
    

}

shinyApp(ui, server)

