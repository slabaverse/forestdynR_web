if (!requireNamespace("httr2", quietly = TRUE)) {
    install.packages("httr2")
}

library(shiny)
library(readxl)
library(forestdynR)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggplot2)
library(factoextra)
library(rmarkdown)
library(pagedown)



ui <- fluidPage(
    dashboardPage(
        skin = "blue",
        dashboardHeader(title = "forestdynR-Web", titleWidth = 320),
        dashboardSidebar(
            width = 320,
            sidebarMenu(
                menuItem("Upload", tabName = "upload", icon = icon("upload")),
                menuItem("Report", tabName = "report", icon = icon("chart-bar")),
                menuItem("PCA", icon = icon("chart-line"),  # Restaurar a aba PCA
                         menuSubItem("PCA spp", tabName = "pca_spp"),
                         menuSubItem("PCA par", tabName = "pca_par")
                )
            )
        ),
        dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                tags$style(HTML("
          /* Adicionar margens ao dynOutput */
          #dynOutput {
            margin: 20px; /* Margem de 20px em todos os lados */
            padding: 15px; /* Espaçamento interno */
            border: 1px solid #ccc; /* Borda cinza */
            border-radius: 5px; /* Cantos arredondados */
            background-color: #f9f9f9; /* Fundo cinza claro */
          }
        "))
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
                            leafletOutput("map", height = 500)
                        )
                    )
                ),
                # Aba Report
                tabItem(
                    tabName = "report",
                    fluidRow(
                        verbatimTextOutput("dynOutput"),  # Exibir o conteúdo de dynOutput
                        downloadButton("download_report_pdf", "Exportar Report em PDF", class = "btn-primary")  # Botão de download
                    )
                ),
                # Aba PCA spp
                tabItem(
                    tabName = "pca_spp",
                    fluidRow(
                        column(4,
                               selectizeInput("pca_columns_n_species",
                                              "Selecione colunas (n - species):",
                                              choices = c("death_rate", "recruitment_rate",
                                                          "net_change_rate", "turn"),
                                              selected = NULL,
                                              multiple = TRUE),
                               selectizeInput("pca_columns_basal_area_species",
                                              "Selecione colunas (basal area - species):",
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
                               actionButton("plot_pca_spp", "Plotar PCA"),
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
                ),
                # Aba PCA par
                tabItem(
                    tabName = "pca_par",
                    fluidRow(
                        column(4,
                               selectizeInput("pca_columns_n_plot",
                                              "Selecione colunas (n - plot):",
                                              choices = c("death_rate", "recruitment_rate",
                                                          "net_change_rate", "turn"),
                                              selected = NULL,
                                              multiple = TRUE),
                               selectizeInput("pca_columns_basal_area_plot",
                                              "Selecione colunas (basal area - plot):",
                                              choices = c("BA_loss_rate", "BA_gain_rate",
                                                          "BA_net_change_rate", "BA_turn"),
                                              selected = NULL,
                                              multiple = TRUE),
                               radioButtons("pca_scale_par",
                                            "Escalonamento dos Dados:",
                                            choices = list(
                                                "Com escalonamento (recomendado para variáveis com unidades diferentes)" = TRUE,
                                                "Sem escalonamento (preserva unidades originais)" = FALSE
                                            ),
                                            inline = FALSE),
                               sliderInput("ellipse_conf_par",
                                           "Nível de Confiança da Elipse:",
                                           min = 0.5, max = 0.99, value = 0.95, step = 0.01),
                               selectInput("ellipse_type_par",
                                           "Tipo de Elipse:",
                                           choices = c("confidence" = "confidence", "convex" = "convex")),
                               actionButton("plot_pca_par", "Plotar PCA"),
                               br(), br(),
                               downloadButton("download_pca_par_csv", "Exportar PCA (.csv)", class = "btn-primary"),
                               downloadButton("download_pca_par_pdf", "Exportar PCA (.pdf)", class = "btn-success")
                        ),
                        column(8,
                               plotOutput("pca_plot_par", height = 500),  # Gráfico de PCA (biplot)
                               br(),
                               plotOutput("scree_plot_par", height = 400),  # Scree Plot
                               br(),
                               plotOutput("var_contrib_plot_par", height = 400)  # Contribuição das Variáveis
                        )
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # Variável reativa para armazenar o resultado do processamento
    values <- reactiveValues(dyn_result = NULL)
    
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
    
    observeEvent(input$process, {
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
                
                # Processar os dados
                result <- forest_dyn(data, inv_time = params$inv_time, coord = params$coord)
                
                # Armazenar o resultado na variável reativa
                values$dyn_result <- result
                
                showNotification("Dados processados com sucesso!", type = "message")
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
        result <- values$dyn_result  # Acessar a variável reativa
        if (is.null(result)) {
            "Nenhum resultado disponível. Verifique os dados e tente novamente."
        } else {
            result
        }
    })
    
    outputOptions(output, "dynOutput", suspendWhenHidden = FALSE)
    
    observeEvent(input$plot_map, {
        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addMarkers(lng = input$longitude, lat = input$latitude, popup = "Localização")
        })
    })
    
    output$download_report_pdf <- downloadHandler(
        filename = function() {
            paste("report_", Sys.Date(), ".pdf", sep = "")  # Nome do arquivo PDF
        },
        content = function(file) {
            # Capturar o conteúdo de dynOutput
            report_content <- capture.output(print(values$dyn_result))
            
            # Criar um arquivo temporário HTML
            temp_html <- tempfile(fileext = ".html")
            temp_content <- tempfile(fileext = ".txt")
            
            # Salvar o conteúdo de dynOutput em um arquivo temporário
            writeLines(report_content, temp_content)
            
            # Criar o conteúdo HTML com tamanho de fonte menor
            html_content <- c(
                "<!DOCTYPE html>",
                "<html>",
                "<head>",
                "<style>",
                "@page { size: landscape; }",  # Orientação paisagem
                "body { font-family: Arial, sans-serif; margin: 1cm; font-size: 10px; }",  # Tamanho da fonte (10px)
                "pre { white-space: pre-wrap; word-wrap: break-word; font-size: 10px; }",  # Tamanho da fonte para conteúdo pré-formatado
                "</style>",
                "</head>",
                "<body>",
                "<pre>",
                paste(readLines(temp_content), collapse = "\n"),
                "</pre>",
                "</body>",
                "</html>"
            )
            
            # Salvar o conteúdo HTML no arquivo temporário
            writeLines(html_content, temp_html)
            
            # Renderizar o PDF usando pagedown
            tryCatch({
                pagedown::chrome_print(temp_html, output = file)
            }, error = function(e) {
                showNotification(paste("Erro ao gerar o PDF:", e$message), type = "error")
            })
            
            # Remover os arquivos temporários
            file.remove(temp_html)
            file.remove(temp_content)
        }
    )
    
    output$pca_plot <- renderPlot({
        input$plot_pca_spp  # Reativa o botão para plotar
        
        isolate({
            result <- values$dyn_result
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
        input$plot_pca_spp  # Reativa o botão para plotar
        
        isolate({
            result <- values$dyn_result
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
        input$plot_pca_spp  # Reativa o botão para plotar
        
        isolate({
            result <- values$dyn_result
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
    
    output$pca_plot_par <- renderPlot({
        input$plot_pca_par  # Reativa o botão para plotar
        
        isolate({
            result <- values$dyn_result
            if (is.null(result)) {
                showNotification("Processe os dados antes de executar a PCA.", type = "error")
                return(NULL)
            }
            
            # Seleciona colunas de interesse
            n_plot_data <- result$dynamics$n_plot
            basal_area_plot_data <- result$dynamics$basal_area_plot
            
            selected_columns <- c(
                input$pca_columns_n_plot,
                input$pca_columns_basal_area_plot
            )
            
            pca_data <- data.frame(n_plot_data, basal_area_plot_data)[, selected_columns, drop = FALSE]
            
            if (ncol(pca_data) < 2) {
                showNotification("Selecione ao menos duas colunas para a PCA.", type = "error")
                return(NULL)
            }
            
            # Verifica o método de escalonamento selecionado
            scale_option <- as.logical(input$pca_scale_par)
            
            # Realiza a PCA com a opção de escalonamento
            pca_result_par <- prcomp(pca_data, scale. = scale_option)
            
            # Armazena os resultados para exportação
            values$pca_result_par <- pca_result_par
            
            # Gráfico PCA com pontos para os plots, sem rótulos
            fviz_pca_biplot(
                pca_result_par,
                geom = "point",             # Apenas pontos
                pointshape = 21,           # Forma dos pontos
                pointsize = 4,             # Tamanho dos pontos
                fill.ind = "#b7dfb9",      # Cor padrão para os pontos
                col.var = "cos2",          # Gradiente baseado em cos²
                gradient.cols = c("#FF4500", "#FF8C00", "#FFD700", "#7FFF00", "#00BFFF", "#1E90FF"),
                addEllipses = TRUE,        # Adiciona elipses de confiança
                ellipse.level = input$ellipse_conf_par,
                ellipse.type = input$ellipse_type_par,
                title = "Biplot da PCA com Plots"
            ) +
                theme_minimal(base_size = 15) +  # Melhor qualidade visual
                theme(legend.position = "right")  # Move a legenda para a direita
        })
    })
    
    output$scree_plot_par <- renderPlot({
        input$plot_pca_par  # Reativa o botão para plotar
        
        isolate({
            pca_result_par <- values$pca_result_par
            if (is.null(pca_result_par)) return(NULL)
            
            # Scree Plot
            fviz_eig(pca_result_par, addlabels = TRUE, barfill = "#b7dfb9", barcolor = "#475957", linecolor = "red") +
                theme_minimal(base_size = 15) +
                ggtitle("Scree Plot: Variância Explicada")
        })
    })
    
    output$var_contrib_plot_par <- renderPlot({
        input$plot_pca_par  # Reativa o botão para plotar
        
        isolate({
            pca_result_par <- values$pca_result_par
            if (is.null(pca_result_par)) return(NULL)
            
            # Gráfico de Contribuição das Variáveis
            fviz_cos2(pca_result_par, choice = "var", axes = 1:2, fill = "#b7dfb9", color = "#475957") +
                theme_minimal(base_size = 15) +
                ggtitle("Contribuição das Variáveis (Cos²)")
        })
    })
    
    
    # Armazena os dados da PCA para exportação
    values <- reactiveValues(pca_result = NULL, pca_result_par = NULL)
    
    
    # Exporta os resultados da PCA como CSV
    output$download_pca_csv <- downloadHandler(
        filename = function() { "pca_spp_results.csv" },
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
        filename = function() { "pca_spp_graph.pdf" },
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
    output$download_pca_par_csv <- downloadHandler(
        filename = function() { "pca_par_results.csv" },
        content = function(file) {
            pca_result_par <- values$pca_result_par
            if (is.null(pca_result_par)) {
                showNotification("Processe a PCA antes de exportar.", type = "error")
                return(NULL)
            }
            
            # Salva as componentes principais
            write.csv(pca_result_par$x, file, row.names = TRUE)
        }
    )
    
    output$download_pca_par_pdf <- downloadHandler(
        filename = function() { "pca_par_plots.pdf" },
        content = function(file) {
            pca_result_par <- values$pca_result_par
            if (is.null(pca_result_par)) {
                showNotification("Processe a PCA antes de exportar.", type = "error")
                return(NULL)
            }
            
            pdf(file, width = 10, height = 8)
            
            # Gráfico 1: Scree Plot
            scree_plot <- fviz_eig(pca_result_par, 
                                   addlabels = TRUE, 
                                   barfill = "#b7dfb9", 
                                   barcolor = "#475957", 
                                   linecolor = "red", 
                                   title = "Scree Plot: Variância Explicada")
            print(scree_plot)
            
            # Gráfico 2: Contribuição das Variáveis
            var_contrib_plot <- fviz_cos2(pca_result_par, 
                                          choice = "var", 
                                          axes = 1:2, 
                                          fill = "#b7dfb9", 
                                          color = "#475957", 
                                          title = "Contribuição das Variáveis (Cos²)")
            print(var_contrib_plot)
            
            # Gráfico 3: Biplot da PCA
            biplot <- fviz_pca_biplot(pca_result_par,
                                      geom = "point",
                                      pointshape = 21,
                                      pointsize = 4,
                                      fill.ind = "#b7dfb9",
                                      col.var = "cos2",
                                      gradient.cols = c("#FF4500", "#FF8C00", "#FFD700", "#7FFF00", "#00BFFF", "#1E90FF"),
                                      addEllipses = TRUE,
                                      ellipse.level = input$ellipse_conf_par,
                                      ellipse.type = input$ellipse_type_par,
                                      title = "Biplot da PCA com Plots") +
                theme_minimal(base_size = 15) +
                theme(legend.position = "right")
            print(biplot)
            
            dev.off()
        }
    )
    
}

shinyApp(ui, server)

