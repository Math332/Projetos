 # Instale e carregue as bibliotecas necessárias
 if (!require("shiny")) install.packages("shiny")
 if (!require("ggplot2")) install.packages("ggplot2")
 if (!require("leaflet")) install.packages("leaflet")
 if (!require("sf")) install.packages("sf")
 if (!require("shinydashboard")) install.packages("shinydashboard")
 if (!require("shinyjs")) install.packages("shinyjs")
 if (!require("shinyWidgets")) install.packages("shinyWidgets")
 if (!require("dplyr")) install.packages("dplyr")
 if (!require("lubridate")) install.packages("lubridate")
 if (!require("DT")) install.packages("DT")
 
 library(shiny)
 library(ggplot2)
 library(leaflet)
 library(sf)
 library(shinydashboard)
 library(shinyjs)
 library(shinyWidgets)
 library(dplyr)
 library(lubridate)
 library(DT)
 
 # Dados de exemplo
 dados_vendas <- data.frame(
     Loja = c("Walmart", "Amazon", "Alibaba", "Costco", "Tesco", "Target", "Apple", "Google", "Microsoft"),
     Vendas = c(524, 386, 768, 254, 204, 300, 600, 450, 800),
     Latitude = c(35.4676, 47.6062, 30.5928, 47.6740, 51.5074, 41.8781, 37.7749, 37.7749, 47.6062),
     Longitude = c(-97.5164, -122.3321, 114.3055, -122.2171, -0.1278, -87.6298, -122.4194, -122.4194, -122.3321),
     Data = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01", "2022-06-01", "2022-07-01", "2022-08-01", "2022-09-01")),
     Pais = c("EUA", "EUA", "China", "EUA", "Reino Unido", "EUA", "EUA", "EUA", "EUA"),
     Estado = c("Arkansas", "Washington", "Zhejiang", "Washington", "Hertfordshire", "Minnesota", "California", "California", "Washington"),
     Cidade = c("Bentonville", "Seattle", "Hangzhou", "Issaquah", "Cheshunt", "Minneapolis", "Cupertino", "Mountain View", "Redmond")
 )
 
 # Interface do aplicativo Shiny
 ui <- dashboardPage(
     dashboardHeader(title = "Dashboard"),
     dashboardSidebar(
         selectizeInput("filtro_unificado", "Filtrar:", choices = c("Todas", "Loja", "Mês", "Ano"), selected = "Todas"),
         uiOutput("filtro_dinamico")
     ),
     dashboardBody(
         fluidRow(
             box(
                 width = 12,
                 style = "display: flex; flex-direction: row; flex-wrap: wrap; justify-content: space-between;",
                 div(
                     div(
                         shinydashboard::infoBoxOutput("kpi_total_vendas"),
                         shinydashboard::infoBoxOutput("kpi_media_vendas"),
                         shinydashboard::infoBoxOutput("kpi_maior_venda"),
                         #selectizeInput("filtro_ano", "Selecione o Ano:", choices = unique(year(dados_vendas$Data)), multiple = TRUE),
                         style = "background-color: #dcdcdc; padding: 10px; margin: 10px; border-radius: 5px; color: black; display: inline-block;"
                     ),
                     style = "display: flex; flex-direction: row; flex-wrap: wrap; justify-content: space-between;"
                 )
             ),
             box(
                 width = 12,
                 plotOutput("plot_bar")
             ),
             box(
                 width = 12,
                 leafletOutput("map")
             ),
             box(
                 width = 12,
                 plotOutput("plot_temporal_kpi")
             ),
             box(
                 width = 6,
                 plotOutput("plot_vendas_cidade")
             ),
             box(
                 width = 6,
                 plotOutput("plot_vendas_pais")
             ),
             box(
                 width = 12,
                 DTOutput("tabela_dinamica")
             ),
             useShinyjs()
         )
     )
 )
 
 # Servidor Shiny
 server <- function(input, output) {
     # Filtro unificado
     output$filtro_dinamico <- renderUI({
         filtro <- input$filtro_unificado
         if (filtro == "Todas") return(NULL)
         if (filtro == "Loja") {
             selectizeInput("filtro_loja", "Selecione a Loja:", choices = unique(dados_vendas$Loja), multiple = TRUE)
         } else if (filtro == "Mês") {
             selectizeInput("filtro_mes", "Selecione o Mês:", choices = month.abb, multiple = TRUE)
         } else if (filtro == "Ano") {
             selectizeInput("filtro_ano", "Selecione o Ano:", choices = unique(year(dados_vendas$Data)), multiple = TRUE)
         }
     })
     
     # Dados filtrados com base no filtro selecionado
     dados_filtrados <- reactive({
         filtro <- input$filtro_unificado
         if (filtro == "Todas") {
             return(dados_vendas)
         } else if (filtro == "Loja") {
             lojas_selecionadas <- input$filtro_loja
             return(dados_vendas[dados_vendas$Loja %in% lojas_selecionadas, ])
         } else if (filtro == "Mês") {
             meses_selecionados <- input$filtro_mes
             return(dados_vendas[month(dados_vendas$Data) %in% match(meses_selecionados, month.abb), ])
         } else if (filtro == "Ano") {
             anos_selecionados <- as.numeric(input$filtro_ano)
             return(dados_vendas[year(dados_vendas$Data) %in% anos_selecionados, ])
         }
     })
     
     # KPI Total de Vendas
     output$kpi_total_vendas <- renderInfoBox({
         dados <- dados_filtrados()
         shinydashboard::infoBox(
             title = "Total de Vendas",
             value = sum(dados$Vendas),
             color = "purple",
             icon = icon("shopping-cart")
         )
     })
     
     # KPI Média de Vendas
     output$kpi_media_vendas <- renderInfoBox({
         dados <- dados_filtrados()
         shinydashboard::infoBox(
             title = "Média de Vendas",
             value = round(mean(dados$Vendas), 2),
             color = "blue",
             icon = icon("shopping-cart")
         )
     })
     
     # KPI Loja com Maior Venda
     output$kpi_maior_venda <- renderInfoBox({
         dados <- dados_filtrados()
         loja_maior_venda <- dados$Loja[which.max(dados$Vendas)]
         shinydashboard::infoBox(
             title = "Loja com Maior Venda",
             value = loja_maior_venda,
             color = "green",
             icon = icon("store")
         )
     })
     
     # Gráfico de Barras
     output$plot_bar <- renderPlot({
         dados_temporais <- dados_filtrados()
         p <- ggplot(dados_temporais, aes(x = Loja, y = Vendas, fill = Loja)) +
             geom_bar(stat = "identity") +
             labs(x = "Loja", y = "Vendas") +
             theme_minimal() +
             theme(legend.position = "none") +
             geom_text(aes(label = Vendas), vjust = -0.5)
         p
     })
     
     # Mapa de Vendas
     output$map <- renderLeaflet({
         dados_temporais <- dados_filtrados()
         mapa <- leaflet() %>%
             addTiles() %>%
             addCircleMarkers(
                 data = dados_temporais,
                 lng = ~Longitude,
                 lat = ~Latitude,
                 popup = ~paste("Loja: ", Loja, "<br>Vendas: ", Vendas),
                 label = ~paste("Loja: ", Loja, "<br>Vendas: ", Vendas)
             )
         mapa
     })
     
     # Análise Temporal
     output$plot_temporal_kpi <- renderPlot({
         dados_temporais <- dados_filtrados()
         p <- ggplot(dados_temporais, aes(x = Data, y = Vendas, group = Loja, color = Loja)) +
             geom_line() +
             labs(x = "Data", y = "Vendas") +
             theme_minimal() +
             theme(legend.position = "none") +
             geom_text(
                 data = dados_temporais %>% group_by(Loja) %>% summarize(Data = max(Data), Vendas = sum(Vendas)),
                 aes(label = Loja), hjust = -0.2, size = 3
             )
         p
     })
     
     # Vendas por Cidade
     output$plot_vendas_cidade <- renderPlot({
         dados_temporais <- dados_filtrados()
         p <- ggplot(dados_temporais, aes(x = Cidade, y = Vendas, fill = Loja)) +
             geom_bar(stat = "identity", position = "dodge") +
             labs(x = "Cidade", y = "Vendas") +
             theme_minimal() +
             theme(legend.position = "none") +
             geom_text(aes(label = Vendas), vjust = -0.5)
         p
     })
     
     # Vendas por País
     output$plot_vendas_pais <- renderPlot({
         dados_temporais <- dados_filtrados()
         p <- ggplot(dados_temporais, aes(x = Pais, y = Vendas, fill = Loja)) +
             geom_bar(stat = "identity", position = "dodge") +
             labs(x = "País", y = "Vendas") +
             theme_minimal() +
             theme(legend.position = "none") +
             geom_text(aes(label = Vendas), vjust = -0.5)
         p
     })
     
     # Tabela Dinâmica
     output$tabela_dinamica <- renderDT({
         dados_temporais <- dados_filtrados() %>%
             group_by(Loja, Cidade, Estado, Pais) %>%
             summarise(
                 Vendas = sum(Vendas),
                 Media_Vendas = mean(Vendas)
             )
         datatable(dados_temporais)
     })
 }
 
 # Executar o aplicativo Shiny
 shinyApp(ui, server)
