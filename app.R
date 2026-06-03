library(shiny)
library(tidyverse)
library(sf)
library(geobr)
library(leaflet)
library(DT)
library(readxl)
library(bslib)
library(bsicons)
library(plotly)

# ==============================================================================
# 1. PREPARAÇÃO (Carrega os dados)
# ==============================================================================
dados_cenarios <- read_xlsx("base_cenarios_rs.xlsx") %>%
  mutate(codigo_ibge = as.character(codigo_ibge))

historico_clima <- read_delim("dados_climaticos_rs_safra_24_25.csv", delim = ";", show_col_types = FALSE) %>%
  mutate(
    codigo_ibge = as.character(codigo_ibge),
    precipitacao = as.numeric(str_replace_all(precipitacao, ",", "."))
  )

malha_rs <- read_municipality(code_muni = "RS", year = 2022, showProgress = FALSE) %>%
  mutate(code_muni = as.character(code_muni))

mapa_dados <- malha_rs %>%
  inner_join(dados_cenarios, by = c("code_muni" = "codigo_ibge"))

# ==============================================================================
# 2. INTERFACE (UI)
# ==============================================================================
ui <- page_sidebar(
  title = "Simulador Dinâmico de Risco: Ferrugem da Soja",
  theme = bs_theme(version = 5, bootswatch = "litera", primary = "#2E7D32", warning = "#FF9800", danger = "#D32F2F"),
  
  tags$head(
    tags$style(HTML("
      .bslib-value-box { min-height: 0 !important; height: auto !important; margin-bottom: 10px !important; }
      .bslib-value-box .card-body { padding: 5px 15px !important; }
      .bslib-value-box .value-box-title { font-family: inherit !important; font-size: 0.8rem !important; font-weight: bold !important; text-transform: uppercase; margin-bottom: 2px !important; }
      .bslib-value-box .value-box-value { font-family: inherit !important; font-size: 1.3rem !important; }
      .bslib-value-box svg { width: 1.8rem !important; height: 1.8rem !important; opacity: 0.7; }
    "))
  ),
  
  sidebar = sidebar(
    title = "Painel de Controle",
    width = 350,
    
    radioButtons(
      inputId = "cenario_plantio",
      label = "Época de Semeadura (Janela Crítica):",
      choices = c(
        "Semeadura Precoce (Outubro)" = "cedo",
        "Semeadura Normal (Novembro)" = "normal",
        "Semeadura Tardia (Dez/Jan)" = "tardio"
      ),
      selected = "normal"
    ),
    hr(),
    
    sliderInput("filtro_pareto", "Relevância Produtiva (Top % do Estado):", min = 10, max = 100, value = 100, step = 5, post = "%"),
    sliderInput("filtro_score", "Score Mínimo de Vulnerabilidade:", min = 0, max = 100, value = 0, step = 5),
    
    helpText("A modelagem matemática de severidade segue a equação empírica oficial.")
  ),
  
  navset_card_underline(
    nav_panel(
      "Mapa de Risco", 
      layout_columns(
        fill = FALSE,
        value_box(title = "Municípios Selecionados", value = textOutput("vb_cidades"), theme = "primary", showcase = bs_icon("geo-alt")),
        value_box(title = "Perda Máxima (kg/ha)", value = textOutput("vb_perda_max"), theme = "danger", showcase = bs_icon("graph-down-arrow")),
        value_box(title = "Score Médio (0-100)", value = textOutput("vb_score_medio"), theme = "warning", showcase = bs_icon("speedometer2"))
      ),
      leafletOutput("mapa_interativo", height = "500px"),
      helpText(bs_icon("info-circle"), " Dica: Clique em um município no mapa para carregar o seu histórico climático detalhado.")
    ),
    
    nav_panel("Dados e Perdas", DTOutput("tabela_ranking")),
    
    nav_panel(
      "Série Histórica Climática", 
      card(
        card_header("Análise Meteorológica Diária"),
        card_body(
          # NOVA BARRA DE PESQUISA DIRETO NA ABA!
          selectizeInput(
            inputId = "seletor_municipio",
            label = "Selecione ou digite o nome de um município:",
            choices = NULL, # Será preenchido pelo servidor
            width = "100%"
          ),
          plotlyOutput("grafico_linha_tempo", height = "400px")
        )
      )
    ),
    
    nav_panel(
      "Metodologia e Avisos",
      card(
        card_header("Como este Simulador Dinâmico Funciona?"),
        markdown("
        **1. Fontes de Dados e Reprodutibilidade:**
        * **Produção:** API do IBGE/SIDRA (Tabela 5457). Produtividade Atingível estabelecida via rendimento máximo de 5 anos.
        * **Clima:** API da NASA POWER cruzada com malha territorial IPEA/geobr.
        
        **2. Engrenagem Matemática:**
        A estimativa da severidade baseia-se no **modelo empírico de regressão não-linear** ajustado por **Del Ponte et al. (2006, Tabela 3, BR3)**:
        * `Severidade (%) = -3.3983 + 0.3777(P) - 0.0003(P²)` *(Onde P = precipitação acumulada na janela selecionada).*
        * O *Score* final de vulnerabilidade aplica normalização estatística *Min-Max* sobre a perda produtiva bruta.
        
        **3. Limitações Assumidas:**
        * O simulador projeta o risco ambiental intrínseco (pior cenário climático), assumindo ausência de controle químico (fungicidas) e adoção de cultivares padrão.
        ")
      )
    )
  )
)

# ==============================================================================
# 3. SERVIDOR (A Mágica da Integração)
# ==============================================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # PREENCHE A NOVA BARRA DE PESQUISA COM AS CIDADES DO RS
  # ----------------------------------------------------------------------------
  observe({
    lista_opcoes <- mapa_dados %>% 
      st_drop_geometry() %>% 
      select(municipio, code_muni) %>% 
      distinct() %>% 
      arrange(municipio)
    
    nomes_valores <- setNames(lista_opcoes$code_muni, lista_opcoes$municipio)
    
    updateSelectizeInput(session, "seletor_municipio", choices = nomes_valores, selected = nomes_valores[1])
  })
  
  # ----------------------------------------------------------------------------
  # INTEGRAÇÃO: O CLIQUE NO MAPA ATUALIZA A BARRA DE PESQUISA
  # ----------------------------------------------------------------------------
  observeEvent(input$mapa_interativo_shape_click, {
    clique <- input$mapa_interativo_shape_click
    updateSelectizeInput(session, "seletor_municipio", selected = clique$id)
  })
  
  # ----------------------------------------------------------------------------
  # CÁLCULOS DO MAPA E TABELA
  # ----------------------------------------------------------------------------
  dados_calculados <- reactive({
    mapa_dados %>%
      mutate(
        chuva_selecionada = case_when(
          input$cenario_plantio == "cedo" ~ chuva_cedo,
          input$cenario_plantio == "normal" ~ chuva_normal,
          input$cenario_plantio == "tardio" ~ chuva_tardio
        ),
        severidade_estimada_pct = -3.3983 + (0.3777 * chuva_selecionada) - (0.0003 * (chuva_selecionada^2)),
        severidade_estimada_pct = case_when(severidade_estimada_pct < 0 ~ 0, severidade_estimada_pct > 100 ~ 100, TRUE ~ severidade_estimada_pct),
        risco_relativo = severidade_estimada_pct / 100,
        potencial_perda_kgha = severidade_estimada_pct * 0.006 * produtividade_atingivel_kgha,
        vulnerabilidade_bruta = risco_relativo * producao_media_ton * potencial_perda_kgha,
        score_vulnerabilidade = case_when(
          max(vulnerabilidade_bruta, na.rm = TRUE) == min(vulnerabilidade_bruta, na.rm = TRUE) ~ 0,
          TRUE ~ (vulnerabilidade_bruta - min(vulnerabilidade_bruta, na.rm = TRUE)) /
            (max(vulnerabilidade_bruta, na.rm = TRUE) - min(vulnerabilidade_bruta, na.rm = TRUE)) * 100
        )
      ) %>%
      filter(pct_pareto <= input$filtro_pareto, score_vulnerabilidade >= input$filtro_score)
  })
  
  output$vb_cidades <- renderText({ nrow(dados_calculados()) })
  output$vb_perda_max <- renderText({ 
    if(nrow(dados_calculados()) == 0) return("0 kg")
    paste0(round(max(dados_calculados()$potencial_perda_kgha, na.rm = TRUE), 0), " kg")
  })
  output$vb_score_medio <- renderText({ 
    if(nrow(dados_calculados()) == 0) return("0")
    round(mean(dados_calculados()$score_vulnerabilidade, na.rm = TRUE), 1)
  })
  
  output$mapa_interativo <- renderLeaflet({
    dados <- dados_calculados()
    if(nrow(dados) == 0) return(leaflet() %>% addTiles() %>% setView(lng = -53.5, lat = -29.5, zoom = 6))
    paleta <- colorNumeric("YlOrRd", domain = c(0, 100), na.color = "transparent")
    
    leaflet(dados) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~code_muni, 
        fillColor = ~paleta(score_vulnerabilidade), weight = 1, opacity = 1, color = "white",
        fillOpacity = 0.75,
        label = ~paste0(municipio, " | Score: ", round(score_vulnerabilidade, 1), 
                        " | Perda: ", round(potencial_perda_kgha, 1), " kg/ha"),
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend(pal = paleta, values = c(0, 100), opacity = 0.7, title = "Vulnerabilidade", position = "bottomright")
  })
  
  # ----------------------------------------------------------------------------
  # GRÁFICO DIRECIONADO PELA BARRA DE PESQUISA (QUE É ATUALIZADA PELO MAPA)
  # ----------------------------------------------------------------------------
  output$grafico_linha_tempo <- renderPlotly({
    req(input$seletor_municipio) # Garante que só desenha se tiver município selecionado
    id <- input$seletor_municipio
    
    dados_grafico <- historico_clima %>% filter(codigo_ibge == id)
    nome <- mapa_dados %>% filter(code_muni == id) %>% pull(municipio) %>% .[1]
    
    plot_ly(dados_grafico, x = ~data, y = ~precipitacao, type = 'scatter', mode = 'lines',
            line = list(color = '#2E7D32', width = 2),
            hoverinfo = 'text',
            text = ~paste("Data: ", format(as.Date(data), "%d/%m/%Y"), "<br>Precipitação: ", precipitacao, " mm")) %>%
      layout(
        title = paste("Precipitação Diária (Safra 24/25) -", nome),
        xaxis = list(title = "Data da Medição"),
        yaxis = list(title = "Precipitação (mm)")
      )
  })
  
  output$tabela_ranking <- renderDT({
    dados_calculados() %>%
      st_drop_geometry() %>%
      select(Município = municipio, `Grupo Pareto (%)` = pct_pareto, 
             `Chuva (mm)` = chuva_selecionada, `Severidade (%)` = severidade_estimada_pct, 
             `Score Vuln.` = score_vulnerabilidade, `Perda (kg/ha)` = potencial_perda_kgha) %>%
      arrange(desc(`Score Vuln.`)) %>%
      mutate(across(where(is.numeric), ~round(.x, 1))) %>%
      datatable(options = list(pageLength = 15, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')))
  })
}

shinyApp(ui, server)