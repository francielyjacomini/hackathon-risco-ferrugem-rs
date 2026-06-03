library(shiny)
library(tidyverse)
library(sf)
library(geobr)
library(leaflet)
library(DT)
library(readxl)
library(bslib)
library(bsicons)

# ==============================================================================
# 1. PREPARAÇÃO (Carrega os dados brutos de cenários)
# ==============================================================================
dados_cenarios <- read_xlsx("base_cenarios_rs.xlsx") %>%
  mutate(codigo_ibge = as.character(codigo_ibge))

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
  
  # CSS para encolher os cartões (Value Boxes)
  tags$head(
    tags$style(HTML("
      /* Remove a altura mínima do contentor principal do cartão */
      .bslib-value-box {
        min-height: 0 !important;
        height: auto !important;
        margin-bottom: 10px !important; /* Aproxima os cartões do mapa */
      }
      /* O verdadeiro vilão: reduz o espaçamento interno (padding) do cartão */
      .bslib-value-box .card-body {
        padding: 5px 15px !important; 
      }
      /* Ajuste da fonte do Título */
      .bslib-value-box .value-box-title {
        font-family: inherit !important;
        font-size: 0.8rem !important;
        font-weight: bold !important;
        text-transform: uppercase;
        margin-bottom: 2px !important;
      }
      /* Ajuste da fonte do Valor numérico */
      .bslib-value-box .value-box-value {
        font-family: inherit !important;
        font-size: 1.3rem !important;
      }
      /* Ajuste do Ícone */
      .bslib-value-box svg {
        width: 1.8rem !important;
        height: 1.8rem !important;
        opacity: 0.7;
      }
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
    
    sliderInput(
      inputId = "filtro_pareto", 
      label = "Relevância Produtiva (Top % do Estado):", 
      min = 10, max = 100, value = 100, step = 5, post = "%"
    ),
    
    sliderInput(
      inputId = "filtro_score", 
      label = "Score Mínimo de Vulnerabilidade:", 
      min = 0, max = 100, value = 0, step = 5
    ),
    
    helpText("A modelagem matemática de severidade segue a equação de regressão oficial do esquema metodológico, ajustada para a época de semeadura escolhida.")
  ),
  
  navset_card_underline(
    nav_panel(
      "Mapa de Risco", 
      
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Municípios Selecionados", 
          value = textOutput("vb_cidades"), 
          theme = "primary", 
          showcase = bs_icon("geo-alt")
        ),
        value_box(
          title = "Perda Máxima (kg/ha)", 
          value = textOutput("vb_perda_max"), 
          theme = "danger", 
          showcase = bs_icon("graph-down-arrow")
        ),
        value_box(
          title = "Score Médio (0-100)", 
          value = textOutput("vb_score_medio"), 
          theme = "warning", 
          showcase = bs_icon("speedometer2")
        )
      ),
      
      leafletOutput("mapa_interativo", height = "550px")
    ),
    
    nav_panel("Dados e Perdas", DTOutput("tabela_ranking")),
    
    nav_panel(
      "Metodologia e Avisos",
      card(
        card_header("Como este Simulador Dinâmico Funciona?"),
        markdown("
        **1. Fontes de Dados e Reprodutibilidade:**
        * **Produção:** API do IBGE/SIDRA (Tabela 5457). A Produtividade Atingível foi estabelecida usando o rendimento máximo de cada município nos últimos 5 anos.
        * **Clima:** API da NASA POWER (Precipitação Diária Ajustada) cruzada via *Spatial Join* com as coordenadas geográficas do RS.
        
        **2. A Engrenagem Matemática:**
        A estimativa da severidade da doença na lavoura baseia-se nos **modelos empíricos de precipitação** propostos por **Del Ponte et al. (2006)**. Utilizamos o modelo de regressão não-linear (quadrático) cujos coeficientes de regressão foram extraídos da **Tabela 3 (modelo BR3)** do referido estudo:
        * `Severidade (%) = -3.3983 + 0.3777(P) - 0.0003(P²)` *(Onde P = chuva acumulada no cenário selecionado).*
        * `Score (0 a 100):` A vulnerabilidade bruta (Severidade x Dano x Produção) foi submetida a uma normalização *Min-Max* para gerar um índice gerencial limpo.
        
        **3. Avisos Estratégicos (Limitações Assumidas):**
        * **Risco Inerente Bruto:** O modelo não contabiliza a eficácia do controle químico (uso de fungicidas) pelo produtor ou a adoção de cultivares resistentes. O painel projeta o **pior cenário ambiental (risco puro)**, ideal para precificação de apólices e auditoria de campo.
        * **Proxy Climático:** Em conformidade com o referencial adotado, o algoritmo isola o volume pluviométrico como a variável independente principal (*proxy*) para estimar a favorabilidade da doença em larga escala, abstraindo variáveis de microclima (como o orvalho).
        ")
      )
    )
  )
)

# ==============================================================================
# 3. SERVIDOR (Cálculos matemáticos dinâmicos)
# ==============================================================================
server <- function(input, output, session) {
  
  dados_calculados <- reactive({
    mapa_dados %>%
      mutate(
        chuva_selecionada = case_when(
          input$cenario_plantio == "cedo" ~ chuva_cedo,
          input$cenario_plantio == "normal" ~ chuva_normal,
          input$cenario_plantio == "tardio" ~ chuva_tardio
        ),
        
        severidade_estimada_pct = -3.3983 + (0.3777 * chuva_selecionada) - (0.0003 * (chuva_selecionada^2)),
        
        severidade_estimada_pct = case_when(
          severidade_estimada_pct < 0 ~ 0,
          severidade_estimada_pct > 100 ~ 100,
          TRUE ~ severidade_estimada_pct
        ),
        
        risco_relativo = severidade_estimada_pct / 100,
        
        potencial_perda_kgha = severidade_estimada_pct * 0.006 * produtividade_atingivel_kgha,
        vulnerabilidade_bruta = risco_relativo * producao_media_ton * potencial_perda_kgha,
        
        score_vulnerabilidade = case_when(
          max(vulnerabilidade_bruta, na.rm = TRUE) == min(vulnerabilidade_bruta, na.rm = TRUE) ~ 0,
          TRUE ~ (vulnerabilidade_bruta - min(vulnerabilidade_bruta, na.rm = TRUE)) /
            (max(vulnerabilidade_bruta, na.rm = TRUE) - min(vulnerabilidade_bruta, na.rm = TRUE)) * 100
        )
      ) %>%
      filter(
        pct_pareto <= input$filtro_pareto,
        score_vulnerabilidade >= input$filtro_score
      )
  })
  
  output$vb_cidades <- renderText({ 
    nrow(dados_calculados()) 
  })
  
  output$vb_perda_max <- renderText({ 
    dados <- dados_calculados()
    if(nrow(dados) == 0) return("0 kg")
    paste0(round(max(dados$potencial_perda_kgha, na.rm = TRUE), 0), " kg")
  })
  
  output$vb_score_medio <- renderText({ 
    dados <- dados_calculados()
    if(nrow(dados) == 0) return("0")
    round(mean(dados$score_vulnerabilidade, na.rm = TRUE), 1)
  })
  
  output$mapa_interativo <- renderLeaflet({
    dados <- dados_calculados()
    if(nrow(dados) == 0) return(leaflet() %>% addTiles() %>% setView(lng = -53.5, lat = -29.5, zoom = 6))
    
    paleta <- colorNumeric("YlOrRd", domain = c(0, 100), na.color = "transparent")
    
    leaflet(dados) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~paleta(score_vulnerabilidade), weight = 1, opacity = 1, color = "white",
        fillOpacity = 0.75,
        label = ~paste0(municipio, " | Score: ", round(score_vulnerabilidade, 1), 
                        " | Perda/ha: ", round(potencial_perda_kgha, 1), " kg",
                        " | Sev: ", round(severidade_estimada_pct, 1), "%"),
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend(pal = paleta, values = c(0, 100), opacity = 0.7, title = "Vulnerabilidade", position = "bottomright")
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