library(shiny)
library(tidyverse)
library(sf)
library(geobr)
library(leaflet)
library(DT)
library(readxl)
library(bslib)

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
# 2. INTERFACE (UI) - Agora com a aba de Metodologia!
# ==============================================================================
ui <- page_sidebar(
  title = "Simulador de Risco: Ferrugem Asiática (Safra 24/25)",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  
  sidebar = sidebar(
    title = "Painel de Controle",
    
    radioButtons(
      inputId = "cenario_plantio",
      label = "Época de Semeadura (Janela Crítica):",
      choices = c(
        "Plantio do Cedo (Outubro)" = "cedo",
        "Plantio Normal (Novembro)" = "normal",
        "Plantio Tardio (Dez/Jan)" = "tardio"
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
    
    helpText("A modelagem matemática de severidade segue a equação polinomial oficial do esquema metodológico, ajustada para a época de semeadura escolhida.")
  ),
  
  navset_card_underline(
    nav_panel("Mapa de Risco", leafletOutput("mapa_interativo", height = "650px")),
    nav_panel("Dados e Perdas", DTOutput("tabela_ranking")),
    
    # === A NOVA ABA DE METODOLOGIA E AVISOS ===
    nav_panel(
      "Metodologia e Avisos",
      card(
        card_header("Como este Simulador Dinâmico Funciona?"),
        markdown("
        **1. Fontes de Dados e Reprodutibilidade:**
        * **Produção:** API do IBGE/SIDRA (Tabela 5457). A Produtividade Atingível foi estabelecida usando o rendimento máximo de cada município nos últimos 5 anos.
        * **Clima:** API da NASA POWER (Precipitação Diária Ajustada) cruzada via *Spatial Join* com as coordenadas geográficas do RS.
        
        **2. A Engrenagem Matemática:**
        A severidade da doença na lavoura segue a modelagem empírica baseada na precipitação acumulada na janela crítica. Aplicamos a equação de regressão calibrada para o modelo:
        * `Severidade (%) = -3.3983 + 0.3777(P) - 0.0003(P²)` *(Onde P = chuva acumulada no cenário selecionado).*
        * `Score (0 a 100):` A vulnerabilidade bruta (Severidade x Dano x Produção) foi submetida a uma normalização *Min-Max* para gerar um índice gerencial limpo.
        
        **3. Avisos Estratégicos (Limitações Assumidas):**
        * **Risco Inerente Bruto:** O modelo não contabiliza a eficácia do controle químico (uso de fungicidas) pelo produtor ou a adoção de cultivares resistentes. O painel projeta o **pior cenário ambiental (risco puro)**, ideal para precificação de apólices e auditoria de campo.
        * **Proxy Climático:** Em conformidade com o referencial adotado, o algoritmo isola o volume pluviométrico como a variável independente principal (*proxy*) para estimar a favorabilidade da doença em larga escala, abstraindo variáveis de microclima (como orvalho).
        ")
      )
    )
  )
)

# ==============================================================================
# 3. SERVIDOR (Mantido idêntico, com a fórmula matemática do edital)
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
        
        # Fórmula do monitor
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

# ==============================================================================
# 4. INICIAR O APLICATIVO
# ==============================================================================
shinyApp(ui, server)