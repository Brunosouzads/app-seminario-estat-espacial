# Instale e carregue os pacotes necessários
#install.packages(c("shiny", "sf", "dplyr", "readxl", "openxlsx", "leaflet", "RColorBrewer", "viridis"))
library(shiny)
library(sf)
library(dplyr)
library(readxl)
library(openxlsx)
library(leaflet)
library(viridis)
library(stringr)
library(geobr)

# Lê os dados do mapa e das variáveis de interesse
dados <- st_read("RS_Municipios_2022.shp")
dados1 <- read.xlsx("dadosrs.xlsx", rows = 3:500)

# Normaliza os nomes dos municípios
colnames(dados1) <- c(
  "municipio",                 
  "Codigo",                    
  "gentilico",                 
  "prefeito",                  
  "area_territorial_km2",      
  "populacao_residente",       
  "densidade_demografica",     
  "escolarizacao_6a14",        
  "IDH",                       
  "mortalidade_infantil",      
  "receitas_brutas",           
  "despesas_brutas",           
  "PIB_per_capita"             
)


dados1$municipio <- tolower(trimws(dados1$municipio))
dados$municipio <- tolower(trimws(dados$NM_MUN)) 
dados$municipio <- str_to_title(dados$municipio) 
dados1$municipio <- str_to_title(dados1$municipio) 

# Transforma para WGS84 se necessário
if (st_crs(dados)$epsg != 4326) {
  dados <- st_transform(dados, crs = 4326)
}

#Juntar bancos e Ajeitar Variaveis
sf_data_joined <- dados %>%
  left_join(dados1, by = "municipio") %>%
  slice(-c(1, 2))




ui<-fluidPage(
  titlePanel("Dados sobre a População do estado do RS (IBGE 2021)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("map_type", "Escolha o tipo de mapa:",
                  choices = c("PIB per Capita" = "PIB", 
                              "IDH" = "IDH",
                              "População" = "População",
                              "Densidade Demográfica" = "Densidade",
                              "Escolarização dos 6 aos 14 anos" = "Escolarizacao")),
      sliderInput('pib','PIB per capita',min = 0, max = 450000, value = c(0,450000)),
      sliderInput('idh','IDH',min = 0, max = 0.9, value = c(0,0.9)),
      sliderInput('pop','População',min = 1000, max = 140000, value = c(1000,140000)),
      sliderInput('ddemo','Densidade Demográfica',min = 1, max = 3200, value = c(1,3200)),
      sliderInput('escol','Escolarização 6 - 14',min = 80, max = 100, value = c(80,100)),
      actionButton("Atualizar", "Atualizar", class = "btn btn-primary")
    ),
    mainPanel(
      leafletOutput('plot',width = "100%", height = 800)
    )
  )
)

# Categoriza PIB per capita
sf_data_joined$PIB_per_capita_cat <- cut(
  sf_data_joined$PIB_per_capita,
  breaks = c(-Inf, 10000, 20000, 50000, 100000, 200000, Inf), 
  labels = c("< 10k", "10k - 20k", "20k - 50k", "50k - 100k", "100k - 200k", "> 200k")
)


#Paletas de Cada tipo de Mapa

custom_palette <- c("#440154", "#31688e", "#35b779", "#fdf425", "#D95F0E")
pal_PIB_cat <- colorFactor(palette = custom_palette, domain = sf_data_joined$PIB_per_capita_cat)

sf_data_joined$IDH <- as.numeric(sf_data_joined$IDH)
pal_IDH <- colorNumeric(palette = custom_palette, domain = na.omit(sf_data_joined$IDH))
pal_area <- colorNumeric(palette = custom_palette, domain = sf_data_joined$area_territorial_km2)
pal_pop <- colorNumeric(palette = custom_palette, domain = sf_data_joined$populacao_residente)
pal_densidade <- colorNumeric(palette = custom_palette, domain = sf_data_joined$densidade_demografica)

sf_data_joined$escolarizacao_6a14 <- as.numeric(sf_data_joined$escolarizacao_6a14)
pal_escolarizacao <- colorNumeric(palette = custom_palette, domain = na.omit(sf_data_joined$escolarizacao_6a14))

sf_data_joined$mortalidade_infantil <- as.numeric(sf_data_joined$mortalidade_infantil)
pal_mortalidade <- colorNumeric(palette = "Reds", domain = na.omit(sf_data_joined$mortalidade_infantil))


server <- function(input, output) {
  observeEvent(input$Atualizar, {
  output$plot <-renderLeaflet({
    final <- reactive({sf_data_joined %>%
      subset(PIB_per_capita >= input$pib[1] & PIB_per_capita <= input$pib[2] & 
                      IDH >= input$idh[1] & IDH <= input$idh[2] &
                      populacao_residente >= input$pop[1] & populacao_residente <= input$pop[2] &
                      densidade_demografica >= input$ddemo[1] & densidade_demografica <= input$ddemo[2] &
                      escolarizacao_6a14 >= input$escol[1] & escolarizacao_6a14 <= input$escol[2]
    )
      })
    
    
    
    var_name <- input$map_type  # Obtém o nome da variável em minúsculas
    
    # Definindo a variável e a paleta com base na seleção
    if (var_name == "PIB") {
      pal <- pal_PIB_cat
      label_var <- sf_data_joined$PIB_per_capita_cat
    } else if (var_name == "População") {
      pal <- pal_pop
      label_var <- sf_data_joined$populacao_residente
    } else if (var_name == "Densidade") {
      pal <- pal_densidade
      label_var <- sf_data_joined$densidade_demografica
    } else {
      pal <- pal_IDH
      label_var <- sf_data_joined$IDH
    }
    p <- final() %>%
      leaflet() %>% 
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(label_var),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 2, color = "white", fillOpacity = 0.7),
        label = sprintf("Município: %s PIB per Capita: %s \nIDH: %s \nPopulação: %s \nDensidade Demográfica: %s", 
                        sf_data_joined$municipio, sf_data_joined$PIB_per_capita, sf_data_joined$IDH, 
                        sf_data_joined$populacao_residente, sf_data_joined$densidade_demografica)) %>%
      addLegend(pal = pal, 
                values = ~label_var, 
                title = input$map_type, 
                position = "bottomright")
    p
  })
  })
}

shinyApp(ui = ui, server = server)