library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets) # Para usar umas botões mais diferentões;
library(fresh) # Alterar as cores do Dash;
library(xaringanthemer) # Para alterar as letras dos gráficos; 
library(here) # Possui a função here()
library(plotly)

# Carregando funções ----


# Aparência do Dashboard ---- 

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#ffffff", # Alterando tudo que recebe a cor "light_blue", o que inclui a header;
    teal = "#4FB6A7" # Alterando tudo que recebeu a cor "teal;
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#ffffff", # Deixa o sidebar branco,
    dark_hover_bg = "#4FB6A7", # Faz com que o item selecionado seja verde;
    dark_color = "#652177"
  )
)

# Dashboard ---- 

header <- dashboardHeader(title = "Depois Imagem"
  
  # title = tags$img(src="https://i.ibb.co/9VZRpG5/icon-dash-sem-fundo.png", 
  #                  width = '110%')
  
) # Fechamento função dashboardHeader()

sidebar <- dashboardSidebar(
  
  sidebarMenu( # Menu no Sidebar ----
               menuItem("Análises Descritivas", tabName = "descritivas", icon = icon("dashboard")),
               menuItem("Testes Estatísticos", icon = icon("instagram"), tabName = "testes"),
               menuItem("Sobre o Banco", icon = icon("linkedin"), tabName = "banco")
  ) # Fechamento função sidebarMenu()
  
) # Fechamento função dashboardSidebar()

body <- dashboardBody(
  
  use_theme(mytheme), # Usando o tema que eu criei;
  
  tabItems( # Criando diferentes páginas para cada item do Menu;
    
    tabItem(tabName = "descritivas", 
            
            fluidRow( # Adicionando a primeira linha de gráficos
              box( # Gráfico OKR - Instagram ----
                   title = "Visitantes - Instagram", width = 3, 
                   height = 170, "Algo escrito"
              ),
              
              box( # Gráfico OKR - LinkedIn ----
                   title = "Visualizações - LinkedIn", width = 3,
                   height = 170, "Algo escrito"
              ),
              
              box( # Gráfico OKR - Site ----
                   title = "Usuários do Site", width = 3,
                   height = 170, "Algo escrito"
              ) ,
              
              box( # Gráfico OKR - Site ----
                   title = "Usuários do Site", width = 3,
                   height = 170, "Algo escrito"
              )
              
            ), # Fechamento fluidRow()
            
            fluidRow(
              
              box( # Gráfico dos quadradinhos ---- 
                   width = 12,
                   height = 370,
                   radioGroupButtons(
                     inputId = "var",
                     label = "Escolha a Variável",
                     choices = c("Emprego", "Gênero", "Idade", "Performance", "Título",
                                 "Departamento", "Senioridade"),
                     justified = TRUE), # Seletor
                   
              ) # Fechamento do box
              
            ) # Fechamento fluidRow()
            
    ), # Fechamento função tabIten(Geral)
    
    tabItem(tabName = "instagram",
            
            fluidRow(
              
              column(width = 9,
                     
                     box(title = "Métricas Gerais",
                         width = NULL,
                         height = 270,
                         actionBttn("teste", "teste")),
                     
                     box(title = "Tipo de Posts",
                         width = NULL,
                         height = 270,
                         "Box content")
                     
              ), # Fechamento primeira coluna;
              
              column(width = 3,
                     
                     valueBox(width = 30,
                              "Aumento Seguidores", value = "n", icon = icon("chart-bar"),
                              color = "teal"),
                     
                     valueBox(width = 30,
                              "Média de Cutidas", value = "n", icon = icon("heart"),
                              color = "teal"),
                     
                     valueBox(width = 30,
                              "Média de Compartilhamentos", value = "n", icon = icon("paper-plane"),
                              color = "teal"),
                     
                     valueBox(width = 30,
                              "Média de Salvamentos", value = "n", icon = icon("bookmark"),
                              color = "teal"),
                     
                     valueBox(width = 30,
                              "Alcance Médio", value = "n", icon = icon("project-diagram"),
                              color = "teal")
                     
                     # valueBox(width = 30,
                     #         "Média de Visitas", value = "n", icon = icon("chart-bar"),
                     #         color = "teal"),
                     
              ) # Fechamento segunda coluna;
              
            ) # Fechamento fluidRow()
            
    ), # Fechamento função tabIten(Instagram)
    
    tabItem(tabName = "linkedin",
            
            fluidRow(
              
              column(width = 9,
                     
                     box(title = "Métricas Gerais",
                         width = NULL,
                         height = 270,
                         actionBttn("teste", "teste")),
                     
                     box(title = "Tipo de Posts",
                         width = NULL,
                         height = 270,
                         "Box content")
                     
              ), # Fechamento primeira coluna;
              
              column(width = 3,
                     
                     valueBox(width = 30,
                              "Aumento Seguidores", value = "n", icon = icon("chart-bar"),
                              color = "teal"),
                     
                     valueBox(width = 30,
                              "Média de Cutidas", value = "n", icon = icon("heart"),
                              color = "teal"),
                     
                     valueBox(width = 30,
                              "Média de Compartilhamentos", value = "n", icon = icon("paper-plane"),
                              color = "teal"),
                     
                     valueBox(width = 30,
                              "Média de Salvamentos", value = "n", icon = icon("bookmark"),
                              color = "teal"),
                     
                     valueBox(width = 30,
                              "Alcance Médio", value = "n", icon = icon("project-diagram"),
                              color = "teal")
                     
                     # valueBox(width = 30,
                     #         "Média de Visitas", value = "n", icon = icon("chart-bar"),
                     #         color = "teal"),
                     
              ) # Fechamento segunda coluna;
              
            ) # Fechamento fluidRow()
            
    ) # Fechamento função tabIten(LinkedIn)
    
  ) # Fechamento função tabItens()
  
) # Fechamento função dashboardBody()

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  # output$simples <- 
   }

shinyApp(ui, server)












