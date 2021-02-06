library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets) # Para usar umas botões mais diferentões;
library(fresh) # Alterar as cores do Dash;
library(xaringanthemer) # Para alterar as letras dos gráficos; 
library(here) # Possui a função here()
library(plotly)

# Carregando funções ----
source(here("Graficos.R"))

# Aparência do Dashboard ---- 

theme_set(theme_minimal())

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#ffffff", # Alterando tudo que recebe a cor "light_blue", o que inclui a header;
    teal = "#4FB6A7" # Alterando tudo que recebeu a cor "teal;
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#283a4e", # Muda a cor de fundo do sidebar,
    dark_hover_bg = "#57b952", # Faz com que o item selecionado seja verde;
    dark_color = "#ffffff" # Cor das letras
  ),
  adminlte_global(
    content_bg = "#d1d9d9", # Altera a cor do plano de fundo
    info_box_bg = "#D8DEE9"
  )
)

# Dashboard ---- 

header <- dashboardHeader(disable = T) # Fechamento função dashboardHeader()

sidebar <- dashboardSidebar(
  
  sidebarMenu( # Menu no Sidebar ----
               menuItem("Análises Descritivas", tabName = "descritivas", icon = icon("dashboard")),
               menuItem("Testes Estatísticos", icon = icon("instagram"), tabName = "testes"),
               menuItem("Sobre o Banco", icon = icon("linkedin"), tabName = "banco"),
               tags$img(src="https://operdata.com.br/wp-content/uploads/2019/07/logo_light-185x156.png",
                        margin = "center",
                        width = "60%")
  ) # Fechamento função sidebarMenu()
  
) # Fechamento função dashboardSidebar()

body <- dashboardBody(
  
  use_theme(mytheme), # Usando o tema que eu criei;
  
  tabItems( # Criando diferentes páginas para cada item do Menu;
    
    tabItem(tabName = "descritivas", 
            
          navbarPage(title = "", 
            
            tabPanel("Principais", # Primeira aba com descritivas ---- 
                     
                     fluidRow( # ValueBoxes com as médias
                       box( 
                         title = "Visitantes - Instagram", width = 4, 
                         height = 120, "Algo escrito"
                       ),
                       
                       box( 
                         title = "Visualizações - LinkedIn", width = 4,
                         height = 120, "Algo escrito"
                       ),
                       
                       box( 
                         title = "Usuários do Site", width = 4,
                         height = 120, "Algo escrito")
                       
                       ), # Fechamento fluidRow()
                     
                     fluidRow( 
                       box( 
                         title = "Visitantes - Instagram", width = 6, 
                         height = 430, 
                         plotOutput("pizza")
                       ),
                       
                       box( 
                         title = "Visualizações - LinkedIn", width = 6,
                         height = 430, 
                         plotOutput("histograma")
                       )
                       
                     ) # Fechamento fluidRow()
                     
                     ) , # Fechamento tabPanel
                       
            tabPanel("Comparações", # Segunda Aba com descritivas ----
                     
                     fluidRow(  # Gráficos das Variáveis Categóricas ---- 
                       
                       box(
                            width = 12,
                            height = 100,
                            radioGroupButtons(
                              inputId = "var",
                              label = "Escolha a Variável",
                              choices = c("Cargo", "Idade", "Performance", "Titulo",
                                          "Departamento", "Senioridade"),
                              justified = TRUE # Seletor
                            
                       ) # Fechamento do box
                       
                     ), # Fechamento fluidRow() 
                     
                     fluidRow(  # Output das descritivas 2 ----
                       
                       box(width = 6,
                           height = 300, 
                           plotlyOutput("simples", width = "500px", height = "230px")),
                       
                       box(width = 6,
                           height = 300,
                           plotlyOutput("proporcao", width = "500px", height = "230px"))
                       
                       ), # Fechamento fluidRow()
                     
                     fluidRow(
                       
                       box(width = 6,
                           height = 300 ),
                       
                       box(width = 6,
                           height = 300)
                       
                     ) # Fechamento fluidRow()
                     
                     ) # Fechamento tabPanel
                     
            ) # Fechamento navbarPage()
            
            ), # Fechamento função tabIten(Geral)
    
    ),  # Fechamento do primeiro tabItem;
    
    tabItem(tabName = "testes", # Segunda Aba ----
            
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
    
    tabItem(tabName = "banco", # Sobre o Banco ----
            
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
  
  output$pizza <- renderPlot({ # Gráfico de Pizza ----
    pizza()
  })
  
  output$histograma <- renderPlot({ # Gráfico de Histograma ----
    salarios()
  })
  
   output$simples <- renderPlotly({ # Gráfico Univariado ----
     univar <- univar(input$var)
     ggplotly(univar)
   })
   
   output$proporcao <- renderPlotly({ # Gráficos com proporção ----
     prop <- prop(input$var)
     ggplotly(prop)
   })
   
   }

shinyApp(ui, server)












