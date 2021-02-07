library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets) # Para usar umas botões mais diferentões;
library(shinydashboardPlus) 
library(shinycssloaders)
library(fresh) # Alterar as cores do Dash; 
library(here) # Possui a função here()
library(plotly)
library(DT)

# Carregando funções ----
source(here("Graficos.R"))
source(here("Testes.R"))

# Aparência do Dashboard ---- 

theme_set(theme_minimal())

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#ffffff", # Alterando tudo que recebe a cor "light_blue", o que inclui a header;
    teal = "#57b952", # Alterando tudo que recebeu a cor "teal;
    aqua = "#283a4e",
    maroon = "#40873d"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#283a4e", # Muda a cor de fundo do sidebar,
    dark_hover_bg = "#57b952", # Faz com que o item selecionado seja verde;
    dark_color = "#ffffff" # Cor das letras
  ),
  adminlte_global(
    content_bg = "#d1d9d9" # Altera a cor do plano de fundo
  )
)

options(spinner.color = "#57b952", spinner.color.brackground = "#ffffff", type = 2)

# Dashboard ---- 

header <- dashboardHeader(disable = T) # Fechamento função dashboardHeader()

sidebar <- dashboardSidebar(
  
  sidebarMenu( # Menu no Sidebar ----
               menuItem("Análises Descritivas", tabName = "descritivas", icon = icon("chart-pie")),
               menuItem("Testes Estatísticos", icon = icon("percent"), tabName = "testes"),
               menuItem("Sobre o Banco", icon = icon("database"), tabName = "banco"),
               tags$img(src="https://operdata.com.br/wp-content/uploads/2019/07/logo_light-185x156.png",
                        Style="padding-right=25px",
                        width = "60%")
  ) # Fechamento função sidebarMenu()
  
) # Fechamento função dashboardSidebar()

body <- dashboardBody(
  
  use_theme(mytheme), # Usando o tema que eu criei;
  
  tabItems( # Criando diferentes páginas para cada item do Menu;
    
    tabItem(tabName = "descritivas", 
            
          navbarPage(title = "", 
            
            tabPanel("Principais", # Primeira aba com descritivas ---- 
                     
                     fluidRow( 
                       box( 
                         background = "aqua",
                         width = 3, 
                         height = 120, 
                         "A Glassdoor é uma empresa que emprega a mesma proporção de homens e mulheres? O gênero influencia na salário recebido pelos funcionários?"
                       ),
                       
                       valueBox(      # ValueBoxes com as médias
                         value = round(mean(dados$Total),2),
                         subtitle = "Média Salarial", 
                         icon = icon("dollar-sign"), color = "maroon",
                         width = 3
                       ),
                       
                       valueBox(      
                         value = round(mean(filter(dados, Genero == "Masculino")$Total),2),
                         subtitle = "Média Salarial Masculina", 
                         icon = icon("male"), color = "maroon",
                         width = 3
                       ),
                       
                       valueBox(      
                         value = round(mean(filter(dados, Genero == "Feminino")$Total),2),
                         subtitle = "Média Salarial Feminina", 
                         icon = icon("female"), color = "maroon",
                         width = 3)
                       
                       ), # Fechamento fluidRow()
                     
                     fluidRow( 
                       box( 
                          width = 4, 
                         height = 430, 
                         withSpinner(plotOutput("pizza"), type =2, color.background = "#ffffff")
                       ),
                       
                       box( 
                          width = 4,
                         height = 430, 
                         withSpinner(plotOutput("histograma"), type =2, color.background = "#ffffff")
                       ),
                       
                       box( 
                         width = 4,
                         height = 430, 
                         withSpinner(plotlyOutput("pontos"), type =2, color.background = "#ffffff")
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
                           withSpinner(plotlyOutput("simples", width = "500px", height = "230px"), type =2, color.background = "#ffffff")),
                       
                       box(width = 6,
                           height = 300,
                           withSpinner(plotlyOutput("proporcao", width = "500px", height = "230px"), type =2, color.background = "#ffffff"))
                       
                       ), # Fechamento fluidRow()
                     
                     fluidRow(
                       
                       box(width = 6,
                           height = 300,
                           withSpinner(plotOutput("boxplot", width = "500px", height = "230px"), type =2, color.background = "#ffffff")),
                       
                       box(width = 6,
                           height = 300)
                       
                     ) # Fechamento fluidRow()
                     
                     ) # Fechamento tabPanel
                     
            ) # Fechamento navbarPage()
            
            ), # Fechamento função tabIten(Geral)
    
    ),  # Fechamento do primeiro tabItem;
    
    tabItem(tabName = "testes", #  Testes Estatísticos ----
            
            fluidRow(
              
              column(width = 9,
                     
                     box(title = "",
                         width = 4,
                         height = 600,
                         DTOutput("associacao"))
                     
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
            
    ), # Fechamento função tabIten(testes)
    
    tabItem(tabName = "banco", # Sobre o Banco ----
            
            fluidRow(
                     
              box(title = "Métricas Gerais",
                  width = 6,
                  height = 270,
                  actionBttn("teste", "teste")),
                     
              widgetUserBox(
                title = "Laura Alexandria de Oliveria",
                subtitle = "Estudante - 6° período de Estatística",
                type = 2,
                src = "https://media-exp1.licdn.com/dms/image/C4E03AQEIYs8UPRUBtA/profile-displayphoto-shrink_200_200/0/1607697535388?e=1618444800&v=beta&t=d53JblrwITuE3kHHW2N6r95i-Kjear5rAcaWmcvYlSI",
                color = "aqua",
                "Some text here!",
                footer = "The footer here!"
              )
              
            ), # Fechamento fluidRow()
            
            fluidRow(
              
              box(title = "Métricas Gerais",
                  width = NULL,
                  height = 270,
                  DTOutput("bruto"))
              
            ) # Fechamento fluidRow()
            
    ) # Fechamento função tabIten(banco)
    
  ) # Fechamento função tabItens()
  
) # Fechamento função dashboardBody()

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  output$pizza <- renderPlot({ # Gráfico de Pizza ----
    pizza() 
  })
  
  output$histograma <- renderPlot({ #  Histograma ----
    salarios()
  })
  
  output$pontos <- renderPlotly({ # Gráfico de Pontos ----
    pontos <- pontos()
    ggplotly(pontos) %>% layout(legend = list(orientation = "h", x = 0.1, y = 0))
  })
  
   output$simples <- renderPlotly({ # Gráfico Univariado ----
     univar <- univar(input$var)
     ggplotly(univar)
   })
   
   output$proporcao <- renderPlotly({ # Gráficos com proporção ----
     prop <- prop(input$var)
     ggplotly(prop) %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.1))
   })
   
   output$boxplot <- renderPlot({ # Boxplots ----
     boxplot(input$var)
   })
   
   output$associacao <- renderDT({ # Tabela de Associações ----
     datatable(assoc, options = list(searching = FALSE)) 
   })
   
   output$bruto <- renderDT({ # Banco Bruto ----
     bruto <- read.csv2("dados.csv") %>% select(-1)
     datatable(bruto)
   })
   
   }

shinyApp(ui, server)












