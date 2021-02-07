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

inter_coeficientes <- read.csv("inter_coeficientes.csv")
inter_descritivas <- read.csv("inter_descritivas.csv")

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
                        style="vertical-align:middle;margin:350px 35px", width = "60%")
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
                       
                       box(title = "Interpretações",
                           width = 6,
                           height = 300,
                           withSpinner(textOutput("inter_descritivas"), type =2, color.background = "#ffffff"))
                       
                     ) # Fechamento fluidRow()
                     
                     ) # Fechamento tabPanel
                     
            ) # Fechamento navbarPage()
            
            ), # Fechamento função tabIten(Geral)
    
    ),  # Fechamento do primeiro tabItem;
    
    tabItem(tabName = "testes", #  Testes Estatísticos ----
            
            fluidRow(
              
              
                     
              box(width = 3,
                  height = 700,
                  "Afim de observar se existe diferença significativa entre a proporção de homens e mulheres em todos os campos da empresa, alguns testes Qui-Quadrado de Associação foram aplicados. 
                  A hipótese nula desses testes são a variável categòrica não está associoada ao gênero.
                  ", br(),
                  withSpinner(gt_output("associacao"), type =2, color.background = "#ffffff"), br(),
                  "Utilizando um nível de significância de 5%, pode-se observar que a grande na maioria das variáveis não possui diferenças na proporção entre homens e mulheres, com exceção do cargo ocupado pela pessoa, era esperado que essa variável apresentasse tal desvio, como vimos a razão para os cargos de gerente e engenheiro de software."
                  ),
               
              
              column(width = 9,
                     
                     
                     box(title = "Teste de Comparação de Médias",
                         width = 20,
                         height = 200,
                         "Após a aplicação de um teste de Anderson-Darling, conclui-se a 5% de significância que a variável relacionada ao salário total segue uma Ditribuição Normal, a partir disso, foi aplicado um teste de comparação de médias entre os dois gêneros, cuja a hipótese nula consiste na tese de que as médias nas duas populações são iguais", br(),
                         "O teste retornou um valor p igual a", round(medias[[3]],7), ", e novamente utilizando um nível de significância igual a 5%, não há indícios de que haja diferença significativa entre os salários recebidos pelos funcionários de diferentes gêneros.",
                         ),
                     
                     box(title = "Avaliando a interação entre o Gênero e demais variáveis no valor do Salário",
                         width = 30,
                         height = 400,
                         "Apesar do resultado do teste anterior, também é interessante avaliar se se o comportamento dos salários dos dois gêneros são iguais para todas as outras variáveis apresentadas no banco, foi ajustado um modelo de regressão linear com os termos todas as variáveis e  suas  respectivos termos de interação com a variável Gênero. Vale lembrar que todos os pressupostos foram verificados e todas as conclusões foram obtidas utizando 5% de Significância.",
                         br(),
                         radioGroupButtons(
                           inputId = "var2",
                           label = "Escolha a Variável",
                           choices = c("Cargo", "Idade", "Performance", "Titulo",
                                       "Departamento", "Senioridade"),
                           justified = TRUE ),
                         textOutput("inter_coeficientes"),
                         withSpinner(gt_output("reg"), type =2, color.background = "#ffffff")
                         )
                     
              ) # Fechamento segunda coluna;
              
            ) # Fechamento fluidRow()
            
    ), # Fechamento função tabIten(testes)
    
    tabItem(tabName = "banco", # Sobre o Banco ----
            
            fluidRow(
                     
              box(title = "Sobre o Banco",
                  width = 6,
                  height = 180,
                  "O banco de dados utilizado é originado da empresa Glassdoor e fornece os salários recebidos por seus funcionários, além de indicar o Gênero, a Idade, o Cargo, Nota de Performance, maior nível de educação, o Departamento e os anos que os funcionários trabalharam na empresa.", br(),
                  "Disponível em: https://www.kaggle.com/nilimajauhari/glassdoor-analyze-gender-pay-gap"),
                     
              widgetUserBox(
                title = "Laura Alexandria de Oliveria",
                subtitle = "Estudante - 6° período de Estatística",
                type = 2,
                src = "https://media-exp1.licdn.com/dms/image/C4E03AQEIYs8UPRUBtA/profile-displayphoto-shrink_200_200/0/1607697535388?e=1618444800&v=beta&t=d53JblrwITuE3kHHW2N6r95i-Kjear5rAcaWmcvYlSI",
                color = "aqua",
                "Tenho como objetivo trabalhar com análises estatísticas e ciência de dados, em conjunto com diferentes linguagens de programação. Por isso, como estagiária gostaria de trabalhar com diferentes tipos de visualizações, diversos métodos de modelagem e machine learning e manipulação de bancos de dados."
              )
              
            ), # Fechamento fluidRow()
            
            fluidRow(
              
              box(width = NULL,
                  height = 550,
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
    ggplotly(pontos, tooltip = "Total") %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = 0))
  })
  
   output$simples <- renderPlotly({ # Gráfico Univariado ----
     univar <- univar(input$var)
     ggplotly(univar, tooltip = "x")
   })
   
   output$proporcao <- renderPlotly({ # Gráficos com proporção ----
     prop <- prop(input$var)
     ggplotly(prop, tooltip = "x") %>% 
       layout(legend = list(orientation = "h", x = 0.2, y = -0.1))
   })
   
   output$boxplot <- renderPlot({ # Boxplots ----
     boxplot(input$var)
   })
   
   output$inter_descritivas <- renderText({ # Interpretações das Decritivas ----
     m <- ifelse(input$var == "Cargo", 1, ifelse(input$var == "Idade", 2,
          ifelse(input$var == "Performance", 3, ifelse(input$var == "Titulo", 4, 
          ifelse(input$var == "Departamento", 5, 6)))))
     inter_descritivas[m,3]
   })
   
   output$associacao <- render_gt({ # Tabela de Associações ----
     gt(assoc) 
   })
   
   output$inter_coeficientes <- renderText({ # Interpretações dos Coeficientes ----
     m <- ifelse(input$var2 == "Cargo", 1, ifelse(input$var2 == "Idade", 2,
                 ifelse(input$var2 == "Performance", 3, ifelse(input$var2 == "Titulo", 4, 
                 ifelse(input$var2 == "Departamento", 5, 6)))))
     inter_coeficientes[m,3]
   })
   
   output$reg <- render_gt({ # Tabela com os Coeficientes ----
     coef <- round(as.data.frame(coeficientes(input$var2)),4)
     gt(coef, rownames_to_stub = T) 
     
   })
   
   output$bruto <- renderDT({ # Banco Bruto ----
     bruto <- read.csv2("dados.csv") %>% select(-1)
     datatable(bruto)
   })
   
   }

shinyApp(ui, server)












