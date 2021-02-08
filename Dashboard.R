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
library(gt)

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
                         subtitle = "Média Salarial por Ano", 
                         icon = icon("dollar-sign"), color = "maroon",
                         width = 3
                       ),
                       
                       valueBox(      
                         value = round(mean(filter(dados, Genero == "Masculino")$Total),2),
                         subtitle = "Média Salarial Masculina por Ano", 
                         icon = icon("male"), color = "maroon",
                         width = 3
                       ),
                       
                       valueBox(      
                         value = round(mean(filter(dados, Genero == "Feminino")$Total),2),
                         subtitle = "Média Salarial Feminina por Ano", 
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
                  Nesse tipo de teste, a  hipótese nula afirma que a variável categórica em questão não está associoada à variável gênero.
                  ", br(), br(),
                  withSpinner(gt_output("associacao"), type =2, color.background = "#ffffff"), br(), br(),
                  "Utilizando um nível de significância de 5%, pode-se observar que a grande na maioria das variáveis não possui diferenças na proporção entre homens e mulheres, com exceção do cargo ocupado pela pessoa, confirmando as conclusões tiradas ao observar as representações gráficas."
                  ),
               
              
              column(width = 9,
                     
                     
                     box(title = "Teste de Comparação de Médias",
                         width = 20,
                         height = 180,
                         "Após a aplicação de um teste de Anderson-Darling, conclui-se a 5% de significância que a variável relacionada ao salário total segue uma Ditribuição Normal, a partir disso, foi aplicado um teste de comparação de médias entre os dois gêneros, cuja a hipótese nula consiste na tese de que as médias nas duas populações são iguais. ", br(),
                         "O teste retornou um valor p igual a", round(medias[[3]],7), ", e novamente utilizando um nível de significância igual a 5%, há indícios de que haja diferença significativa entre os salários recebidos pelos funcionários de diferentes gêneros.",
                         ),
                     
                     box(title = "Avaliando a interação entre o Gênero e demais variáveis no valor do Salário",
                         width = 30,
                         height = 500,
                         "E para entender melhor o resultado do teste anterior, também é interessante avaliar qual comportamento dos salários dos dois gêneros para todas as outras variáveis apresentadas no banco, por isso foi ajustado um modelo de regressão linear com todas as variáveis e  seus  respectivos termos de interação com a variável Gênero. Vale lembrar que todos os pressupostos foram verificados e todas as conclusões foram obtidas utizando 5% de Significância.",
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
                     
              box(title = "Sobre o Projeto",
                  width = 6,
                  height = 220,
                  "O presente projeto foi realizado para uma etapa do processo seletivo da empresa Oper. O banco de dados utilizado é originado da empresa Glassdoor e fornece os salários anuais recebidos por seus funcionários, além de indicar o Cargo, o Gênero, a Idade, a Nota de Performance, o maior nível de educação, o Departamento e os anos que o funcionário trabalhou na empresa, exatamante nesta ordem.", br(), br(),
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
     m <- ifelse(input$var == "Cargo", "A partir da análise do primeiro gráfico podemos observar que a frequência de todos os cargos é aproximadamente a mesma, todas se encontram no intervalo entre 90 e 118. Em seguida, vemos qual a proporção de mulheres e homens em cada um dos cargos, a grande maioria apresenta uma proporção próxima de 50%, com exceção dos Engenheiros de Software, Associados do Marketing e Gerentes, nesses casos a proporção de homens é igual a 92.7%, 9.3% e 80%, respectivamente. Ao avaliar o padrão dos boxplots em relação ao valor total recebido pela pessoa, podemos perceber que a distribuição dos boxplots pelos cargos é muito semelhante entre os gêneros. ", 
          ifelse(input$var == "Idade", "De acordo com o histograma apresentado, é possível aferir que a idade dos membros da empresa é aleatorimente distribuída entre 18 e 65 anos. Para avaliar a proporção entre os gêneros, a variável foi categorizada, e como resultado, o gráfico indica que as proporções de gênero entre as diferentes faxias etárias são próximas de 50%, todas inclusas no intervalo entre 48% e 58.6%.  Com os boxplots apresentados ao lado, pode-se observar o comportamento do salário dos empregados segundo a faixa etária e gênero, nesse caso, em ambos gêneros o média salarial cresce a medida que o funcionário é mais velho, porém, a última faixa etária parece favorecer os homens.",
          ifelse(input$var == "Performance", "Primeiro, pode-se observar que a frequência de funcionários com todas as possibilidades de notas para Performance é aproximadamente igual, sendo que pessoas avaliadas com nota 2 forma observadas 192 vezes e pessoas que possuem nota 5 foram observadas 209 vezes. Apesar de todas as notas apresentaram pequeno desvio em relação a proporção de 50%, a amostra coletada mostra uma leve associação, em que quanto maior a nota, maior será a proporção de homens. Partindo para a análise de distribuição dos salários, vê-se que a variação nas notas não parece afetar significantemente o valor do salário, assim como o padrão dos salários são parecidos para ambos os gêneros.", 
          ifelse(input$var == "Titulo", "Novamente, a frequência para todas as categorias é bastante similar, variando entre 238 e 265. Em cada uma das titulações também vê-se que a proporção entre os gêneros não é discrepante, pode-se observar que a menor razão de homens é igual a 49% e a maior proporção é igual a 58%. A distribuição dos salários é muito parecida entre os diferentes títulos e entre os gêneros.", 
          ifelse(input$var == "Departamento", "Ao observar o primeiro gráfico tem-se que o departamento com o maior número de funcionários é o de Operação, com uma frequência igual a 210, enquanto que o menor setor observado na amostra possui frequência igual a 192, que é o departamento de Engenharia. A proporção entre homens e mulheres em todos os setores também é muito parecida e todas próximas de 50%. Pode-se avaliar também com os boxplots que os salários são igualmente distribuídos entre todos os setores e também entre os gêneros.", 
                 "Tal variável representa o números de anos em que o funcionário trabalha na compania. Vê-se que funcionários com 4 anos de experiência na empresa são menos frequentes que os demais tipos, com 184 observações, enquanto que a maior frequência se encontra com os funcionários com 3 anos de empresa, com valor igual a 219. De acordo com os gráficos empilhados, observa-se que a razão de funcionários homens e mulheres são todos parecidos e próximos de 50%. E ainda, ao avaliar a distribuição dos salários dos funcionárias vemos a mesma tendência de crescimento com o aumento de tempo na empresa.")))))
     m
   })
   
   output$associacao <- render_gt({ # Tabela de Associações ----
     gt(assoc) 
   })
   
   output$inter_coeficientes <- renderText({ # Interpretações dos Coeficientes ----
     m <- ifelse(input$var2 == "Cargo", "O modelo apontou que os coeficientes dos cargos de Gerente, Associados do Marketing e Engenheiros de Software se mostraram significativamanete diferente dos demais cargos. Ao olhar para os fatores cruzados, nenhum dos cargos apresenta diferença significativa entre os dois gêneros.",
          ifelse(input$var2 == "Idade", "Pode-se observar que os coeficientes para todas as faixas etárias se mostraram significativas, ao contrário dos coeficientes relacionados ao fatores do interação, em que nenhum mostrou significantemente diferente de zero.",
          ifelse(input$var2 == "Performance", "É observado que o coeficiente ligado à variação das notas de performances é significantemente diferente de zero, porém o coeficiente ligado a interação com os gêneros não de mostrou significativo.", 
          ifelse(input$var2 == "Titulo", "Pela análise dos resultados é visto que o coeficiente relacionado a ter ou não PhD e o coeficiente relacionado a ter ou não Mestrado se mostraram significativamente diferentes de zero. Mas não parece haver diferenças significativas entre os valores totais das diferentes titulações em relação aos dois gêneros.", 
          ifelse(input$var2 == "Departamento", "Pode-se observar que não houve diferença significativa entre os diferentes departamentos ou a interação entre esses departamentos e o gênero dos funcionários.", 
                 "Para Senioridade, é possível observar que o coeficiente se mostrou significativo, de forma que o aumento dos anos de trabalho na empresa aumentam o salário do trabalhador, bem como o coeficiente para a interação entre essa variável e o Gênero se mostrou significativo, o que sugere ser o motivo pelo qual o resultado do teste geral entre as médias ter indicado diferenças nas médias.")))))
     m
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












