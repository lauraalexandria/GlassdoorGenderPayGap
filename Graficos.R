library(tidyverse)

dados <- read.csv2("dados.csv", sep = ";") %>% select(-1) %>% 
  mutate(Idade = case_when(Age < 25 ~ "18 - 24",
                           24 < Age & Age < 33 ~ "25 - 32",
                           32 < Age & Age < 40 ~ "33 - 39",
                           39 < Age & Age < 48 ~ "40 - 47",
                           47 < Age & Age < 56 ~ "48 - 55",
                           Age > 55 ~ "> 55"),
         Gender = case_when(Gender == "Female" ~ "Feminino",
                            Gender == "Male" ~ "Masculino")) 
dados$Idade <- fct_relevel(as.factor(dados$Idade), "18 - 24", "25 - 32", "33 - 39",
                           "40 - 47", "48 - 55", "> 55")
colnames(dados) <- c("Cargo", "Genero", "Age", "Performance", "Titulo",
                     "Departamento", "Senioridade", "PagamentoBase", "Bonus",
                     "Idade")

# Gráficos Básicos ----

pizza <- function(){
  ggplot(dados, aes(x = factor(1), fill = Genero)) + geom_bar(width = 1) + 
    coord_polar("y") + 
    labs(title = "",
         fill = "Legenda") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top")
}

salarios <- function(){
  ggplot(dados, aes(x = PagamentoBase + Bonus)) + geom_histogram(color = "white") + 
    labs(x = "", y = "Frequência")
}

univar <- function(var){
  if(var == "Idade"){
    ggplot(dados, aes(Age)) + geom_histogram() + 
      labs(x = "", y = "", title = var)
  } else{
    ggplot(dados, aes(y = get(var))) + geom_bar()  + 
      labs(x = "", y = "", title = var)
  }
}

# Proporções ----

prop <- function(var){
  ggplot(dados, aes(y = get(var), fill = `Genero`)) + geom_bar(position = "fill") + 
    labs(x = "", y = "", fill = "") + theme(legend.position = "top")
}

# Boxplots com Salários ----

boxplot <- function(var){
  ggplot(dados, aes(color = as.factor(get(var)), x = `Genero`, y = PagamentoBase+Bonus)) + 
    geom_jitter() + geom_boxplot() + labs(x = "", y = "", color = "")
}











