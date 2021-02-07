library(tidyverse)

paleta1 <- colorRampPalette(c("#57b952","#283a4e"))

dados <- read.csv2("dados.csv", sep = ";") %>% select(-1) %>% 
  mutate(Idade = case_when(Age < 25 ~ "18 - 24",
                           24 < Age & Age < 33 ~ "25 - 32",
                           32 < Age & Age < 40 ~ "33 - 39",
                           39 < Age & Age < 48 ~ "40 - 47",
                           47 < Age & Age < 56 ~ "48 - 55",
                           Age > 55 ~ "> 55"),
         Gender = case_when(Gender == "Female" ~ "Feminino",
                            Gender == "Male" ~ "Masculino"),
         Total = BasePay + Bonus) 
dados$Idade <- fct_relevel(as.factor(dados$Idade), "18 - 24", "25 - 32", "33 - 39",
                           "40 - 47", "48 - 55", "> 55")
colnames(dados)[1:10] <- c("Cargo", "Genero", "Age", "Performance", "Titulo",
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
          legend.position = "top") + 
    scale_fill_manual(values=paleta1(2))
}

salarios <- function(){
  ggplot(dados, aes(x = Total)) + geom_histogram(color = "white", fill = "#d1d9d9") + 
    labs(x = "", y = "Frequência")
}

pontos <- function(){
  ggplot(dados, aes(y = Total, x = Genero, color = Genero)) + geom_jitter() + 
    labs(x = "", y = "Frequência") + 
    scale_color_manual(values=paleta1(2))
}

univar <- function(var){
  if(var == "Idade"){
    ggplot(dados, aes(Age)) + geom_histogram(fill = "#283a4e") + 
      labs(x = "", y = "", title = var)
  } else{
    ggplot(dados, aes(y = get(var))) + geom_bar(fill = "#283a4e")  + 
      labs(x = "", y = "", title = var)
  }
}

# Proporções ----

prop <- function(var){
  ggplot(dados, aes(y = get(var), fill = `Genero`)) + geom_bar(position = "fill") + 
    labs(x = "", y = "", fill = "") + theme(legend.position = "top") + 
    scale_fill_manual(values=paleta1(2))
}

# Boxplots com Salários ----

boxplot <- function(var){
  ggplot(dados, aes(color = as.factor(get(var)), x = `Genero`, y = Total)) + 
    geom_boxplot() + labs(x = "", y = "", color = "") + 
    scale_color_manual(values=paleta1(length(as.character(unique(dados[, var])))))
}











