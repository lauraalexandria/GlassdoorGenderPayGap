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
colnames(dados) <- c("Emprego", "Gênero", "Age", "Performance", "Título",
                     "Departamento", "Senioridade", "PagamentoBase", "Bonus",
                     "Idade")

# Gráficos Básicos ----

univar <- function(var){
  if(var == "Idade"){
    ggplot(dados, aes(Age)) + geom_bar() + 
      labs(x = "", y = "", title = var)
  } else{
    ggplot(dados, aes(get(var))) + geom_bar()  + 
      labs(x = "", y = "", title = var)
  }
}

# Proporções ----

prop <- function(var){
  ggplot(dados, aes(get(var), fill = `Gênero`)) + geom_bar(position = "fill") + 
    labs(x = "", y = "") + theme(legend.position = "top")
}

# Boxplots com Salários ----

boxplot <- function(var){
  ggplot(dados, aes(color = as.factor(get(var)), x = `Gênero`, y = PagamentoBase+Bonus)) + 
    geom_jitter() + geom_boxplot() + labs(x = "", y = "")
}











