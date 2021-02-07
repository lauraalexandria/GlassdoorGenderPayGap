
# Comparações de Médias

medias <- t.test(dados$Total ~ dados$Genero, paired = F)
medias[[3]]

# Associações ----

assoc <- data.frame("Variaveis" = c("Genero vs Cargo",
                                   "Genero vs Idade",
                                   "Genero vs Perfomance",
                                   "Genero vs Título",
                                   "Genero vs Departamento",
                                   "Genero vs Senioridade"),
         "P-Valor" = round(c(chisq.test(dados$Genero, dados$Cargo)[[3]],
                             chisq.test(dados$Genero, dados$Idade)[[3]],
                             chisq.test(dados$Genero, dados$Performance)[[3]],
                             chisq.test(dados$Genero, dados$Titulo)[[3]],
                             chisq.test(dados$Genero, dados$Departamento)[[3]],
                             chisq.test(dados$Genero, dados$Senioridade)[[3]]), 4))

# 