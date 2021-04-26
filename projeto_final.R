library("tidyr")
library("dplyr")
library("magrittr")
library("readxl")
library("ggplot2")
library("plotly")

pop <- read.csv("./projeto_final.csv", encoding = "UTF-8")
View(pop)

# Idade média da população
idade_media = pop %>% summarise(mean(idade, na.rm = TRUE))
View(idade_media) #35.0405

# Salário médio da população
salario_medio = pop %>% summarise(mean(salario, na.rm = TRUE))
View(salario_medio) #11659.72


# Idade da pessoa mais velha
pessoa_velha = pop %>% summarise(max(idade, na.rm = TRUE))
View(pessoa_velha)


# Idade da pessoa mais nova
pessoa_nova = pop %>% summarise(min(idade, na.rm = TRUE))
View(pessoa_nova)


# Quantidade de mulheres
qtd_mulheres = pop %>% 
  group_by(sexo) %>% filter(sexo == "F") %>% summarise(qtd = n())
View(qtd_mulheres) #5004


# Quantidade de homens
qtd_homens = pop %>% 
  group_by(sexo) %>% filter(sexo == "M") %>% summarise(qtd = n())
View(qtd_homens) #4996


# Gráfico Histograma - Quantidade de pessoas x Idade
pop %>%
  ggplot()+
  geom_histogram(aes(x=idade), color="black", fill="pink")+ #binwidth = 2
  labs(x="Idade", y="Quantidade de pessoas")


# Gráfico de Pontos Iterativo - IMC x Escolaridade x Fumante
graf <- pop %>% 
  filter(!is.na(fumante.f)) %>%
  ggplot() +
  geom_point(mapping = aes(x=imc, y=escolaridade,
                           text=paste("Fumante", fumante.f)))+
  labs(x="IMC", y="Escolaridade")

ggplotly(graf)


# Gráfico de Pontos - IMC x Salário x Sexo
pop %>%
  ggplot()+
  geom_point(aes(x=imc, y=salario, color=sexo))+
  labs(x="IMC", y="Salário", color="Sexo")


# Gráfico de Linha - Peso x Idade x Sexo
pop %>%
  ggplot()+
  geom_line(aes(x=peso, y=idade, color=sexo))+
  labs(x="Peso", y="Idade", color="Sexo")


# Gráfico de Pontos - Peso x Filhos x Salário
pop %>%
  ggplot()+
  geom_point(aes(x=peso, y=salario, color=filhos))+
  labs(x="Peso", y="Salário", color="Filhos")


# Gráfico de Linha- Altura x Peso
pop %>%
  ggplot()+
  geom_line(aes(x=altura, y=peso))+
  labs(x="Altura", y="Peso")


# Gráfico de Pontos Iterativo - Peso x Fumante
grafic <- pop %>% 
  ggplot() +
  geom_point(mapping = aes(x=peso, y=fumante.f))+
  labs(x="Peso", y="Fumante")

ggplotly(grafic)


