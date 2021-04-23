library("tidyr")
library("dplyr")
library("magrittr")
library("readxl")
library("ggplot2")
library("plotly")

pop <- read.csv("./projeto_final.csv")
View(pop)

#1-d
idade_media = pop %>% summarise(mean(idade, na.rm = TRUE))
View(idade_media) #35.0405

#1-e
salario_medio = pop %>% summarise(mean(salario, na.rm = TRUE))
View(salario_medio) #11659.72


#1-f
pessoa_velha = pop %>% summarise(max(idade, na.rm = TRUE))
View(pessoa_velha)


#1-g
pessoa_nova = pop %>% summarise(min(idade, na.rm = TRUE))
View(pessoa_nova)


#1-h
qtd_mulheres = pop %>% 
  group_by(sexo) %>% filter(sexo == "F") %>% summarise(qtd = n())
View(qtd_mulheres) #5004


#1-h
qtd_homens = pop %>% 
  group_by(sexo) %>% filter(sexo == "M") %>% summarise(qtd = n())
View(qtd_homens) #4996


#2
pop %>%
  ggplot()+
  geom_histogram(aes(x=idade), color="black", fill="pink")+ #binwidth = 2
  labs(x="Idade", y="Quantidade de pessoas")


#3
graf <- pop %>% 
  filter(!is.na(fumante.f)) %>%
  ggplot() +
  geom_point(mapping = aes(x=imc, y=escolaridade,
                           text=paste("Fumante", fumante.f)))+
  labs(x="IMC", y="Escolaridade")

ggplotly(graf)


#4a - IMC x Salário x Sexo
pop %>%
  ggplot()+
  geom_point(aes(x=imc, y=salario, color=sexo))+
  labs(x="IMC", y="Salário", color="Sexo")


#4b - Peso x Idade x Sexo
pop %>%
  ggplot()+
  geom_line(aes(x=peso, y=idade, color=sexo))+
  labs(x="Peso", y="Idade", color="Sexo")


#4c - Peso x Filhos x Salário
pop %>%
  ggplot()+
  geom_point(aes(x=peso, y=salario, color=filhos))+
  labs(x="Peso", y="Salário", color="Filhos")


#4d - Altura x Peso
pop %>%
  ggplot()+
  geom_line(aes(x=altura, y=peso))+
  labs(x="Altura", y="Peso")


#4e - Peso x Fumante
grafic <- pop %>% 
  ggplot() +
  geom_point(mapping = aes(x=peso, y=fumante.f))+
  labs(x="Peso", y="Fumante")

ggplotly(grafic)


