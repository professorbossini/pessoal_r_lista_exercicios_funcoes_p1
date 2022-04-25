#1. Considerando a sequência numérica a seguir, selecione todos os elementos 
# de x que podem ser representados por um número inteiro, ou seja, cuja parte 
# fracionária é igual a zero (exemplo: 1.0, 2.0, etc)
#x = 1, 1.25, 1.50, ..., 9.75, 10
x <- seq (1, 10, 0.25);x
#1, 1, 1, 1, 2, 2, 2, 2, ..., 9, 9, 9, 9, 10, 10, 10, 10
trunc(x)
#0, -0.25, -0.5, -0.75, 0, ..., 0, -0.25, -0.5, -0.75, 0
trunc(x) - x
#1, 2, ..., 10
x[trunc(x) - x == 0]
######################################################
#2. Dado um vetor qualquer, liste os elementos que aparecem repetidos no vetor. 
#Por exemplo, os elementos repetidos no vetor (1 2 1 5 3 4 1 5 2 8 1) são 1, 2 e 5.
v <- c(1, 2, 1, 5, 3, 4, 1, 5, 2, 8, 1);v
#A função duplicated determina quais elementos de uma coleção são cópias de elementos com índice menor do que o deles
?duplicated
#veja que começa mostrando false, embora o 1 esteja repetido. 
#O primeiro 1 é o de menor índice
#O terceiro 1 tem índice 3, como há alguém igual a ele e de índice menor que o dele, ele é considerado uma cópia
duplicated(v)
#1, 1, 5, 2, 1
v[duplicated(v)]
#1, 5, 2
unique(v[duplicated(v)])
#1, 2, 5
sort(unique(v[duplicated(v)]))
######################################################
#3. Abra o arquivo questionario.txt como um dataframe.
#ID: Identificação do aluno (número sequencial)
#Turma: Turma a que o aluno foi alocado (A ou B)
#Sexo (F ou M)
#Idade (idade em anos)
#Alt (Altura em metros)
#Peso (Peso em quilogramas)
#Filhos (Número de filhos na família)
#Fuma (Sim ou Não)
#Toler (Tolerância ao cigarro (I (I)ndiferente, P incomoda (P)ouco e M incomoda (M)uito))
#Exerc: horas de atividade física, por semana
#Cine: número de vezes que vai ao cinema por semana
#OpCine: Opinião a respeito das salas de cinema na cidade (B: Regular e (B)oa, M: (M)uito Boa)
#TV: horas gastas assistindo TV, por semana
#OpTV: Opinião a respeito da qualidade da programação na TV
#exibe o diretório atual
getwd()
#se necessário, configura o diretório atual
#setwd(diretorio_de_interesse)
#lê o arquivo
arq <- read.table(file="questionario.txt", header = TRUE);arq
######################################################
#4. Escolha apenas os fumantes.
arq[arq$Fuma=="SIM",]
######################################################
#5. Qual o percentual de pessoas muito incomodadas com o fumo da turma A?
#se o objeto é um dataframe, length devolve o número de colunas. não serve nesse caso
length(arq)
#número de linhas total: 50
nrow(arq)

#todas as variáveis(colunas) dos alunos de Turma=A e Toler=M
arq[arq$Toler=="M" & arq$Turma=="A",]

#número de linhas em que Turma=A e Toler=M: 9
nrow(arq[arq$Toler=="M" & arq$Turma=="A",])

#percentual: 0.18
percentual <- nrow(arq[arq$Toler=="M" & arq$Turma=="A",]) / nrow(arq)
#paste para concatenar
print (paste(percentual * 100, '%', sep=''))

######################################################
#6. Calcular a massa corporal dos alunos, adicionando nova coluna ao dataframe
#IMC = peso / (altura * altura)
arq["massa_corp"] <- arq$Peso / (arq$Alt ^ 2);arq$massa_corp
######################################################
#7. Qual o sexo e a turma do(s) aluno(s) com o maior peso?
maior_peso <- max(arq$Peso);maior_peso
arq[arq$Peso == maior_peso, c("Sexo", "Turma")]
######################################################
#8. Quais os alunos da turma B que são mais baixos que o aluno mais baixo da turma A?
menor_altura_turmaA <- min(arq[arq$Turma=="A", "Alt"])
arq[arq$Turma=="B" & arq$Alt < menor_altura_turmaA, ]
######################################################
#9. Os 5 alunos de maior IMC são os 5 que mais assistem televisão?

#order devolve uma lista de índices ordenada de acordo com a variável especificada
?order

arq #Observe que o elemento 37 tem maior IMC, seguido de 35 etc

#decreasing, por padrão, é FALSE
#observe o resultado: 37, 35, 18... etc são os índices das pessoas de maior IMC
order(arq$massa_corp, decreasing = TRUE)

ordenado_por_IMC <- arq[order(arq$massa_corp, decreasing = TRUE),];ordenado_por_IMC

ordenado_por_TV <- arq[order(arq$TV, decreasing = T), ]; ordenado_por_TV

?intersect
#intesect calcula a intersecção (elementos em comum) entre conjuntos
#veja exemplos
intersect(c(1, 2), c(2, 3))
intersect(c(4, 3, 2, 1), c(4, 3, 3, 1))
intersect(c(1, 2, 3), c(4, 5, 6))

#agora pegamos os ids dos 5 primeiros por IMC e por tv
idsIMC <- ordenado_por_IMC[1:5, 'Id']; idsIMC
idsTV <- ordenado_por_TV[1:5, 'Id'];idsTV

#Vejamos a intersecção: só o aluno 35 está nos dois grupos
intersect(idsIMC, idsTV)

#vamos mostrar todos (só tem um nesse caso)
arq[arq$Id %in% intersect(idsIMC, idsTV),]

######################################################
#10. Faça um gráfico de dispersão entre tempo de TV e tempo de exercícios. 
# Você nota alguma relação entre estas variáveis aleatórias?
#aparentemente não muito...
plot(arq$TV, arq$Exer, xlab = "TV", ylab = 'Exercícios')

# e entre tempo de tv e IMC?
#arqui parece que as pessoas que menos veem tv tendem a ter menor IMC
plot(arq$TV, arq$massa_corp, xlab="TV", ylab="IMC")
#11. Faça três boxplots da variável aleatória Peso, um geral (cinza claro), 
#um para os meninos (verde claro) e outro para as meninas (azul claro). 
#Compare.
par(mfrow = c (1, 3))
#de todos
boxplot (arq[, "Peso"], main = "Geral", ylab = "peso em Kg", col = "lightgray")
boxplot (arq[arq$Sexo=="M", "Peso"], main = "M", ylab = "peso em Kg", col = "lightgreen")
boxplot (arq[arq$Sexo=="F", "Peso"], main = "F", ylab = "peso em Kg", col = "lightblue")
par(mfrow = c (1, 1))
print("O peso dos homens possui maior amplitude no 3o quartil, enquanto o")
print("feminino, no 1o quartil. A mediana do peso feminino está abaixo da")
print("mediana geral. A do masculino, acima.")

######################################################
#12. Faça um histograma azul da v.a idade.
histograma <- hist(arq[, "Idade"], main = "Idade", xlab = "Idade em anos", ylab = "Frequência absoluta", col = "blue")
#veja algumas informações do histograma, se desejar
histograma
######################################################
#13. Faça um gráfico de barras verde da v.a filhos. Comente o resultado.
barplot(arq[, 'Filhos'], main = "Filhos", xlab = "filhos", ylab = "quantidade", col = "green")
print ("Diferente do histograma, um gráfico de barras não faz agrupamento de valores para contagem de frequência")
######################################################
#14. Crie um vetor com as frequências absolutas de fumantes e não fumantes e com ele
#faça um gráfico de barras roxo. Passe um vetor c('SIM','NÃO') no parâmetro names.arg
#para configurar as etiquetas embaixo de cada barra.

#ids dos fumantes
fumantes <- arq[arq$Fuma=="SIM", 'Id']; fumantes;

#pode ser assim
nFumantes <- arq[arq$Fuma=="NAO", 'Id']; nFumantes;

#dá pra fazer com a diferença de conjuntos também
#da coleção total, removemos aqueles que são fumantes com setdiff
?setdiff
#ids dos não fumantes
nFumantes <- setdiff(arq[, 'Id'], fumantes); nFumantes;
etiqs <- c('SIM','NÃO')
#col="purple" para cor roxa para as duas colunas
#col=c("red", "green") para cores diferentes para cada coluna
barplot(c(length(fumantes), length(nFumantes)), main='Fumantes', names.arg=etiqs, ylab='frequência', col=c('red', 'green'))

