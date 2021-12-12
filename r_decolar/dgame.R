#----------------------------------------------------------------#
# Autor: Cherlynn Arce                                           #
# Titulo: Calculo o numero de tentativas de obtencao da primeira #
# sequencia em lancamento de dados                               #
# Curso: Jornada Ômega Data Science                              #
#                                                                #
#----------------------------------------------------------------#

dgame <- function(n = 10000, dados = 3){
  # funcao retorna uma lista tendo como primeiro elemento o numero medio 
  # de tentativas ate obtencao da primeira sequencia do lancamento de dados
  # e o segundo elemento as contagens de tentativas dos lancamentos
  
  # input (usuario)
  # n = numero de vezes que os dados serao lancados
  # dados = numero de dados que deverao ser lancados
  
  u <- replicate(n, {
    i <- 0
    repeat{
      s <- sample(1:6, size = dados, replace = T)
      s1 <- sort(s)
      s2 <- diff(s1)
      i <- i + 1
      if(all(s2 == rep(1, (dados-1)))) break
    }
    i
  })
  return(list( media = round(mean(u),0),
               tentativas = u))
}