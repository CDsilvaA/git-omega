#---------------------------------------------------------------#
# Autor: Cherlynn Arce                                          #
# Titulo: Calculo de probabilidades do jogo de Bozo             #
# Curso: Jornada Ômega Data Science                             #
#                                                               #
#---------------------------------------------------------------#

bozo <- function(n = 50000){
  # funcao que retorna as probabilidades de obtencao de
  # sequencia, full_house, quadra, general e outro 
  # no jogo de Bozo
  
  # input (usuario)
  # n = total de lancamento dos dados
  
  i <- 1
  sequencia = full = quadra = general <- 0
  repeat{
    s <- sample(1:6, size = 5, replace = T)
    s1 <- sort(s)
    s2 <- diff(s1)
    i <- i + 1
    if(all(s2 == c(1, 1, 1, 1))){
      sequencia <- sequencia + as.integer(all(s2 == c(1, 1, 1, 1)))
    } else if(all(abs(s2)[1:3] == c(0, 0, 0)) & is.element(sum(s2), seq(1, 6))){
      quadra <- quadra + as.integer(all(abs(s2)[1:3] == c(0, 0, 0)))
    } else if(all(abs(s2)[1:2] == c(0, 0)) & is.element(sum(s2), seq(1, 9))){
      full <- full + as.integer(all(abs(s2)[1:2] == c(0, 0)))
    } else if(all(abs(s2) == c(0, 0, 0, 0))){ 
      general <- general + as.integer(all(abs(s2) == c(0, 0, 0, 0)))
    } else {
      next
    }
    if(i > n) break
  }
  prob<-data.frame(sequencia  = sequencia/n,
                   full_house = full/n     ,
                   quadra     = quadra/n   ,
                   general    = general/n  ,
                   outro      = 1 - sum(sequencia, full, quadra, general)/n)
  return(prob)
}