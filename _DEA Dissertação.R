install.packages("Benchmarking")
library(Benchmarking)

#MODELO 1 --- TODAS AS IES, com FGV consolidada
IES<-read.csv("M1_INPUTS & OUTPUTS_FGV.csv", sep=";", dec=",")

(x1<-with(IES, cbind(STUDENTS, FACULTY, PHD, MA, TEC, VL_DESPESA_PESSOAL_DOCENTE, VL_DESPESA_PESSOAL_TECNICO, VL_DESPESA_CUSTEIO, VL_DESPESA_INVESTIMENTO, VL_DESPESA_PESQUISA, VL_DESPESA_OUTRA)))
(y1<-with(IES, cbind(ST_COMPLETING, PUB_SC, PATENTS_UM)))

summary(IES)
summary(x1)
summary(y1)

  #rodando modelos com crs e vrs para comparar
(e1_crs <- dea(x1,y1, RTS="crs", ORIENTATION = "out"))
summary(e1_crs)
(e1_vrs <- dea(x1,y1, RTS="vrs", ORIENTATION = "out"))
summary(e1_vrs)

  #Calculando slacks
s1crs<-slack(x1, y1, e1_crs)
(s1crs)
s1vrs<-slack(x1, y1, e1_vrs)
(s1vrs)
  #Plot com TRANSPOSE=TRUE para casos com mais de 1 output?
    #ORIENTATION: 1 input, 1 output: in-out; 2 inputs: "in"; 2 outputs: "out"
graf <- dea.plot(x1, y1, RTS="vrs", ORIENTATION="in-out", xlab = "Inputs", ylab = "Outputs")
(graf)

  #Calcular eficiência de Farrell
scores <- eff(e1_vrs) #ou
(scores)
#Efficiency ladder - n é o número da firma da qual se quer a ladder
eladder(n, X, Y, RTS = "vrs", ORIENTATION = "out") 

#Excesso de inputs comparado com a fronteira.
excess(e_out1)

#Encontrar DMUs parecidas (peers). N refere à DMU que se deseja encontrar peers.
peers(dea, NAMES=TRUE, N=1:dim(dea$lambda)[1], LAMBDA=0)
get.which.peers(dea, N=1:dim(dea$lambda)[2], LMABDA=0)
get.peers.lambda(dea, N=1(dea$lambda)[1], LAMBDA=0)
#____________________________________________________________________________________________________________________

#MODELO 2 --- PUBS vs PRIVS
IESpub<-read.csv("M2_INPUTS & OUTPUTS pub.csv", sep=";", dec=",")
IESprv<-read.csv("M2_INPUTS & OUTPUTS prv.csv", sep=";", dec=",")

(x2pub<-with(IESpub, cbind(STUDENTS, FACULTY, PHD, MA, TEC, VL_DESPESA_PESSOAL_DOCENTE, VL_DESPESA_PESSOAL_TECNICO, VL_DESPESA_CUSTEIO, VL_DESPESA_INVESTIMENTO, VL_DESPESA_PESQUISA, VL_DESPESA_OUTRA)))
(y2pub<-with(IESpub, cbind(ST_COMPLETING, PUB_SC, PATENTS_UM)))

(x2prv<-with(IESprv, cbind(STUDENTS, FACULTY, PHD, MA, TEC, VL_DESPESA_PESSOAL_DOCENTE, VL_DESPESA_PESSOAL_TECNICO, VL_DESPESA_CUSTEIO, VL_DESPESA_INVESTIMENTO, VL_DESPESA_PESQUISA, VL_DESPESA_OUTRA)))
(y2prv<-with(IESprv, cbind(ST_COMPLETING, PUB_SC, PATENTS_UM)))

  #PUBs rodando modelos com crs e vrs para comparar
(e2pub_crs <- dea(x2pub,y2pub, RTS="crs", ORIENTATION = "out"))
summary(e2pub_crs)
(e2pub_vrs <- dea(x2pub,y2pub, RTS="vrs", ORIENTATION = "out"))
summary(e2pub_vrs)

  #Privs rodando modelos com crs e vrs para comparar
(e2prv_crs <- dea(x2prv,y2prv, RTS="crs", ORIENTATION = "out"))
summary(e2prv_crs)
(e2prv_vrs <- dea(x2prv,y2prv, RTS="vrs", ORIENTATION = "out"))
summary(e2prv_vrs)


#Calculando slacks
s2pub_crs<-slack(x2pub, y2pub, e2pub_crs)
(s2pub_crs)
s2pub_vrs<-slack(x2pub, y2pub, e2pub_vrs)
(s2pub_vrs)

s2prv_crs<-slack(x2prv, y2prv, e2prv_crs)
(s2prv_crs)
s2prv_vrs<-slack(x2prv, y2prv, e2prv_vrs)
(s2prv_vrs)

#Plot com TRANSPOSE=TRUE para casos com mais de 1 output?
#ORIENTATION: 1 input, 1 output: in-out; 2 inputs: "in"; 2 outputs: "out"
graf2pub <- dea.plot(x2pub, y2pub, RTS="vrs", ORIENTATION="in-out", xlab = "Inputs", ylab = "Outputs")
(graf)

graf2prv <- dea.plot(x2prv, y2prv, RTS="vrs", ORIENTATION="in-out", xlab = "Inputs", ylab = "Outputs")
(graf)

#____________________________________________________________________________________________________________________
#MODELO 3 --- REMOVENDO OUTLIERS

IES3<-read.csv("INPUTS & OUTPUTS_outliers.csv", sep=";", dec=",")

(x3<-with(IES3, cbind(STUDENTS, FACULTY, PHD, MA, TEC, VL_DESPESA_PESSOAL_DOCENTE, VL_DESPESA_PESSOAL_TECNICO, VL_DESPESA_CUSTEIO, VL_DESPESA_INVESTIMENTO, VL_DESPESA_PESQUISA, VL_DESPESA_OUTRA)))
(y3<-with(IES3, cbind(ST_COMPLETING, PUB_SC, PATENTS_UM)))

(e_in3 <- dea(x3,y3, RTS="vrs", ORIENTATION = "in"))
summary(e_in3)

(e_out3 <- dea(x3,y3, RTS="vrs", ORIENTATION = "out"))
summary(e_out3)

graf3 <- dea.plot(x3, y3, RTS="vrs", ORIENTATION="in-out", xlab = "Inputs", ylab = "Outputs")
(graf3)

#_________________________________________________________________________