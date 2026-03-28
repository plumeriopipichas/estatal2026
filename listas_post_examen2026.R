setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal2026/estatal_2026_RProject")

library(readxl)
library(dplyr)
library(stringr)
library(tools)

source("funciones_adhoc.R")

#-----agregar los extras a la lista de registro

temp <- read.csv("../listas_crudas/extras_registro.csv")
temp$clave <- as.character(temp$clave)
lista_general_registro_ <- bind_rows(lista_general_registro,temp) 

#---------- Limpiar listas de respuestas por sede 
respuestas_sede<-list()

# Variables esperadas
vars <- c("clave", "Aciertos", paste0("Resp_", 1:12))

for (sede in sedes) {
  if (is.data.frame(capturar_respuesta(sede))){
      respuestas_sede[[sede]] <- capturar_respuesta(sede)
  }
}  


lista_respuestas <- juntar_bases(respuestas_sede)

correcciones <- capturar_respuesta("correcciones")
print(dim(correcciones))
for (i in 1:nrow(correcciones)){
      clave <- substring(correcciones$clave[i],1,2)
      if (clave%in%claves_sede){
          x<-which(claves_sede==substring(correcciones$clave[i],1,2))
          correcciones$sede[i] <- sedes[x]
      }
}

extras <- capturar_respuesta("revision_registro")

#----------hacer que las claves ausentes en lista respuestas no se asocien (temporal, hay que checar las hojitas)

x<-which(nchar(lista_respuestas$clave)==0)
lista_respuestas$clave[x]<-1:length(x)

#------------- Crear lista para revisar examanes con claves no registradas

#x<-which(!lista_respuestas$clave%in%lista_general_registro$clave)

#claves_revisar <- lista_respuestas[x, c("clave","sede")]


#x<-which(!correcciones$clave%in%lista_general_registro$clave)

#claves_revisar <- correcciones[x, c("clave","sede")]

#write.csv(claves_revisar,"../listas_generadas/claves_revisar.csv",row.names = FALSE)


lista_respuestas <- bind_rows(lista_respuestas, correcciones, extras)

####Poner puntuaciones en la lista de respuestas

correctas <- c("A","B","C","C","C","A","C","B","A","C","D","A")  
valores <- c(rep(3, 4), rep(4, 4), rep(5, 4))
lista_respuestas$Puntos <- 0

# Recorre fila por fila y suma puntos correctos
for (i in 1:nrow(lista_respuestas)) {
  respuestas_alumno <- as.character(lista_respuestas[i, paste0("Resp_", 1:12)])
  aciertos <- respuestas_alumno == correctas
  puntaje <- sum(valores[aciertos]) 
  aciertos<-sum(aciertos)
  lista_respuestas$Puntos[i] <- puntaje
  lista_respuestas$Aciertos[i] <- aciertos
}

# Reordenar columnas: dejar 'clave', 'Aciertos', 'Puntos' al inicio
orden_col <- c("clave", "Aciertos", "Puntos",
               setdiff(names(lista_respuestas), c("clave", "Aciertos", "Puntos")))
lista_respuestas <- lista_respuestas[, orden_col]



#--------------la lista completa y la lista depurada

lista_general_puntuaciones <- merge(lista_general_registro_,lista_respuestas,by ="clave",all.y=TRUE)

lista_general_puntuaciones <- lista_general_puntuaciones %>%
  arrange(
    desc(Puntos),           # Primero por puntuación (mayor primero)
    desc(Aciertos),         # Luego por aciertos (mayor primero)
    Resp_1, Resp_2, Resp_3, Resp_4, Resp_5, Resp_6,
    Resp_7, Resp_8, Resp_9, Resp_10, Resp_11, Resp_12
  )


lista_depurada <- filter(lista_general_puntuaciones,!is.na(Nombre))%>%
                  filter(!sede=="revision_registro")
lista_depurada$Equipo<-subte(lista_depurada$Equipo)
lista_depurada$Escuela<-subte(lista_depurada$Escuela)

# para hacer las constancias

para_constancias <- select(lista_depurada,Nombre,Primer_apellido,Segundo_apellido,
                           Escuela=Equipo,Correo,Correo_escuela)


x<-grep("@",para_constancias$Correo)
y<-grep("@",para_constancias$Correo_escuela)
para_constancias<-para_constancias[union(x,y), ]

para_constancias$Nombre_completo<-paste(para_constancias$Nombre,
                                        para_constancias$Primer_apellido,para_constancias$Segundo_apellido)

para_constancias<-select(para_constancias,Nombre_completo,Escuela,Correo,Correo_escuela)
para_constancias$Correo<-subte_correo(para_constancias$Correo)
para_constancias$Correo_escuela<-subte_correo(para_constancias$Correo_escuela)
para_constancias<-unique(para_constancias)%>%
  arrange(Nombre_completo)

inicio <- 409
para_constancias$folio <- inicio:(inicio+nrow(para_constancias)-1)  

write.csv(para_constancias,"../listas_generadas/lista_constancias_participacion.csv",
          row.names = FALSE)

#-------------- varias listas para entregar: comite de examen, constancias de participacion

x<-grep("undar",lista_depurada$Grado_escolar,ignore.case = TRUE)
lista_secundaria_local<-lista_depurada[x, ]
rownames(lista_secundaria_local) <- NULL

x<-grep("chiller",lista_depurada$Grado_escolar,ignore.case = TRUE)
lista_prepa_local<-lista_depurada[x, ]
rownames(lista_prepa_local) <- NULL

comite_examen_preparatoria <- select(lista_prepa_local,clave,Aciertos,Puntos)%>%
          unique()
comite_examen_secundaria <- select(lista_secundaria_local,clave,Aciertos,Puntos)%>%
          unique()


write.csv(comite_examen_secundaria,"../listas_generadas/comite_examen_secu.csv",row.names = FALSE)
write.csv(comite_examen_preparatoria,"../listas_generadas/comite_examen_prepa.csv",row.names = FALSE)

rm(temp,vars,x,y)