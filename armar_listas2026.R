setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal2026/estatal_2026_RProject")

library(readxl)
library(dplyr)
library(stringr)
library(tools)

source("funciones_adhoc.R")


sedes <- c("Actopan","Huejutla","Huichapan","Ixmiquilpan","Ixtlahuaco","Metztitlan","Pachuca ITESM",
           "Pachuca UAEH","Pachuca CECYTEH","Pisaflores","Sahagun","Tizayuca",
           "Tlanchinol","Tula","Tulancingo","Zimapan")


claves_sede <- c(55,54,26,94,71,92,37,87,85,51,64,73,82,17,93,67)

#Convertir a csv las listas de registro de escuela

codes <- setNames(10:99,LETTERS)

input_escuelas<-"../listas_crudas/Registro_escuelas"
output_escuelas<-"../listas_crudas/csv_escuelas"

temp<-list.files(input_escuelas,pattern="\\.xlsx$",full.names = TRUE)

for (f in temp){
  df<-read_excel(f,sheet = 1, skip =2)[ ,1:9]
  names(df) <- gsub("\\s+", "_", names(df))
  out_name<-paste0(file_path_sans_ext(basename(f)),".csv")
  out_path<-file.path(output_escuelas,out_name)
  write.csv(df,out_path,row.names = FALSE)
}

# Depuracion de lista de independientes

indeps <- read.csv("../listas_crudas/independientes_formulario.csv")[ ,2:9]
names(indeps)<-c("Correo","Primer_apellido","Segundo_apellido","Nombre","CURP",
                 "Grado_escolar","Correo_alt","Sede")
indeps$Sede <- sub("-.*", "", indeps$Sede)
indeps$Sede <- sub("á", "a", indeps$Sede)
indeps$Sede <- sub("ú", "u", indeps$Sede)

cols <- c("Primer_apellido","Segundo_apellido","Nombre")

indeps[cols] <- lapply(indeps[cols], arregla_nombre)
indeps$Grado_escolar <- str_to_sentence(str_to_lower(indeps$Grado_escolar))

indeps$Equipo<-"Participante Independiente"
indeps$Clave_escuela <- NA
indeps$Correo_escuela <- NA


#Generar listas basicas por sede, juntando las escuelas con los independientes

indeps_por_sede<-list()    #lista para bases de datos de independientes por sede, completos
escuela_por_sede<-list() #lista para bases de datos de equipos de escuela por sede, completos
basica_sede <- list()    #lista para bases de datos por sede con variables basicas

archivos<-dir("../listas_crudas/csv_escuelas/")


for (temp in sedes){
  print(temp)
  x<-which(indeps$Sede==temp)
  indeps_por_sede[[temp]] <- data.frame()
  if (length(x)>0){
    indeps_por_sede[[temp]] <- indeps[x, ]
  }  

  y <- grep(paste0(gsub("\\s+", "",temp),"_"),archivos)
  if (length(y)>1){
              print("Error: archivos excedentes.")
              break
  }
  print(length(y))
  if (length(y)==1){
    path <- file.path("../listas_crudas/csv_escuelas", archivos[y])
    escuela_por_sede[[temp]]<-read.csv(path)
    escuela_por_sede[[temp]]$Equipo<-escuela_por_sede[[temp]]$Escuela
    escuela_por_sede[[temp]]$Sede<-temp
    basica_sede[[temp]] <- bind_rows(escuela_por_sede[[temp]],indeps_por_sede[[temp]])
    print(c(temp,ncol(basica_sede[[temp]]),"variables"))
    basica_sede[[temp]]$Nombre<-arregla_nombre(basica_sede[[temp]]$Nombre)
    basica_sede[[temp]]$Primer_apellido<-arregla_nombre(basica_sede[[temp]]$Primer_apellido)
    basica_sede[[temp]]$Segundo_apellido<-arregla_nombre(basica_sede[[temp]]$Segundo_apellido)
  }
  else{
    escuela_por_sede[[temp]]<-data.frame()
    basica_sede[[temp]]<-data.frame()
  }
}

# generar las claves de los participantes y agregarlas a las listas basica_sede 


for (i in 1:length(sedes)){
  if (nrow(basica_sede[[i]])>0){
      clave<-claves_sede[i]
      print(sedes[i])
      basica_sede[[i]]$clave<-""
      for (j in 1:nrow(basica_sede[[i]])){
          Curp<-basica_sede[[i]]$CURP[j]
          basica_sede[[i]]$clave[j]<-paste0(clave,letranumero(substr(Curp,4,4)),
                                            substr(Curp,5,10))  
      }
  }
}


temp<-lista_general_registro$clave
borrados<-0
while (anyDuplicated(temp)>0){
      pivote<-anyDuplicated(temp)
      print(c("repetido encontrado en", pivote+borrados,"clave",temp[pivote]))
      if (temp[pivote]%in%temp[1:(pivote-1)]){
          temp<-temp[-pivote]
          borrados<-borrados+1
          lista_general_registro$CURP[1:(pivote-1)]
      }
      else{ 
          temp[pivote]<-remplazar(temp[pivote],4)
          lista_general_registro$clave[pivote+borrados]<-temp[pivote]
      }
}



x<-which(sapply(basica_sede,nrow)>0)
sede_novacia<-sedes[x]


#general la lista general con todos juntos a partir de las listas basica_sede, exportarla

lista_general_registro <- juntar_bases(basica_sede)


write.csv(lista_general_registro,
          "../listas_generadas/lista_general_registro.csv",row.names = FALSE)



# hacer las listas de asistencia desde las listas basicas, exportar ambas por sede en csv


indeps_repetidos<-list()
asistencia<-list()

for (i in sede_novacia){
  
  print(c("asistencia", i))
  
  asistencia[[i]] <- basica_sede[[i]] %>%
    select(clave, Nombre, Primer_apellido, Segundo_apellido, Equipo, Sede) %>%
    arrange(Nombre)
  
  asistencia[[i]] <- cbind(orden=1:nrow(asistencia[[i]]),asistencia[[i]])
  
  res <- borrar_duplicados(asistencia[[i]])
  asistencia[[i]] <- res$base_limpia
  indeps_repetidos[[i]] <- res$eliminados
}


#exportar listas basicas y de asistencia por sede a los csv

for (sede in sede_novacia){
    print(c("exportar listas de",sede))
    path1 <- file.path("../listas_generadas/basicas_por_sede/",
                       paste0(sede,"_basica.csv",sep=""))
    path2 <- file.path("../listas_generadas/listas_de_asistencia/",
                       paste0(sede,"_asistencia.csv",sep=""))
    print(dim(basica_sede[[sede]]))
    write.csv(basica_sede[[sede]],file = path1,row.names = FALSE)
    write.csv(asistencia[[sede]],file = path2,row.names = FALSE)
}

resumen <- tibble(
  sede = names(basica_sede),
  alumnos = sapply(basica_sede, nrow)
)


write.csv(resumen,"../listas_generadas/participacion_sedes.csv",row.names = FALSE)

#-----------extraer lista de correos y mandarla a un csv

correos<-select(lista_general_registro,Correo)%>%
         unique()
correosesc<-select(lista_general_registro,Correo_escuela)%>%
    unique()
names(correosesc)<-"Correo"
correos<-rbind(correos,correosesc)

write.csv(x = correos,"../listas_generadas/correos.csv",row.names = FALSE)

rm(correos,correosesc)
#--------------------------------

rm(clave,codes,cols,Curp,df,f,i,j,out_name,out_path,path,path1,path2,pivote,res,sede,temp,x,y)