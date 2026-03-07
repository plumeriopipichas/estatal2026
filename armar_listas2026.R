setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal2026/estatal_2026_RProject")

library(readxl)
library(dplyr)
library(stringr)
library(tools)

source("funciones_adhoc.R")


sedes <- c("Actopan","Huejutla","Huichapan","Ixmiquilpan","Ixtlahuaco","Metztitlan","Pachuca ITESM",
           "Pachuca UAEH","Pachuca CECYTEH","Pisaflores","Sahagun","Tizayuca",
           "Tlanchinol","Tula","Tulancingo","Zimapan")


#Convertir a csv las listas de registro de escuela

input_escuelas<-"../listas_crudas/Registro_escuelas"
output_escuelas<-"../listas_crudas/csv_escuelas"

temp<-list.files(input_escuelas,pattern="\\.xlsx$",full.names = TRUE)

for (f in temp){
  df<-read_excel(f,sheet = 1, skip =2)
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
  }
  else{
    escuela_por_sede[[temp]]<-data.frame()
    basica_sede[[temp]]<-data.frame()
  }
}

lista_general_registro <- juntar_bases(basica_sede)

write.csv(lista_general_registro,
          "../listas_generadas/lista_general_registro.csv",row.names = FALSE)

rm(df,f,out_name,out_path,temp,x,y)