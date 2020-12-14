library(tidyverse)
library(dplyr)
library(tidyr)
library(rmarkdown)

### (1) Cargamos las Bases de Datos ###
setwd("~/CLASE BIG DATA")
GrandesChile <- read_csv2("grandes_chile.csv")
GrandesColombia <- read_csv2("grandes_colombia.csv")
GrandesPeru <- read_csv2("grandes_peru.csv")
MedianaChile <- read_csv2("medianas_chile.csv")
MedianaColombia <- read_csv2("medianas_colombia.csv")
MedianaPeru <- read_csv2("medianas_peru.csv")
MicroChile <- read_csv2("micro_chile.csv")
MicroColombia <- read_csv2("micro_colombia.csv")
MicroPeru <- read_csv2("micro_peru.csv")
PequenaChile <- read_csv2("pequena_chile.csv")
PequenaColombia <- read_csv2("pequena_colombia.csv")
PequenaPeru <- read_csv2("pequena_peru.csv")

### (1) Agregamos Columna Tamaño ###
GrandesChi <- as.data.frame(mutate(GrandesChile, tamaño = "grande"))
GrandesCol <- as.data.frame(mutate(GrandesColombia, tamaño = "grande"))
GrandesPer <- as.data.frame(mutate(GrandesPeru, tamaño = "grande"))
MedianaChi <- as.data.frame(mutate(MedianaChile, tamaño = "mediana"))
MedianaCol <- as.data.frame(mutate(MedianaColombia, tamaño = "mediana"))
MedianaPer <- as.data.frame(mutate(MedianaPeru, tamaño = "mediana"))
MicroChi <- as.data.frame(mutate(MicroChile, tamaño = "micro"))
MicroCol <- as.data.frame(mutate(MicroColombia, tamaño = "micro"))
MicroPer <- as.data.frame(mutate(MicroPeru, tamaño = "micro"))
PequenoChi <- as.data.frame(mutate(PequenaChile, tamaño = "pequeño"))
PequenoCol <- as.data.frame(mutate(PequenaColombia, tamaño = "pequeño"))
PequenoPer <- as.data.frame(mutate(PequenaPeru, tamaño = "pequeño"))

### (2) Renombrando Columnas por problemas de unión ###

GrandesChiRen <- GrandesChi %>%
  rename(PorcentajeMujeres = procentaje_mujeres)
GrandesColRen <- GrandesCol %>%
  rename(PorcentajeMujeres = procentaje_mujeres)
GrandesPerRen <- GrandesPer %>%
  rename(PorcentajeMujeres = procentaje_muejeres)
MedianaChiRen <- MedianaChi %>%
  rename(PorcentajeMujeres = procentaje_mujeres)
MedianaColRen <- MedianaCol %>%
  rename(PorcentajeMujeres = porcentaje_mujeres)
MedianaPerRen <- MedianaPer %>%
  rename(PorcentajeMujeres = procentaje_muejeres)
MicroChiRen <- MicroChi %>%
  rename(PorcentajeMujeres = procentaje_muejeres)
MicroColRen <- MicroCol %>%
  rename(PorcentajeMujeres = porcentaje_mujeres)
MicroPerRen <- MicroPer %>%
  rename(PorcentajeMujeres = porcentaje_mujeres)
PequenoChiRen <- PequenoChi %>%
  rename(PorcentajeMujeres = porcentaje_mujeres)
PequenoColRen <- PequenoCol %>%
  rename(PorcentajeMujeres = porcentaje_mujeres)
PequenoPerRen <- PequenoPer %>%
  rename(PorcentajeMujeres = procentaje_mujeres)

### (2) Reunir todas las bases en una sola ###

GrandesChiRen %>%
  union_all(GrandesColRen) %>% 
  union_all(GrandesPerRen) %>% 
  union_all(MedianaChiRen) %>% 
  union_all(MedianaColRen) %>% 
  union_all(MedianaPerRen) %>% 
  union_all(MicroChiRen) %>% 
  union_all(MicroColRen) %>% 
  union_all(MicroPerRen) %>% 
  union_all(PequenoChiRen) %>% 
  union_all(PequenoColRen) %>% 
  union_all(PequenoPerRen)-> carac_empresas

### (2) Tipologia de los datos ###
names(carac_empresas)
str(carac_empresas$fecha) ## Carácter
str(carac_empresas$pais) ## Carácter
str(carac_empresas$ingresos) ## Numérico
str(carac_empresas$costos) ## Numérico
str(carac_empresas$PorcentajeMujeres) ## Numérico
str(carac_empresas$exportaciones) ## Numérico
str(carac_empresas$importaciones) ## Numérico
str(carac_empresas$endeudamiento) ## Numérico
str(carac_empresas$morosidad) ## Numérico
str(carac_empresas$reservas) ## Numérico
str(carac_empresas$spread) ## Numérico
str(carac_empresas$tasa_interes) ## Numérico
str(carac_empresas$tamaño) ## Carácter 

### (3) Obervaciones de Perú vs Chile ###
pais <- dplyr::select(carac_empresas, pais)

Observaciones <- "chileperu"

if (Observaciones == "chileperu"){
  Chi <- paged_table(carac_empresas %>% 
                       filter(pais == "chile")) %>% count(pais)
  Per <- paged_table(carac_empresas %>% 
                       filter(pais == "peru")) %>% count(pais)
  print(paste("Chile tiene",Chi[-1],"observaciones"))
  print(paste("Perú tiene",Per[-1],"observaciones"))
  ChiGra <- filter(carac_empresas, tamaño %in% c("grande"), pais %in% c("chile")) %>% count(tamaño)
  ChiMed <- filter(carac_empresas, tamaño %in% c("mediana"), pais %in% c("chile")) %>% count(tamaño)
  ChiMic <- filter(carac_empresas, tamaño %in% c("micro"), pais %in% c("chile")) %>% count(tamaño)
  ChiPeq <- filter(carac_empresas, tamaño %in% c("pequeño"), pais %in% c("chile")) %>% count(tamaño)
  PerGra <- filter(carac_empresas, tamaño %in% c("grande"), pais %in% c("peru")) %>% count(tamaño)
  PerMed <- filter(carac_empresas, tamaño %in% c("mediana"), pais %in% c("peru")) %>% count(tamaño)
  PerMic <- filter(carac_empresas, tamaño %in% c("micro"), pais %in% c("peru")) %>% count(tamaño)
  PerPeq <- filter(carac_empresas, tamaño %in% c("pequeño"), pais %in% c("peru")) %>% count(tamaño)
  print(paste("De las",Chi[-1],"observaciones que tiene Chile,",ChiGra[-1], "son de tamaño grande,", 
              ChiMed[-1], "son de tamaño mediano,", ChiMic[-1], "son de tamaño micro y",
              ChiPeq[-1],"son de tamaño pequeño"))
  print(paste("Por otro lado, de las",Per[-1],"observaciones que tiene Perú,",PerGra[-1], "son de tamaño grande,", 
              PerMed[-1], "son de tamaño mediano,", PerMic[-1], "son de tamaño micro y",
              PerPeq[-1],"son de tamaño pequeño"))           
}

### (4) país con mayor ingreso de explotación para los años que considera la muestra ###

ingchile <- carac_empresas %>% select(ingresos, pais) %>%
  filter(pais == "chile") 
ingperu <- carac_empresas %>% select(ingresos, pais) %>%
  filter(pais == "peru")
ingcol <- carac_empresas %>% select(ingresos, pais) %>%
  filter(pais == "colombia") 

VectorIngresos <- c(sum(ingperu[1]), sum(ingcol[1]),sum(ingchile[1]))

### Ordenamos los Números ###
numeroingresos <- function(VectorIngresos){
  if(VectorIngresos[1] > VectorIngresos[2]){
    temporal <- VectorIngresos[1]
    VectorIngresos[1] <- VectorIngresos[2]
    VectorIngresos[2] <- temporal
  } 
  if(VectorIngresos[2] > VectorIngresos[3]){
    temporal <- VectorIngresos[2]
    VectorIngresos[2] <- VectorIngresos[3]
    VectorIngresos[3] <- temporal
  } 
  if(VectorIngresos[1] > VectorIngresos[2]){
    temporal <- VectorIngresos[1]
    VectorIngresos[1] <- VectorIngresos[2]
    VectorIngresos[2] <- temporal
  } 
  if(VectorIngresos[2] > VectorIngresos[3]){
    temporal <- VectorMuestra[2]
    VectorIngresos[2] <- VectorIngresos[3]
    VectorIngresos[3] <- temporal
  }
  return(VectorIngresos)
}

### Vector Ordenado ###
vec <- numeroingresos(VectorIngresos)
print(paste("De acuerdo a los ingresos de explotación durante los años 2012-2017, el país Chile es el que menos ingresos tuvo con",
            vec[1], ", luego el país Perú solo recaudó",vec[2],", y por último, el país con mayor cantidad ingresos fue Colombia con",
            vec[3]))


### (5)  si el país es Chile multiplique la tasa de interes por 0,1, 
### cuando sea Peru le sume 0,3 y, y finalmente si es Colombia divida por 10 ###

tasaChile <-  carac_empresas %>% filter(pais == "chile") %>% 
              mutate(TasaNueva = tasa_interes * 0.1)
tasaPeru <-  carac_empresas %>% filter(pais == "peru") %>% 
              mutate(TasaNueva = tasa_interes + 0.3)
tasaColombia <-  carac_empresas %>% filter(pais == "colombia") %>% 
              mutate(TasaNueva = tasa_interes / 10)
  
  
carac_empresas <- rbind(tasaChile, tasaPeru , tasaColombia) 

### Reemplace en la columna exportaciones con 1 cuando es mayor a 2,1, con un 2
### cuando es menor 2,1y un 3 cuando es igual a 2,1, redondee al primer decimal la variable


exp1 <-  carac_empresas %>% filter(round(carac_empresas[6], 1) > 2.1) %>% 
             mutate(exportaciones = 1)
exp2 <-  carac_empresas %>% filter(round(carac_empresas[6], 1) < 2.1) %>% 
             mutate(exportaciones = 2)
exp3 <-  carac_empresas %>% filter(round(carac_empresas[6], 1) = 2.1) %>% 
             mutate(exportaciones = 3)

Caracteristicas <- rbind(exp1, exp2)


### Gráfique algunas variables seleccionadas, las cuales puedan responder a una
### pregunta que se haga con respecto a los datos.

ggplot(data = Caracteristicas, aes(x=tamaño, y=tasa_interes)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + xlab("Tamaño de Empresa")

## De acuerdo al Box-Plot, se puede inferir que las tasas de interes estan mas concentradas 
## en las empresas grandes, presentando algunos outliers dentro de las observaciones, y por otro lado, 
## que sus tasas son mas bajas respecto a empresas de menor tamaño.
## Por su parte las empresas micro, presentan datos mas dispersos y con una media superior a las empresas
## grandes, medianas y pequeñas.

ggplot(data = Caracteristicas, aes(x=ingresos)) + geom_histogram()

## La mayor concentracion de ingresos se ubica cercano a los 12 y 13 millones, por otro lado,
## se puede decir que la muestra esta sesgada y presenta datos asimetricos hacia la derecha.










