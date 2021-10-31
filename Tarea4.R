# Tarea 4 curso Fundamento de Ciencia de Datos Ocean Teacher

# Tarea Curso Ciencia de Datos: Visualización
# 
# 1.Descargar los 2 conjuntos de datos
# 2.Hacer agregación de información por municipio/mes
# 3.Elaborar una visualización que permita encontrar si hay correlación directa/indirecta entre las variables expuestas
# 4.Crear una visualización que permita a través de filtros de años, ver el top 5 de los municipios con el valor promedio más alto para las variables estudiadas
# 5.Crear visualización que presente el comportamiento histórico de los datos por municipio de tal manera que se evidencien patrones temporales climáticos
# 6.De forma paralela, mostrar espacialmente los puntos de los sitios indicando para cada uno de ellos el valor máximo alcanzado

############################
# Datos de precipitación
### 1. Carga de datos precipitacion y temperatura (exploración)
prep_df <- read.table("Precip_MAG.csv",sep=',',header=T)
temp_df <- read.table("Temp_MAG.csv",sep=',',header=T)

#Agrega un columna con la fecha en formato Date
prep_df$Fecha_d <- as.Date(substr(prep_df$FechaObservacion, 0, 10),format = "%m/%d/%Y")
temp_df$Fecha_d <- as.Date(substr(temp_df$FechaObservacion, 0, 10),format = "%m/%d/%Y")

#prep_dfsub <- subset(prep_df, Fecha_d > as.Date("2015-12-31") & Fecha_d < as.Date("2017-01-01"))

#Agrega una columna con fecha solo año y mes
prep_df$YearMonth <-format(prep_df$Fecha_d,format='%Y-%m')
temp_df$YearMonth <-format(temp_df$Fecha_d,format='%Y-%m') 

#Extrae columnas de interes : Municipio Año-mes y Valor
df_pre <- prep_df[c('Municipio', 'YearMonth', 'ValorObservado')] 
df_tem <- temp_df[c('Municipio', 'YearMonth',  'ValorObservado')] 

#Crea una columna adicional con el nombre de la variable medida
df_pre$Variable <- 'Precipitacion'
df_tem$Variable <- 'Temperatura'

# Concatena los dos conjuntos de datos
df_tot <- rbind(df_pre,df_tem)

#df_tot2 <-df_tot[df_tot$Municipio=='EL BANCO',]
# Redimensiona el data frame largo a ancho
library(reshape2)
datos2 <- dcast(df_tot, Municipio + YearMonth  ~ Variable, value.var="ValorObservado",fun.aggregate = mean)
datos3 <- na.omit(datos2)

#Grafico scatterplot para observar correlación entre las variables
library(ggplot2)
ggplot(datos3, aes(x=Precipitacion, y=Temperatura)) + geom_point() + geom_smooth(method='lm', formula= y~x) + facet_grid(~ Municipio)
ggsave(filename='Correlaciones.png',width = 18,height = 12,dpi=300)




# 4.Crear una visualización que permita a través de filtros de años,
# ver el top 5 de los municipios con el valor promedio más alto para las variables estudiadas

datos3$Year <- substr(datos3$YearMonth,1,4)
library(dplyr)
datos4 <- datos3 %>%  group_by(Municipio,Year) %>% summarise(prec_prom = mean(Precipitacion),
                                                   tem_prom = mean(Temperatura))
val_year <- 2018
datos5 <- datos4[datos4$Year==val_year,] %>% arrange(desc(prec_prom))
datos6 <- datos4[datos4$Year==val_year,] %>% arrange(desc(tem_prom))

ggplot(datos5[1:5,], aes(reorder(x=Municipio,prec_prom), y=prec_prom)) + geom_bar(stat = "identity",color= 'white',fill="blue") +
  xlab("Municipio")+ylab("Precipitación Promedio 2018") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=8,colour="black")) + 
  geom_text(data=datos5[1:5,],aes(x=Municipio,y=prec_prom+0.005,label=round(prec_prom,3)),hjust=0,size=4)+coord_flip()
ggsave(filename='TopMunicipios2018_prec.png',width = 12,height = 8,dpi=300)

ggplot(datos6[1:5,], aes(reorder(x=Municipio,tem_prom), y=tem_prom)) + geom_bar(stat = "identity",color= 'white',fill="blue") +
  xlab("Municipio")+ylab("Temperatura Promedio 2018") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=8,colour="black")) + 
  geom_text(data=datos6[1:5,],aes(x=Municipio,y=tem_prom+1,label=round(tem_prom,1)),hjust=0,size=4)+coord_flip()
ggsave(filename='TopMunicipios2018_tem.png',width = 12,height = 8,dpi=300)


#datos4[order(datos4[datos4$Year==2017,]$prec_prom,decreasing = TRUE),]
#dat_prec <- order(data4$prec_prom)


