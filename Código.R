library(tidyverse)
library(TSstudio)
library(plotly)
library(forecast)
library(magrittr)
library(lubridate)
library(zoo)
library(readxl)
library(janitor)
library(car)
library(lattice)
library(gridExtra)
library(grid)

#---- PUNTOS TEÓRICOS ---#
## Punto 1 

#---A: ACF ----#
acf<- c(1,6/13,17/26,2/13,2/13,0)
barplot(acf, las=1, main="ACF teórica",names.arg = 0:5)

#----B:PACF ----#

ARMAacf

acf_ma1 <- c(6/13,17/26,2/13,2/13,0)
pacf_ma1 <- vector()
pacf_ma1[1] <- acf_ma1[1]
for (i in 2:length(acf_ma1)){
  deno <- toeplitz(c(1,acf_ma1[1:(i-1)]))
  aux_1 <- deno
  aux_1[,i] <- acf_ma1[1:i]
  nume <- aux_1
  pacf_ma1[i] <- det(nume)/det(deno)
  barplot(pacf_ma1, las=1, main="PACF teórica", names.arg = 1:length(pacf_ma1))}

pacf_ma1
# ---C----#
set.seed(1)
sigma_w<-sqrt(4.8)
wt<-rnorm(204,0,sigma_w)
xt<-vector()
for (t in 3:202) {
  xt[t]<- wt[t-2]+ 0.5*wt[t-1]+ 2*wt[t] + 0.5*wt[t+1] + wt[t+2]
}

length(xt)
xt<- xt[3:202]
plot(xt,type = "l")


# ---D----#
acf(xt, na.action = na.pass,plot = F)
pacf(xt, na.action = na.pass)


## Punto 2

# ---C----#
polyroot(c(1,-0.9,0.6))
abs(polyroot(c(1,-0.9,0.6)))
options(scipen=100)

acf2 <- ARMAacf(ar=c(0.9,-0.6),lag.max = 20)
round(acf2,4)
barplot(acf2,ylim = c(-1,1), main = "ACF teórica AR(2)")


pacf2 <- ARMAacf(ar=c(0.9,-0.6),pacf = TRUE, lag.max = 20)
round(pacf2,4)
barplot(pacf2, ylim = c(-1,1),  main = "PACF teórica AR(2)")

# ---D----#

a0<-3.1
a1<-0.9
a2<- -0.6
sd<-sqrt(6.2)
set.seed(12)
y<-arima.sim(model = list(ar=c(a1,a2)),n=180,sd=sd)+a0 ## NO TOCAR
plot(y) ## 

acf(y)
pacf(y)


#---- PUNTOS PRÁCTICOS ---#

## Punto 3
rm(list = ls())
# -----A-----#
BD1<-read_excel("Afluencia_Metro_2019.xlsx")

colnames(BD1) <- c("FECHA", "LINEA_DE_SERVICIO",4:23,"TOTAL_PASAJEROS_DIA")
BD1 %>% head(5)  ## ENCABEZADO BASE DE DATOS
BD1 %>% tail()   ## COLA BASE DE DATOS
BD1 %>% dim()    ## DIMENSIONES BASE DE DATOS

BD2<-read_excel("Afluencia_Metro.xlsx")
colnames(BD2) <- c("FECHA", "LINEA_DE_SERVICIO",4:23,"TOTAL_PASAJEROS_DIA")
BD2 %>% head(5)
BD2 %>% tail() 
BD2 %>% dim()
BD2 %>% str()

BD3<-read_excel("Afluencia_2021.xlsx")
colnames(BD3) <- c("FECHA", "LINEA_DE_SERVICIO",4:23,"TOTAL_PASAJEROS_DIA")
BD3 %>% head(5)
BD3 %>% tail() 
BD3 %>% dim()
BD3 %>% str()


# -----B-----#
datos_juntos<-rbind(BD1,BD2,BD3)  ## BASES DE DATOS JUNTAS
datos_juntos$FECHA %<>% as.Date()
datos_juntos %>% dim()   ## DIMENSIONES BASE DE DATOS

datos_juntos$LINEA_DE_SERVICIO <- tolower(datos_juntos$LINEA_DE_SERVICIO) ## Mismo formato minusculas

# -----C-----#
datos_juntos_1 <-  gather(datos_juntos, 3:22, key="HORA",value="TOTAL_PASAJEROS_HORA")
datos_juntos_1$DIA <- day(datos_juntos_1$FECHA)  ## Día del mes 
datos_juntos_1$DIA_SEMANA <- wday(datos_juntos_1$FECHA, label = F) ## Día de la semana
wday(datos_juntos_1$FECHA, label = F) %>% as.factor()
ors <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
datos_juntos_1$DIA_SEMANA %<>% factor(.,levels = 1:7,
                                     labels = ors[c(7,1,2,3,4,5,6)])
datos_juntos_1$SEMANA <- week(datos_juntos_1$FECHA)   ## Número semana
datos_juntos_1$MES <- month(datos_juntos_1$FECHA) %>% as.factor()  ## Mes
datos_juntos_1$ANO <- year(datos_juntos_1$FECHA)  ## AÑO

datos_juntos_1<- datos_juntos_1[,c(1,4,6,7,8,9,10,5,3,2)]
datos_juntos_1$HORA <- as.factor(as.integer(datos_juntos_1$HORA))

datos_juntos_1 %<>% arrange(datos_juntos_1$FECHA)

datos_juntos_1$DIA_SEMANA %<>% as.factor() 

levels(datos_juntos_1$DIA_SEMANA)

levels(datos_juntos_1$MES)<-c("enero", "febrero","marzo","abril","mayo" ,
                              "junio","julio","agosto","septiembre","octubre",
                              "noviembre","diciembre")   ## Organizar niveles mes
levels(datos_juntos_1$MES)


str(datos_juntos_1)

# -----D-----#
### Filtrar por línea de servicio
dat_lin_A <- filter(datos_juntos_1,LINEA_DE_SERVICIO =="línea a")
dat_lin_B <- filter(datos_juntos_1,LINEA_DE_SERVICIO =="línea b")

## Ordenar por fecha

dat_lin_A %<>% arrange(dat_lin_A$FECHA)
dat_lin_B %<>% arrange(dat_lin_B$FECHA)

dat_lin_A %>% dim()
dat_lin_B %>% dim()



### Ordenar por hora
str(dat_lin_A)
dat_lin_A <- dat_lin_A[order(dat_lin_A$HORA),]
head(dat_lin_A,20)


str(dat_lin_B)
dat_lin_B <- dat_lin_B[order(dat_lin_B$HORA),]
head(dat_lin_B,20)


# -----E-----#

### Antes del 23 de marzo del 2020

## Línea A
dat_lin_A_antes_summary <- dat_lin_A[dat_lin_A$FECHA < "2020-03-23",] %>% 
  group_by(DIA_SEMANA, HORA) %>% 
  summarise(mean=mean(na.omit(TOTAL_PASAJEROS_HORA)))

dat_lin_A_antes_summary

## Línea B
dat_lin_B_antes_summary <- dat_lin_B[dat_lin_B$FECHA < "2020-03-23",] %>% 
  group_by(DIA_SEMANA, HORA) %>% 
  summarise(mean=mean(na.omit(TOTAL_PASAJEROS_HORA)))

dat_lin_B_antes_summary

### Después del 23 de marzo del 2020

## Línea A
dat_lin_A_despues_summary <- dat_lin_A[dat_lin_A$FECHA >= "2020-03-23",] %>% 
  group_by(DIA_SEMANA, HORA) %>% 
  summarise(mean=mean(na.omit(TOTAL_PASAJEROS_HORA)))

dat_lin_A_despues_summary

## Línea B
dat_lin_B_despues_summary <- dat_lin_B[dat_lin_A$FECHA >= "2020-03-23",] %>% 
  group_by(DIA_SEMANA, HORA) %>% 
  summarise(mean=mean(na.omit(TOTAL_PASAJEROS_HORA)))

dat_lin_B_despues_summary


#---- GRAFICOS -----#

### Antes del 23 de marzo del 2020

## Línea A
dat_lin_A_antes_summary$HORA %<>% as.numeric()

dat_lin_A_antes_summary$HORA <-recode(dat_lin_A_antes_summary$HORA, "1=4; 2=5; 3=6; 4=7;
                                      5=8; 6=9; 7=10; 8=11; 9=12; 10=13; 11=14; 12=15;
                                      13=16; 14=17; 15=18; 16=19; 17=20; 18=21; 19=22; 20=23")
min_a<-min(dat_lin_A_antes_summary$mean) ## Mínimo media antes de la pandemia
max_a<-max(dat_lin_A_antes_summary$mean) ## Máximo media antes de la pandemia

lin_A_antes<-ggplot(dat_lin_A_antes_summary, aes(x = HORA, y = mean, color = DIA_SEMANA)) +
  geom_line(lwd=1.1)+
  ylab("MEDIA") + 
  theme_minimal(base_size = 14, base_family = "sans")+
  labs(title = "ANTES DE PANDEMIA")
lin_A_antes

min_b<-min(dat_lin_B_antes_summary$mean)
max_b<-max(dat_lin_B_antes_summary$mean)

## Línea B

dat_lin_B_antes_summary$HORA %<>% as.numeric()

dat_lin_B_antes_summary$HORA <-recode(dat_lin_B_antes_summary$HORA, "1=4; 2=5; 3=6; 4=7;
                                      5=8; 6=9; 7=10; 8=11; 9=12; 10=13; 11=14; 12=15;
                                      13=16; 14=17; 15=18; 16=19; 17=20; 18=21; 19=22; 20=23")

lin_B_antes<-ggplot(dat_lin_B_antes_summary, aes(x = HORA, y = mean, color = DIA_SEMANA)) +
  geom_line(lwd=1.1)+
  ylab("MEDIA") + 
  theme_minimal(base_size = 14, base_family = "sans")+
  labs(title = "ANTES DE PANDEMIA")
lin_B_antes


### Después del 23 de marzo del 2020

## Línea A
dat_lin_A_despues_summary$HORA %<>% as.numeric()

dat_lin_A_despues_summary$HORA <-recode(dat_lin_A_despues_summary$HORA, "1=4; 2=5; 3=6; 4=7;
                                      5=8; 6=9; 7=10; 8=11; 9=12; 10=13; 11=14; 12=15;
                                      13=16; 14=17; 15=18; 16=19; 17=20; 18=21; 19=22; 20=23")

min(dat_lin_A_despues_summary$mean)  ## Mínimo media antes de la pandemia
max(dat_lin_A_despues_summary$mean)  ## Máximo media antes de la pandemia

lin_A_desp<-ggplot(dat_lin_A_despues_summary, aes(x = HORA, y = mean, color = DIA_SEMANA)) +
  geom_line(lwd=1.1)+
  ylab("Media") + 
  theme_minimal(base_size = 14, base_family = "sans")+
  labs(title = "DESPUÉS DE PANDEMIA")+ylim(2.153846,81557.33)
lin_A_desp

## Línea B

dat_lin_B_despues_summary$HORA %<>% as.numeric()

dat_lin_B_despues_summary$HORA <-recode(dat_lin_B_despues_summary$HORA, "1=4; 2=5; 3=6; 4=7;
                                      5=8; 6=9; 7=10; 8=11; 9=12; 10=13; 11=14; 12=15;
                                      13=16; 14=17; 15=18; 16=19; 17=20; 18=21; 19=22; 20=23")

lin_B_desp<- ggplot(dat_lin_B_despues_summary, aes(x = HORA, y = mean, color = DIA_SEMANA)) +
  geom_line(lwd=1.1)+
  ylab("Media") + 
  theme_minimal(base_size = 14, base_family = "sans")+
  labs(title = "DESPUÉS DE PANDEMIA")+ylim(1,10923.03)
lin_B_desp


#---GRÁFICOS JUNTOS---#

#--- los de la linea A---#
grid.arrange(lin_A_antes,lin_A_desp,
             ncol=2)

#--- los de la linea B---#

grid.arrange(lin_B_antes,lin_B_desp,
             ncol=2)


#---- F ----#

## LÍNEA A Y B POR EL NUMERO TOTAL DE PASAJEROS POR DÍA
datos_1<-dat_lin_A[,c(1,3,4,5,7,6,9)]
datos_1 %>% head()

datos_2<-dat_lin_B[,c(1,3,4,5,7,6,9)]
datos_2 %>% head()


#---- G ----#

## Línea A
linea_a <- datos_1[, c("FECHA", "TOTAL_PASAJEROS_DIA","MES", "ANO")] 
linea_a %>% head()
str(linea_a)


## Convirtiendo a objeto ts
first_cycle_number <- linea_a$ANO[which.min(linea_a$FECHA)]
first_cycle_unit <- linea_a$MES[which.min(linea_a$FECHA)]

print(c(first_cycle_number, first_cycle_unit))

final_cycle_number <- linea_a$ANO[which.max(linea_a$FECHA)]
final_cycle_unit <- linea_a$MES[which.max(linea_a$FECHA)]


print(c(final_cycle_number, final_cycle_unit))


A <- ts(data = linea_a$TOTAL_PASAJEROS_DIA,
             start = c(first_cycle_number, first_cycle_unit),
             end =c(final_cycle_number, final_cycle_unit), 
             frequency = 365)

## GRÁFICO SERIE
linea_a %>% ggplot(aes(x=FECHA, y=TOTAL_PASAJEROS_DIA))+
  geom_line()


# oscilan al rededor de 1000.000 con una variabilidad constante, luego se ve un punto de ruptura,
#oscila entre dos valores 

pre_linea_a<-ts_plot(A,
                    title = "Total pasajeros por día- Línea A",
                    Xtitle = "Total pasajeros por día- Línea A",
                    Ytitle = "Número de pasajeros",
                    slider = TRUE,
                    Xgrid = TRUE,
                    Ygrid = TRUE,
                    color = "blue")

pre_linea_a  ## Gráfico ts_plot

Linea_a_decompose <- decompose(A)  ## Descomponer serie
plot(Linea_a_decompose)

#### Gráficos de la ACF y PACF muestrales.

acf(A)
pacf(A)

## Línea B

linea_b <- datos_2[, c("FECHA", "TOTAL_PASAJEROS_DIA","MES", "ANO")] 
linea_b %>% head()
str(linea_b)

## Convirtiendo a objeto ts
first_cycle_number <- linea_b$ANO[which.min(linea_b$FECHA)]
first_cycle_unit <- linea_b$MES[which.min(linea_b$FECHA)]

print(c(first_cycle_number, first_cycle_unit))

final_cycle_number <- linea_b$ANO[which.max(linea_b$FECHA)]
final_cycle_unit <- linea_b$MES[which.max(linea_b$FECHA)]


print(c(final_cycle_number, final_cycle_unit))


B <- ts(data = linea_b$TOTAL_PASAJEROS_DIA,
        start = c(first_cycle_number, first_cycle_unit),
        end =c(final_cycle_number, final_cycle_unit), 
        frequency = 365)

linea_b %>% ggplot(aes(x=FECHA, y=TOTAL_PASAJEROS_DIA))+
  geom_line()

plot(B)

# oscilan al rededor de 1000.000 con una variabilidad constante, luego se ve un punto de ruptura,
#oscila entre dos valores 

pre_linea_b<-ts_plot(B,
                     title = "Línea B",
                     Xtitle = "Total pasajeros por día - Línea B",
                     Ytitle = "Número de pasajeros",
                     #slider = TRUE,
                     Xgrid = TRUE,
                     Ygrid = TRUE,
                     color = "blue"
)

pre_linea_b   ## Gráfico ts_plot

Linea_b_decompose <- decompose(B)
plot(Linea_b_decompose)

#### Gráficos de la ACF y PACF muestrales.

acf(B)
pacf(B)


#---- H ----#

### Antes del 23 de marzo del 2020

## Línea A
datos_antes_A <- dat_lin_A[dat_lin_A$FECHA < "2020-03-23",]

datos_antes_A_1<-datos_antes_A %>% ggplot(aes(x=FECHA, y=TOTAL_PASAJEROS_DIA))+
  geom_line(color="blue") +geom_hline(yintercept =mean(datos_antes_A$TOTAL_PASAJEROS_DIA), color="black")+
  labs(title = "Línea A antes de pandemia")+
  ylab("Total pasajeros día")+
  xlab("Fecha")
datos_antes_A_1  ## Gráfico estacionalidad

## Convirtiendo a objeto ts
first_cycle_number <- datos_antes_A$ANO[which.min(datos_antes_A$FECHA)]
first_cycle_unit <- datos_antes_A$MES[which.min(datos_antes_A$FECHA)]

print(c(first_cycle_number, first_cycle_unit))

final_cycle_number <- datos_antes_A$ANO[which.max(datos_antes_A$FECHA)]
final_cycle_unit <- datos_antes_A$MES[which.max(datos_antes_A$FECHA)]

print(c(final_cycle_number, final_cycle_unit))

ANTES_A <- ts(data = datos_antes_A$TOTAL_PASAJEROS_DIA,
              start = c(first_cycle_number, first_cycle_unit),
              end =c(final_cycle_number, final_cycle_unit), 
              frequency = 365)


PRE_ANTES_A<-ts_plot(ANTES_A,
                     Xtitle = "Línea A: antes del 23 de marzo del 2020",
                     Ytitle = "Número de pasajeros",
                     #slider = TRUE,
                     Xgrid = TRUE,
                     Ygrid = TRUE,
                     color = "blue"
)

PRE_ANTES_A   ## Gráfico ts_plot



#oscila alrededor de un mismo valor con varianza constante 

##LÍNEA B

datos_antes_B <- dat_lin_B[dat_lin_B$FECHA<"2020-03-23",] 

datos_antes_B_1<-datos_antes_B %>% ggplot(aes(x=FECHA, y=TOTAL_PASAJEROS_DIA))+
  geom_line(color="blue") +geom_hline(yintercept =mean(datos_antes_B$TOTAL_PASAJEROS_DIA), color="black")+
  labs(title = "Línea B antes de pandemia")+
  ylab("Total pasajeros día")+
  xlab("Fecha")
datos_antes_B_1  ## Gráfico estacionalidad

#--- los de la linea A y B ANTES---#
grid.arrange(datos_antes_A_1,datos_antes_B_1,
             ncol=2)

## Convirtiendo a objeto ts
first_cycle_number <- datos_antes_B$ANO[which.min(datos_antes_B$FECHA)]
first_cycle_unit <- datos_antes_B$MES[which.min(datos_antes_B$FECHA)]

print(c(first_cycle_number, first_cycle_unit))

final_cycle_number <- datos_antes_B$ANO[which.max(datos_antes_B$FECHA)]
final_cycle_unit <- datos_antes_B$MES[which.max(datos_antes_B$FECHA)]

print(c(final_cycle_number, final_cycle_unit))

ANTES_B <- ts(data = datos_antes_B$TOTAL_PASAJEROS_DIA,
              start = c(first_cycle_number, first_cycle_unit),
              end =c(final_cycle_number, final_cycle_unit), 
              frequency = 365)


PRE_ANTES_B<-ts_plot(ANTES_B,
                     Xtitle = "Línea B: antes del 23 de marzo del 2020",
                     Ytitle = "Número de pasajeros",
                     #slider = TRUE,
                     Xgrid = TRUE,
                     Ygrid = TRUE,
                     color = "blue"
)

PRE_ANTES_B  ## Gráfico ts_plot


### Después del 23 de marzo del 2020

### Línea A
datos_depues_A <- dat_lin_A[dat_lin_A$FECHA >= "2020-03-23",] 

datos_despues_A_1<-datos_depues_A %>% ggplot(aes(x=FECHA, y=TOTAL_PASAJEROS_DIA))+
  geom_line(color="blue")+geom_smooth(method = "lm", color="black") +
  labs(title = "Línea A después de pandemia")+
  ylab("Total pasajeros día")+
  xlab("Fecha")  ## Gráfico tendencia

## Convirtiendo a objeto ts
first_cycle_number <- datos_depues_A$ANO[which.min(datos_depues_A$FECHA)]
first_cycle_unit <- datos_depues_A$MES[which.min(datos_depues_A$FECHA)]

print(c(first_cycle_number, first_cycle_unit))

final_cycle_number <- datos_depues_A$ANO[which.max(datos_depues_A$FECHA)]
final_cycle_unit <- datos_depues_A$MES[which.max(datos_depues_A$FECHA)]

print(c(final_cycle_number, final_cycle_unit))

DESPUES_A <- ts(data = datos_depues_A$TOTAL_PASAJEROS_DIA,
                start = c(first_cycle_number, first_cycle_unit),
                end =c(final_cycle_number, final_cycle_unit), 
                frequency = 365)

PRE_DESPUES_A<-ts_plot(DESPUES_A,
                       Xtitle = "Línea A: después del 23 de marzo del 2020",
                       Ytitle = "Número de pasajeros",
                       #slider = TRUE,
                       Xgrid = TRUE,
                       Ygrid = TRUE,
                       color = "blue"
)

PRE_DESPUES_A  ## Gráfico ts_plot

### Línea B 
datos_despues_B <- dat_lin_B[dat_lin_B$FECHA >= "2020-03-23",] 

datos_despues_B_1<-datos_despues_B %>% ggplot(aes(x=FECHA, y=TOTAL_PASAJEROS_DIA))+
  geom_line(color="blue")+geom_smooth(method = "lm", color="black") +
  labs(title = "Línea B después de pandemia")+
  ylab("Total pasajeros día")+
  xlab("Fecha")   ## Gráfico tendencia

#--- los de la linea A y B DESPUES---#
grid.arrange(datos_despues_A_1,datos_despues_B_1,
             ncol=2)

## Convirtiendo a objeto ts
first_cycle_number <- datos_despues_B$ANO[which.min(datos_despues_B$FECHA)]
first_cycle_unit <- datos_despues_B$MES[which.min(datos_despues_B$FECHA)]

print(c(first_cycle_number, first_cycle_unit))

final_cycle_number <- datos_despues_B$ANO[which.max(datos_despues_B$FECHA)]
final_cycle_unit <- datos_despues_B$MES[which.max(datos_despues_B$FECHA)]

print(c(final_cycle_number, final_cycle_unit))

DESPUES_B <- ts(data = datos_despues_B$TOTAL_PASAJEROS_DIA,
                start = c(first_cycle_number, first_cycle_unit),
                end =c(final_cycle_number, final_cycle_unit), 
                frequency = 365)

plot(DESPUES_B)

PRE_DESPUES_B<-ts_plot(DESPUES_B,
                       Xtitle = "Línea B: después del 23 de marzo del 2020",
                       Ytitle = "Número de pasajeros",
                       #slider = TRUE,
                       Xgrid = TRUE,
                       Ygrid = TRUE,
                       color = "blue"
)

PRE_DESPUES_B  ## Gráfico ts_plot



#---- I ----#
#### Un modelamiento de la tendencia y de la estacionalidad con la función lm.


### ANTES

## Línea A
datos_antes_A %<>%  mutate(t = c(1:nrow(datos_antes_A)))

mod_A_antes <- lm(TOTAL_PASAJEROS_DIA ~ t+DIA_SEMANA ,data = dat_lin_A_antes) 
summary(mod_A_antes)


## Línea B
datos_antes_B %<>%  mutate(t = c(1:nrow(datos_antes_B)))

mod_B_antes <- lm(TOTAL_PASAJEROS_DIA ~ t+DIA_SEMANA ,data = datos_antes_B) 
summary(mod_B_antes)

datos_antes_B$DIA_SEMANA %>% levels()

### Después

## Línea A
datos_depues_A %<>%  mutate(t = c(1:nrow(datos_depues_A)))

mod_A_despues <- lm(TOTAL_PASAJEROS_DIA ~ t+DIA_SEMANA ,data = datos_depues_A) 
summary(mod_A_despues)


## Línea B
datos_despues_B %<>%  mutate(t = c(1:nrow(datos_despues_B)))
class(datos_despues_B$MES)

mod_B_despues <- lm(TOTAL_PASAJEROS_DIA ~ t+DIA_SEMANA ,data = datos_despues_B) 
summary(mod_B_despues)





