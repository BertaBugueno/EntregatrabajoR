

#tarea 2 


#CARGAR PAQUETES##

pacman::p_load(tidyverse, #Conjunto de paquetes, sobre todo dplyr y ggplot2
               car, #Para recodificar
               haven,
               summarytools, #Para descriptivos
               sjmisc,
               psych,
               sjlabelled,
               sjPlot,
               corrplot,
               kableExtra

)

##CARGAR BASE DE DATOS#

Base_de_datos_EBS_2021_STATA <- read_dta("Imput/Base de datos EBS 2021 STATA.dta")


##selección de variables ##
#Se trabajará con variables nivel educacional, satisfacción de ingresos, satisfacción de vivienda
#educación ingresos,zona, la encuesta EBS, contiene estas variables que se corresponden 
#lo que esta investigación se plantea dilucidar#


sjlabelled::get_label(proc_data)

#Cambiar nombres y etiquetas
# Se siguen los pasos para el cambio de etiquetas, las que quedarán finalmente como; nivel educacional
#satisfacción con los ingresos, satisfacción con la vivienda, efecto de la educación sobre los ingresos, zona
proc_data = Base_de_datos_EBS_2021_STATA %>% select(e6a, a3_1, a3_3, a3_7, d1_1, zona)

#e6a se toma de la base de datos del 2020 porque la del 2021 tiene muchos casos perdidos 
proc_data = proc_data %>% rename("nivel_educ"=e6a, "satisniveleducacional"=a3_1, "satis_ingreso"=a3_3, "satis_vivienda"=a3_7, "educ_ingresos"=d1_1, "zona"=zona)

proc_data$nivel_educ = set_label(x = proc_data$nivel_educ,label = "nivel educacional")
proc_data$satisniveleducacional = set_label(x = proc_data$satisniveleducacional, label = "satisfacción con el  nivel educacional")
proc_data$satis_ingreso = set_label(x = proc_data$satis_ingreso,label = "satisfacción con los ingresos")
proc_data$satis_vivienda = set_label(x = proc_data$satis_vivienda,label = "satisfacción con la vivienda")
proc_data$educ_ingresos = set_label(x = proc_data$educ_ingresos,label = "efecto de la educación sobre los ingresos")
proc_data$zona = set_label(x = proc_data$zona,label = "zona")



#TABLA DESCRIPTIVA GENERAL#


view(dfSummary(proc_data, headings=FALSE))

#OPERALIZACIÓN DE VARIABLE#

frq(proc_data$satisniveleducacional)
frq(proc_data$satis_ingreso)
frq(proc_data$satis_vivienda)
frq(proc_data$educ_ingresos) #tiene NA#

proc_data =na.omit(proc_data)#omitir casos perdidos de toda la base (educ, ingreso era la única que tenía NA) 


##Recodificación de variables

#Variable binaria de zona

frq(proc_data$zona)
proc_data$zona <- car::recode(proc_data$zona, "1=0;2=1")

proc_data$zona <- as.numeric(proc_data$zona,
                         labels=c( "Urbano",
                                   "Rural"),
                         levels=c(0,1)


proc_data$zona <- set_labels(proc_data$zona,
                                     labels=c( "Urbano"=0,
                                               "Rural"=1))
frq(proc_data$zona)
frq(proc_data$nivel_educ)

proc_data$nivel_educ <- car::recode(proc_data$nivel_educ, "c(1,2,3,4)=1; c(5)=2; c(6,7)=3; c(8,9,10,11)=4; c(12,13,14,15,16,17)=5")

proc_data$nivel_educ <- set_labels(proc_data$nivel_educ,
                             labels=c( "Analfabeto"=1,
                                       "Educación especial"=2,
                                       "Educación básica"=3,
                                       "Educación media"=4,
                                       "Educación superior"=5))

#HACER GRÁFICOS
proc_data %>% ggplot(aes(x = nivel_educ)) + 
  geom_bar()

proc_data %>% ggplot(aes(x = satis_ingreso)) + 
  geom_bar()

proc_data %>% ggplot(aes(x = satis_vivienda)) + 
  geom_bar()





#TAREA 3
corrplot.mixed(cor(select(proc_data,nivel_educ,satis_vivienda,satis_ingreso,satisniveleducacional,educ_ingresos,zona),
                   use = "complete.obs"))
####INTERPRETAR GRAFICO DE PELOTITAS EN QUARTO#### n(se llama interpretación de la asociación)

#Cargar paquetes
pacman::p_load(tidyverse, #Conjunto de paquetes, sobre todo dplyr y ggplot2
               car, #Para recodificar
               haven,
               summarytools, #Para descriptivos
               sjmisc,
               psych,
               sjlabelled,
               sjPlot,
               corrplot
               
#Cargar base de datos
Base_de_datos_EBS_2021_STATA <- read_dta("Imput/Base de datos EBS 2021 STATA.dta")

#Selección de variables

#Se trabajará con variables nivel educacional, satisfacción con el nivel educacional,  satisfacción de ingresos, efecto de la educación sobre los ingresos, satisfacción de vivienda, y zona
#educación ingresos, en la encuesta EBS, contiene estas variables que se corresponden o contienen lo que esta investigación se plantea dilucidar. En esta etapa se desarrollará nivel educacional


##TAREA 4##
pacman::p_load(dplyr, 
               car, 
               summarytools, 
               sjPlot, 
               texreg, 
               corrplot, 
               ggplot2, 
               sjlabelled, 
               fastDummies, 
               ggeffects)
#Tabla descriptiva
proc_data$zona <- set_labels(proc_data$zona,
                             labels=c( "Urbano"=0,
                                       "Rural"=1))
view_df(proc_data,max.len = 50)

######plot_stackfrq(proc_data[,c("part01","part02","part03","part04")]) + theme(legend.position="bottom")

corrplot.mixed(cor(select(proc_data,nivel_educ,satis_vivienda,satis_ingreso,satisniveleducacional,educ_ingresos,zona),
                   use = "complete.obs"))

###REGRESIÓN LINEAL###
fit01<- lm(nivel_educ~zona,data=proc_data)
fit02<- lm(satis_ingreso~zona,data=proc_data)
fit03<- lm(educ_ingresos~zona,data=proc_data)
                              
# htmlreg para que se vea en el sitio web
knitreg(list(fit01,fit02,fit03),
        custom.model.names = c("Modelo 1","Modelo 2","Modelo 3")
