# Bienvenido al script de análisis de ingreso y estratificación en Colombia.
# Puede encontrar el artículo explicativo en http://vasijadeideas.blogspot.com/2015/10/por-que-las-tarifas-de-los-servicios.html
# Por: Norman Simón Rodríguez
# Versión 17 de octubre de 2015


# Líneas de pobreza e indigencia
# Fuente 2002 a 2010: "Misión para el Empalme de las Series de Empleo,
# Pobreza y Desigualdad (mesep)
# Pobreza monetaria en Colombia: Nueva metodología y cifras 2002-2010
# Resultados segunda fase de la Mesep"
# Link: https://colaboracion.dnp.gov.co/CDT/Desarrollo%20Social/Documento%20Mesep%20segunda%20fase_26-03-2012_FINAL.pdf
# Página 92
# Fuente 2011 a 2014: "Pobreza monetaria y Multidimensional 2014" (Anexos)
# Link: http://www.dane.gov.co/index.php/esp/estadisticas-sociales/pobreza/160-uncategorised/6020-pobreza-monetaria-y-multidimensional-2014
# Pesos corrientes mensuales por persona
lpobreza <- read.csv("./datos/lpobreza.csv", dec=",")
#
# Definición de las clases sociales (en pesos de 2014)
# Muy baja: Ingresos menores a la línea de indigencia.
muybaja <- c(0, lpobreza$li.nac[which(lpobreza$anho==2014)])
# Baja: Ingresos menores a la línea de pobreza.
baja <- c(muybaja[2]+1, lpobreza$lp.nac[which(lpobreza$anho==2014)])
# Media baja: Ingresos menores a 6 veces la línea de pobreza.
mediabaja <- c(baja[2]+1, 6*lpobreza$lp.nac[which(lpobreza$anho==2014)])
# Media alta: Ingresos menores a 12 veces la línea de pobreza.
mediaalta <- c(mediabaja[2]+1, 12*lpobreza$lp.nac[which(lpobreza$anho==2014)])
# Alta: Ingresos menores a 24 veces la línea de pobreza.
alta <- c(mediaalta[2]+1, 24*lpobreza$lp.nac[which(lpobreza$anho==2014)])
# Muy alta: Ingresos mayores a 24 veces la línea de pobreza.
muyalta <- c(alta[2]+1, NA)
#
clases2014 <- data.frame(muybaja, baja, mediabaja, mediaalta, alta, muyalta)
clases2014 <- t(clases2014)
clases2014 <- data.frame(clases2014)
colnames(clases2014) <- c("min", "max")


# Funciones para repetir los vectores
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
# Datos de personas del DANE (GEIH)
# http://formularios.dane.gov.co/Anda_4_1/index.php/catalog/334/study-description
personas2014 <- read.csv("./datos/Personas 2014 small.txt") #Lee el archivo base.
gc()
personas2014 <- personas2014[,c("Directorio","Secuencia_p", "Estrato1")]
gc()

# Datos de hogares del DANE (GEIH)
# http://formularios.dane.gov.co/Anda_4_1/index.php/catalog/334/study-description
hogares2014 <- read.csv("./datos/Hogares 2014 small.txt")
hogares2014 <- hogares2014[,c("Directorio", "Secuencia_p", "Ingpcug", "Fex_c")]


# Tabla de hogares, estrato e ingreso per capita del hogar
data2014 <- merge(hogares2014, personas2014)
data2014 <- unique(data2014)

# Se redondea el factor de expansión para facilitar el cálculo.
data2014$Fex_c_round <- round(data2014$Fex_c)


# Creación de vectores de personas (ingresos mensuales) por estrato.
es1 <- subset(data2014, data2014$Estrato1==1)
es1 <- rep(es1$Ingpcug, es1$Fex_c_round)

es2 <- subset(data2014, data2014$Estrato1==2)
es2 <- rep(es2$Ingpcug, es2$Fex_c_round)

es3 <- subset(data2014, data2014$Estrato1==3)
es3 <- rep(es3$Ingpcug, es3$Fex_c_round)

es4 <- subset(data2014, data2014$Estrato1==4)
es4 <- rep(es4$Ingpcug, es4$Fex_c_round)

es5 <- subset(data2014, data2014$Estrato1==5)
es5 <- rep(es5$Ingpcug, es5$Fex_c_round)

es6 <- subset(data2014, data2014$Estrato1==6)
es6 <- rep(es6$Ingpcug, es6$Fex_c_round)

gc()

# Se termina de definir el límite máximo de ingresos de la clase muy alta.
clases2014$max[6] <- max(data2014$Ingpcug)

# Creación de tabla que dicen cuánta gente hay de cada clase en cada estrato.

ingestratos <- clases2014
ingestratos <- ingestratos[,c(-1,-2)]

# Distribución de clases estrato 1
for (a in 1:NROW(clases2014)) {
  ingestratos$es1[a] <- NROW(subset(es1, es1<clases2014$max[a] & es1>clases2014$min[a]))
}
gc()

# Distribución de clases estrato 2
for (a in 1:NROW(clases2014)) {
  ingestratos$es2[a] <- NROW(subset(es2, es2<clases2014$max[a] & es2>clases2014$min[a]))
}
gc()

# Distribución de clases estrato 3
for (a in 1:NROW(clases2014)) {
  ingestratos$es3[a] <- NROW(subset(es3, es3<clases2014$max[a] & es3>clases2014$min[a]))
}
gc()

# Distribución de clases estrato 4
for (a in 1:NROW(clases2014)) {
  ingestratos$es4[a] <- NROW(subset(es4, es4<clases2014$max[a] & es4>clases2014$min[a]))
}
gc()

# Distribución de clases estrato 5
for (a in 1:NROW(clases2014)) {
  ingestratos$es5[a] <- NROW(subset(es5, es5<clases2014$max[a] & es5>clases2014$min[a]))
}
gc()

# Distribución de clases estrato 6
for (a in 1:NROW(clases2014)) {
  ingestratos$es6[a] <- NROW(subset(es6, es6<clases2014$max[a] & es6>clases2014$min[a]))
}
gc()

# Creación de tabla que dice el porcentaje de personas de cada clase social para cada estrato.

ingestratospc <- data.frame(row.names=rownames(ingestratos))
ingestratospc$es1 <- 100*ingestratos$es1/sum(ingestratos$es1)
ingestratospc$es2 <- 100*ingestratos$es2/sum(ingestratos$es2)
ingestratospc$es3 <- 100*ingestratos$es3/sum(ingestratos$es3)
ingestratospc$es4 <- 100*ingestratos$es4/sum(ingestratos$es4)
ingestratospc$es5 <- 100*ingestratos$es5/sum(ingestratos$es5)
ingestratospc$es6 <- 100*ingestratos$es6/sum(ingestratos$es6)


# Gráficos de barras apiladas.

purples <- c("midnightblue", "mediumpurple4", "mediumpurple"
             , "mediumorchid", "plum2", "pink")

png("./graficos/barrasapiladas.png")
par(mar=c(5,4,5,7))
barplot(as.matrix(ingestratospc), col=purples, ylab="Porcentaje")
legend(x=7.5, y=80, rev(rownames(ingestratospc)), fill=rev(purples)
       , horiz=F
       , xpd=T
       , border="gray"
       , box.col="white"
       , title="Clase social")
title(main="Estratos y clases sociales \n en Colombia 2014"
      , sub="Fuente: Vasija de ideas con datos del DANE \n(clases con base en ingresos mensuales)")
dev.off()

# Gráfico de "estratos ideales"
# Todas las personas de cada estrato corresponden a la clase social respectiva.
# Estrato 1: Clase muy baja; 2: baja; 3: media baja; 4: media alta; 5: alta; 6: muy alta
ingestratospcide <- ingestratospc
ingestratospcide$es1 <- 0
ingestratospcide$es2 <- 0
ingestratospcide$es3 <- 0
ingestratospcide$es4 <- 0
ingestratospcide$es5 <- 0
ingestratospcide$es6 <- 0

ingestratospcide$es1[1] <- 100
ingestratospcide$es2[2] <- 100
ingestratospcide$es3[3] <- 100
ingestratospcide$es4[4] <- 100
ingestratospcide$es5[5] <- 100
ingestratospcide$es6[6] <- 100

png("./graficos/barrasapiladasideal.png")
par(mar=c(5,4,5,7))
barplot(as.matrix(ingestratospcide), col=purples, ylab="Porcentaje")
legend(x=7.5, y=80, rev(rownames(ingestratospc)), fill=rev(purples)
       , horiz=F
       , xpd=T
       , border="gray"
       , box.col="white"
       , title="Clase social")
title(main="Estratos y clases sociales \n en Colombia (caso ideal)"
      , sub="Fuente: Vasija de ideas \n(clases con base en ingresos mensuales)")
dev.off()
