load(file = "EjercicioExtraTema2_30.10.26.RData")

head(dataExt)

save(dataExt, file = "EjercicioExtraTema2_30.10.26.RData")

#medv (Median Value): Valor mediano expresado en miles de dólares (ejemplo: 24.0 = $24,000).
#Utilizar esta variable como explicativa - Y

#crim: Tasa de criminalidad per cápita por barrio/suburbio.
#zn: Proporción de terreno residencial zonificado para parcelas grandes (más de 25,000 pies cuadrados)
#indus: Proporción de acres comerciales no minoristas e industriales por suburbio.   
#chas: (Charles River): Variable dicotómica 1 si el barrio linda con el río Charles; 0 en caso contrario.
#nos: Concentración de óxidos de nitrógeno (en partes por 10 millones), como medida de la contaminación del aire.
#rm: Número medio de habitaciones (rooms) por vivienda.   age: Proporción de viviendas ocupadas por sus dueños construidas antes de 1939 
#dis: Media ponderada de las distancias a cinco centros de empleo de la región de Boston.
#rad: Índice de accesibilidad a las autopistas radiales.   
#tax: Tasa del impuesto sobre la propiedad de valor total por cada $10,000.   
#ptratio: Ratio de alumnos por profesor (pupil-teacher ratio) en los colegios de la zona.
#black: Cálculo transformado de la población afroamericana del barrio: $1000(Bk - 0.63)^2$, donde $Bk$ es la proporción de habitantes de raza negra.
#lstat: Porcentaje de la población considerada de bajo estatus socioeconómico (lower status). 
#TipoZona: tipo de zona de las viviendas, hay 4 categorías A, B, C y D.


