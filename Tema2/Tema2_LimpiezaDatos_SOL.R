#https://www.kaggle.com/datasets/joebeachcapital/lending-club
#Este conjunto de datos recoge miles de préstamos concedidos a través 
#de la plataforma Lending Club, que permite a particulares prestar dinero 
#a otros particulares. 
#Por supuesto, no todos los préstamos son iguales. 
#install.packages(readr)
library(readr)

loan <- read_csv(file = "C:/Users/david/OneDrive - Universidad de Alcala/AOF/SIMULACION/CURSO 2026_27/GitHub/SIMULACION2/loans_DDBB.csv")

head(loan)
dim(loan)
#columnas que tenemos en la BBDD
names(loan)

#Chequear tipo de dato que es cada columna: 
str(loan)
#Resume variables
summary(loan)

#Vamos a contar los valores que tienen NA en la bbdd de ptos
colSums(is.na(loan))

#Porcentajes de NA
colSums(is.na(loan))/length(loan$emp_title)

#Antes de eliminar vamos a ver el significado de cada variable: 
#"emp_title" --- titulo del trabajo
#"emp_length" -- tiempo en el trabajo
#"state" ------- estado  

#"homeownership" -- Régimen de vivienda del solicitante (propietario, hipoteca, alquiler, etc.)
#"annual_income" -- Ingresos anuales 
#"verified_income" -- Estado de verificación de los ingresos (verificados o no)

#"debt_to_income" -- Ratio deuda/ingresos del solicitante (DTI)
#"annual_income_joint" -- Ingresos anuales conjuntos en solicitudes compartidas

#"verification_income_joint" -- Estado de verificación de los ingresos conjuntos
#"debt_to_income_joint" -- Ratio deuda/ingresos conjunto

#"delinq_2y" -- numero de impagos o morosidades en los últimos 2 años
#"months_since_last_delinq" -- Meses transcurridos desde la última morosidad

#"earliest_credit_line" -- Año de apertura de la primera línea de crédito
#"inquiries_last_12m" -- Consultas crediticias realizadas en los últimos 12 meses

#"total_credit_lines" -- Número total de líneas de crédito históricas
#"open_credit_lines" -- Número de líneas de crédito actualmente abiertas

#"total_credit_limit" -- Límite total de crédito disponible
#"total_credit_utilized" -- Crédito utilizado actualmente

#"num_collections_last_12m" -- Número de cuentas enviadas a recobro en los últimos 12 meses
#"num_historical_failed_to_pay" - Número histórico de incumplimientos de pago

#"months_since_90d_late" -- Meses desde el último retraso superior a 90 días
#"current_accounts_delinq" -- Número de cuentas actualmente en mora

#"total_collection_amount_ever" - Importe total reclamado históricamente por agencias de recobro
#"current_installment_accounts" - Número de préstamos o cuentas con pagos periódicos activos

#"accounts_opened_24m" ---------- Número de cuentas abiertas en los últimos 24 meses
#"months_since_last_credit_inquiry" -- Meses desde la última consulta crediticia

#"num_satisfactory_accounts" ---- Número de cuentas con historial satisfactorio
#"num_accounts_120d_past_due" --- Número de cuentas con más de 120 días de retraso

#"num_accounts_30d_past_due" ---- Número de cuentas con más de 30 días de retraso
#"num_active_debit_accounts" ---- Número de cuentas de débito activas

#"total_debit_limit" ------------ Límite total disponible en cuentas de débito
#"num_total_cc_accounts" -------- Número total de cuentas de tarjeta de crédito

#"num_open_cc_accounts" --------- Número de tarjetas de crédito abiertas
#"num_cc_carrying_balance" ------ Número de tarjetas con saldo pendiente de pago

#"num_mort_accounts" ------------ Número de préstamos hipotecarios activos
#"account_never_delinq_percent" - Porcentaje de cuentas que nunca han presentado mora

#"tax_liens" -------------------- Número de embargos o gravámenes fiscales
#"public_record_bankrupt" ------- Número de registros públicos de quiebra o bancarrota

#"loan_purpose" ----------------- Finalidad del préstamo (coche, vivienda, deuda, etc.)
#"application_type" ------------- Tipo de solicitud (individual o conjunta)

#"loan_amount" ------------------ Importe solicitado del préstamo
#"term" ------------------------- Duración del préstamo (normalmente 36 o 60 meses)

#"interest_rate" ---------------- Tipo de interés anual aplicado al préstamo
#"installment" ------------------ Cuota periódica a pagar por el prestatario

#"grade" ------------------------ Categoría general de riesgo crediticio (A-G)
#"sub_grade" -------------------- Subcategoría de riesgo dentro del grade (A1, A2,..., G5)

#"issue_month" ------------------ Mes de concesión del préstamo
#"loan_status" ------------------ Estado actual del préstamo (Fully Paid, Current, Default, etc.)

#"initial_listing_status" ------- Estado inicial de publicación del préstamo en la plataforma
#"disbursement_method" ---------- Método de desembolso de los fondos concedidos

#"balance" ---------------------- Saldo pendiente del préstamo
#"paid_total" ------------------- Importe total pagado hasta la fecha

#"paid_principal" --------------- Capital amortizado hasta la fecha
#"paid_interest" ---------------- Intereses pagados hasta la fecha
#"paid_late_fees" --------------- Comisiones por retraso pagadas
#################################################################
#Visualizacion variables numericas
hist(loan$emp_length, col = "lightblue", xlab="")
hist(loan$annual_income, col = "lightblue", xlab="", breaks = 20)
hist(loan$debt_to_income, col ="lightblue", xlab="", breaks = 20)
hist(loan$annual_income_joint, col ="lightblue", xlab="", breaks = 20)
hist(loan$earliest_credit_line, col ="lightblue", xlab="", breaks = 20)
hist(loan$open_credit_lines, col ="lightblue", xlab="", breaks = 20)
hist(loan$loan_amount, col ="lightblue", xlab="", breaks = 20)

#Visualizacion variables texto
barplot(table(loan$state), las = 2, col = "lightblue",
        main = "Número de préstamos por estado",
        ylab = "Frecuencia")
barplot(table(loan$loan_status), las = 2, col = "lightblue",
        main = "Estado del préstamo",
        ylab = "Frecuencia")
barplot(table(loan$grade), las = 2, col = "lightblue",
        main = "Grado riesgo del pto",
        ylab = "Frecuencia")

##############################################################
#Ahora tenemos que tomar la decisión de que variables nos quedamos
#y cuáles elminamos previo a montar el modelo
#Luego de los datos NA que hacemos con ellos?

#No existen duplicados en este caso
sum(duplicated(loan))

#Ejercicio: 
#Me podrías decir que harías con estas variables y como imputarías los
#NA de estas columnas: 
#emp_title
#"emp_title" --- titulo del trabajo
unique(loan$emp_title)
sum(is.na(loan$emp_title))
barplot(table(loan$emp_title))

#emp_length
loan$emp_length
max(loan$emp_length, na.rm = T)
min(loan$emp_length, na.rm = T)
sum(is.na(loan$emp_length))

#"emp_length" -- tiempo en el trabajo

#annual_income_joint
loan$annual_income_joint
sum(is.na(loan$annual_income_joint))


#"annual_income_joint" -- Ingresos anuales conjuntos en solicitudes compartidas
#debt_to_income_joint
#months_since_last_delinq
#months_since_90d_late
#debt_to_income

###################################################################3
#SOLUCION:
###################################################################3
loan$emp_title 
#Crear una variable desconocida para NA

unique(loan$emp_length) 
#Daria un valor medio o creando tiempo desconocido

loan$annual_income_joint 
#Imputar 0 si el pto es individual 
#loan_imp$annual_income_joint[loan_imp$application_type == "individual"] <- 0

loan$months_since_last_delinq
#Imputar NA --- como 0 (no moroso)

#Imputar NA --- como 0 (no moroso)
loan$months_since_90d_late

#Aquí yo imputaría la media o mediana.
loan$debt_to_income
