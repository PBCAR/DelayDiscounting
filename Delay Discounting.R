#####################################################################################################
### ---------------------------------  DD 100 & DD 1000   -------------------------------------------
#####################################################################################################

### SET WORKING DIRECTORY: SESSION > SET WORKING DIRECTORY > CHOOSE DIRECTORY
setwd("~/Desktop/")

### NAME OF .CSV FILE
df.name <- "DD.Data.csv"

### NAME OF ID VARIABLE
id.name <- "participant_id"

### DD100 & DD1000 VARIABLE NAMES (CHANGE IF REQUIRED)
variable.names <- c("dd3weeks100","dd2years100","dd1day100","dd8years100","dd4months100",
                    "dd4days100","dd4hours100","dd18years100","dd4years100","dd8months100",
                    "dd2months100","dd1_5weeks100","dd2days100","dd9hours100","dd2hours100",
                    "dd25years100","dd12years100","dd5years100","dd3years100","dd1year100",
                    "dd6months100","dd3months100","dd1month100","dd2weeks100","dd1week100",
                    "dd3days100","dd1_5days100","dd12hours100","dd6hours100","dd3hours100",
                    "dd1hour100","dd3weeks1000","dd2years1000","dd1day1000","dd8years1000",
                    "dd4months1000","dd4days1000","dd4hours1000","dd18years1000","dd4years1000",
                    "dd8months1000","dd2months1000","dd1_5weeks1000","dd2days1000","dd9hours1000",
                    "dd2hours1000","dd25years1000","dd12years1000","dd5years1000","dd3years1000",
                    "dd1year1000","dd6months1000","dd3months1000","dd1month1000","dd2weeks1000",
                    "dd1week1000","dd3days1000","dd1_5days1000","dd12hours1000","dd6hours1000",
                    "dd3hours1000","dd1hour1000","ddcontrol100","ddcontrol1000")

#####################################################################################################
##### --- IMPORT DATA

DD <- read.csv(df.name)
### SUBSET VARIABLES
DD <- DD[c(id.name,variable.names)]

########### ----------  DELAY DISCOUNTING $100
#####################################################################################################

DD$dd1 <- ifelse(DD$dd25years100==1, 0.000110,
                   ifelse(DD$dd25years100==2, 0.000129,NA))

DD$dd2 <- ifelse(DD$dd12years100==1, 0.000186,
                   ifelse(DD$dd12years100==2, 0.000279,NA))

DD$dd3 <- ifelse(DD$dd5years100==1, 0.000433,
                   ifelse(DD$dd5years100==2, 0.000612, NA))

DD$dd4 <- ifelse(DD$dd3years100==1, 0.000791,
                   ifelse(DD$dd3years100==2, 0.00112, NA))

DD$dd5 <- ifelse(DD$dd1year100==1, 0.00194,
                   ifelse(DD$dd1year100==2, 0.00335, NA))

DD$dd6 <- ifelse(DD$dd6months100==1, 0.00474,
                   ifelse(DD$dd6months100==2, 0.00671, NA))

DD$dd7 <- ifelse(DD$dd3months100==1, 0.00949,
                   ifelse(DD$dd3months100==2, 0.0134, NA))

DD$dd8 <- ifelse(DD$dd1month100==1, 0.0232,
                   ifelse(DD$dd1month100==2, 0.0396, NA))

DD$dd9 <- ifelse(DD$dd2weeks100==1, 0.0583,
                   ifelse(DD$dd2weeks100==2, 0.0825, NA))

DD$dd10 <- ifelse(DD$dd1week100==1, 0.117,
                    ifelse(DD$dd1week100==2, 0.189, NA))

DD$dd11 <- ifelse(DD$dd3days100==1, 0.289,
                    ifelse(DD$dd3days100==2, 0.408, NA))

DD$dd12 <- ifelse(DD$dd1_5days100==1, 0.577,
                    ifelse(DD$dd1_5days100==2, 0.816, NA))

DD$dd13 <- ifelse(DD$dd12hours100==1, 1.41,
                    ifelse(DD$dd12hours100==2, 2.31, NA))

DD$dd14 <- ifelse(DD$dd6hours100==1, 3.27,
                    ifelse(DD$dd6hours100==2, 4.90, NA))

DD$dd15 <- ifelse(DD$dd3hours100==1, 6.93,
                    ifelse(DD$dd3hours100==2, 9.79, NA))

DD$dd16 <- ifelse(DD$dd1hour100==1, 17.0,
                    ifelse(DD$dd1hour100==2, 24.0, NA))

DD100 <- DD[c(id.name,"ddcontrol100","dd1","dd2","dd3",'dd4',"dd5","dd6","dd7","dd8","dd9",
               "dd10","dd11","dd12","dd13","dd14","dd15","dd16")]

### CHANGE NA to 0 values
for(i in 1:ncol(DD100)) {
  for(j in 1:nrow(DD100)) {
    if(is.na(DD100[j,i]==T)) {
      DD100[j,i] <- 0
    }
  }
}

DD100$dd100_kvalue <- apply(DD100[c(-1,-2)],1,max)
DD100$dd100_kvalue <- ifelse(DD100$ddcontrol100==2,DD100$dd100_kvalue,NA)

########### ----------  DELAY DISCOUNTING $1000
#####################################################################################################

DD$DD1 <- ifelse(DD$dd25years1000==1, 0.000110,
                   ifelse(DD$dd25years1000==2, 0.000129,NA))

DD$DD2 <- ifelse(DD$dd12years1000==1, 0.000186,
                   ifelse(DD$dd12years1000==2, 0.000279,NA))

DD$DD3 <- ifelse(DD$dd5years1000==1, 0.000433,
                   ifelse(DD$dd5years1000==2, 0.000612, NA))

DD$DD4 <- ifelse(DD$dd3years1000==1, 0.000791,
                   ifelse(DD$dd3years1000==2, 0.00112, NA))

DD$DD5 <- ifelse(DD$dd1year1000==1, 0.00194,
                   ifelse(DD$dd1year1000==2, 0.00335, NA))

DD$DD6 <- ifelse(DD$dd6months1000==1, 0.00474,
                   ifelse(DD$dd6months1000==2, 0.00671, NA))

DD$DD7 <- ifelse(DD$dd3months1000==1, 0.00949,
                   ifelse(DD$dd3months1000==2, 0.0134, NA))

DD$DD8 <- ifelse(DD$dd1month1000==1, 0.0232,
                   ifelse(DD$dd1month1000==2, 0.0396, NA))

DD$DD9 <- ifelse(DD$dd2weeks1000==1, 0.0583,
                   ifelse(DD$dd2weeks1000==2, 0.0825, NA))

DD$DD10 <- ifelse(DD$dd1week1000==1, 0.117,
                    ifelse(DD$dd1week1000==2, 0.189, NA))

DD$DD11 <- ifelse(DD$dd3days1000==1, 0.289,
                    ifelse(DD$dd3days1000==2, 0.408, NA))

DD$DD12 <- ifelse(DD$dd1_5days1000==1, 0.577,
                    ifelse(DD$dd1_5days1000==2, 0.816, NA))

DD$DD13 <- ifelse(DD$dd12hours1000==1, 1.41,
                    ifelse(DD$dd12hours1000==2, 2.31, NA))

DD$DD14 <- ifelse(DD$dd6hours1000==1, 3.27,
                    ifelse(DD$dd6hours100==2, 4.90, NA))

DD$DD15 <- ifelse(DD$dd3hours1000==1, 6.93,
                    ifelse(DD$dd3hours100==2, 9.79, NA))

DD$DD16 <- ifelse(DD$dd1hour1000==1, 17.0,
                    ifelse(DD$dd1hour1000==2, 24.0, NA))

DD1000 <- DD[c(id.name,"ddcontrol1000","DD1","DD2","DD3",'DD4',"DD5","DD6","DD7","DD8","DD9",
                  "DD10","DD11","DD12","DD13","DD14","DD15","DD16")]

### CHANGE NA to 0 values
for(i in 1:ncol(DD1000)) {
  for(j in 1:nrow(DD1000)) {
    if(is.na(DD1000[j,i]==T)) {
      DD1000[j,i] <- 0
    }
  }
}

### SELECTS MAX VALUE ACROSS DD ITEMS (EXCLUDES ID AND DD CONTROL)
DD1000$dd1000_kvalue <- apply(DD1000[c(-1,-2)],1,max)

### QC: REMOVES DD K VALUE IF DD CONTROL ANSWERED INCORRECTLY
DD1000$dd1000_kvalue <- ifelse(DD1000$ddcontrol1000==2,DD1000$dd1000_kvalue,NA)

#####################################################################################################
### MERGE AND EXPORT

DD100 <- DD100[c(id.name,"dd100_kvalue")]
DD1000 <- DD1000[c(id.name,"dd1000_kvalue")]

DD.complete <- merge(DD100,DD1000, by = id.name)

write.csv(DD.complete,"Delay Discounting Processed.csv", row.names = F)




