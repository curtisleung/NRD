setwd("U:/_Curtis/PHD/2017 - Term 2/Advanced Methods in Health Services Research/Dataset/NRD_2014")

#rm(list=ls())

# install.packages("readr")
library(readr)

unzip("NRD_2014_Core.zip", list = TRUE)

#import all records and columns
NRD <- read_csv("NRD_2014_Core.CSV",
                #col_names = FALSE,
                locale = default_locale(),
                col_names=c("AGE", "AWEEKEND", "DIED", "DISCWT", "DISPUNIFORM", "DMONTH", "DQTR", "DRG", "DRGVER",
                            "DRG_NoPOA", "DX1", "DX2", "DX3", "DX4", "DX5", "DX6", "DX7", "DX8", "DX9", "DX10", "DX11",
                            "DX12", "DX13", "DX14", "DX15", "DX16", "DX17", "DX18", "DX19", "DX20", "DX21", "DX22",
                            "DX23", "DX24", "DX25", "DX26", "DX27", "DX28", "DX29", "DX30", "DXCCS1", "DXCCS2",
                            "DXCCS3", "DXCCS4", "DXCCS5", "DXCCS6", "DXCCS7", "DXCCS8", "DXCCS9", "DXCCS10", "DXCCS11",
                            "DXCCS12", "DXCCS13", "DXCCS14", "DXCCS15", "DXCCS16", "DXCCS17", "DXCCS18", "DXCCS19",
                            "DXCCS20", "DXCCS21", "DXCCS22", "DXCCS23", "DXCCS24", "DXCCS25", "DXCCS26", "DXCCS27",
                            "DXCCS28", "DXCCS29", "DXCCS30", "ECODE1", "ECODE2", "ECODE3", "ECODE4", "ELECTIVE",
                            "E_CCS1", "E_CCS2", "E_CCS3", "E_CCS4", "FEMALE", "HCUP_ED", "HOSP_NRD", "KEY_NRD",
                            "LOS", "MDC", "MDC_NoPOA", "NCHRONIC", "NDX", "NECODE", "NPR", "NRD_DaysToEvent",
                            "NRD_STRATUM", "NRD_VisitLink", "ORPROC", "PAY1", "PL_NCHS", "PR1", "PR2", "PR3", "PR4",
                            "PR5", "PR6", "PR7", "PR8", "PR9", "PR10", "PR11", "PR12", "PR13", "PR14", "PR15", "PRCCS1",
                            "PRCCS2", "PRCCS3", "PRCCS4", "PRCCS5", "PRCCS6", "PRCCS7", "PRCCS8", "PRCCS9", "PRCCS10",
                            "PRCCS11", "PRCCS12", "PRCCS13", "PRCCS14", "PRCCS15", "PRDAY1", "PRDAY2", "PRDAY3",
                            "PRDAY4", "PRDAY5", "PRDAY6", "PRDAY7", "PRDAY8", "PRDAY9", "PRDAY10", "PRDAY11",
                            "PRDAY12", "PRDAY13", "PRDAY14", "PRDAY15", "REHABTRANSFER", "RESIDENT", "SAMEDAYEVENT",
                            "SERVICELINE", "TOTCHG", "YEAR", "ZIPINC_QRTL"),
                col_types = "iiiniiiiiicccccccccccccccccccccccccccccciiiiiiiiiiiiiiiiiiiiiiiiiiiiiicccciiiiiiiiiiiiiiiiiiciiiccccccccccccccciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiciiii",
                quoted_na = TRUE,
                quote = "\"",
                comment = "",
                skip = 0,
                #progress = TRUE
                n_max = 6969
)


#import all records, subset of
NRD <- read_csv("NRD_2014_Core.CSV",
                col_names = FALSE,
                col_types = cols_only (X1 = 'i',
                                       X3 = 'i',
                                       X4 = 'n',
                                       X5 = 'i',
                                       X6 = 'i',
                                       X11 = 'c',
                                       X12 = 'c',
                                       X41 = 'i',
                                       X42 = 'i',
                                       X75 = 'i',
                                       X80 = 'i',
                                       X82 = 'i',
                                       X83 = 'i',
                                       X84 = 'i',                                       
                                       X87 = 'i',
                                       X90 = 'i',
                                       X91 = 'i',
                                       X92 = 'i',
                                       X93 = 'c',
                                       X95 = 'i',
                                       X96 = 'i',
                                       X97 = 'c',
                                       X98 = 'c',
                                       X112 = 'i',
                                       X113 = 'i',
                                       X142 = 'i',
                                       X143 = 'i',
                                       X144 = 'c',
                                       X145 = 'i',
                                       X146 = 'i',
                                       X148 = 'i'
                                       ),
                quoted_na = TRUE,
                quote = "\"",
                comment = "",
                n_max = 1,
                skip = 0
                )


#rename columns
library(plyr) 

NRD <- rename(NRD,c("X1" = "AGE",
                    "X3" = "DIED",
                    "X4" = "DISCWT",
                    "X5" = "DISPUNIFORM",
                    "X6" = "DMONTH",
                    "X11" = "DX1",
                    "X12" = "DX2",
                    "X41" = "DXCCS1",
                    "X42" = "DXCCS2",
                    "X75" = "ELECTIVE",
                    "X80" = "FEMALE",
                    "X82" = "HOSP_NRD",
                    "X83" = "KEY_NRD",
                    "X84" = "LOS",
                    "X87" = "NCHRONIC",
                    "X90" = "NPR",
                    "X91" = "NRD_DaysToEvent",
                    "X92" = "NRD_STRATUM",
                    "X93" = "NRD_VisitLink",
                    "X95" = "PAY1",
                    "X96" = "PL_NCHS",
                    "X97" = "PR1",
                    "X98" = "PR2",
                    "X112" = "PRCCS1",
                    "X113" = "PRCCS2",
                    "X142" = "REHABTRANSFER",
                    "X143" = "RESIDENT",
                    "X144" = "SAMEDAYEVENT",
                    "X145" = "SERVICELINE",
                    "X146" = "TOTCHG",
                    "X148" = "ZIPINC_QRTL"
                    ))



#List of column names and class
#names(NRD)
lapply(NRD2, class)
head(NRD2)
summary(NRD2)


#connect to SQL table
install.packages("RODBC")
library(RODBC)

#import data from SQL table
con  <- odbcConnect("ESMUCRMRPTP01")
NRD2 <- sqlQuery(con, "SELECT * FROM analytics.dbo.NRD_Core")
NRD_Hospitals <- sqlQuery(con, "SELECT * FROM analytics.dbo.NRD_Hospitals")
close(con)


#Clean Up Formats
#See distribution: table(NRD2$HCUP_ED)

  #N10PF: NRD_DaysToEvent, TOTCHG
    NRD2$TOTCHG[NRD2$TOTCHG == "-999999999"] <- "."
    NRD2$TOTCHG[NRD2$TOTCHG == "-888888888"] <- ".A"
    NRD2$TOTCHG[NRD2$TOTCHG == "-666666666"] <- ".C"
  #N11P7F: DISCWT
    NRD2$DISCWT[NRD2$DISCWT == "-99.9999999"] <- "."
    NRD2$DISCWT[NRD2$DISCWT == "-88.8888888"] <- ".A"
    NRD2$DISCWT[NRD2$DISCWT == "-66.6666666"] <- ".C"
  #N2PF: DIED, DISPUNIFORM, DMONTH, ELECTIVE, FEMALE, HCUP_ED, NCHRONIC, PAY1, REHABTRANSFER, RESIDENT, SERVICELINE, IPINC_QRTL
    NRD2$IPINC_QRTL[NRD2$IPINC_QRTL == "-9"] <- "."
    NRD2$IPINC_QRTL[NRD2$IPINC_QRTL == "-8"] <- ".A"
    NRD2$IPINC_QRTL[NRD2$IPINC_QRTL == "-6"] <- ".C"
    NRD2$IPINC_QRTL[NRD2$IPINC_QRTL == "-5"] <- ".N"
  #N3PF: AGE, DXCCS1, DXCCS2, NPR, PL_NCHS, PRCCS1, PRCCS2
    NRD2$PRCCS2[NRD2$PRCCS2 == "-99"] <- "."
    NRD2$PRCCS2[NRD2$PRCCS2 == "-88"] <- ".A"
    NRD2$PRCCS2[NRD2$PRCCS2 == "-66"] <- ".C"    
  #N5PF: LOS, NRD_STRATUM
    NRD2$NRD_STRATUM[NRD2$NRD_STRATUM == "-9999"] <- "."
    NRD2$NRD_STRATUM[NRD2$NRD_STRATUM == "-8888"] <- ".A"
    NRD2$NRD_STRATUM[NRD2$NRD_STRATUM == "-6666"] <- ".C"



#Actual analysis
install.packages("survey"); library(survey)


# Identify index events
# AMI ICD9 Diagnoses between 41000:41091
# Exclude dispositions for Transfer to Short-term Hospital (2), Transfer Other (5), AMA (7), Died (20), or Invalid (., .A)

#DXrange <- factor(41000:41091, levels = levels(NRD2$DX1))
NRD2$IndexEvent <- NRD2$DMONTH >= 1 & NRD2$DMONTH <= 11 &
  NRD2$DIED == 0 & NRD2$LOS >= 0 & NRD2$AGE >= 18 &
  #Surgical (4) and Medical (5) service lines
  NRD2$SERVICELINE %in% c("4","5") &
  #In-state residents
  NRD2$RESIDENT == 1 &
  !(NRD2$DISPUNIFORM %in% c("2", "5", "7", "20", ".", ".A")) &
  # Exclude admissions for primary psychiatric diagnoses
  !(NRD2$DXCCS1 %in% c("650", "651", "652", "654", "655", "656","657","658","659","662","670")) &
  # Exclude admissions for primary rehabilitation
  !(NRD2$DXCCS1 %in% c("254")) &
  # Exclude admissions for medical treatment of cancer
  !(NRD2$DXCCS1 %in% c("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22",
                       "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
                       "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45"))
  #NRD2$DX1 %in% DXrange & substr(NRD2$DX1, 5, 5) != 2
cat("Frequency of IndexEvent : \n"); table(NRD2$IndexEvent)

# Calculate discharge dates for index events
# Add LOS to NRD_DaysToEvent field
NRD2$PseudoDDate[NRD2$IndexEvent] <- NRD2$NRD_DaysToEvent[NRD2$IndexEvent] +
  ifelse(NRD2$LOS >= 0,
         NRD2$LOS, NA)[NRD2$IndexEvent]

# Identify readmission events and subset AMI index events
# for columns in (readmits) that match to "KEY_NRD", rename to "KEY_NRD_R"
indexE <- subset( NRD2, IndexEvent == 1,
                  select = c( HOSP_NRD, KEY_NRD,NRD_VisitLink, PseudoDDate ))
readmits <- subset( NRD2, select = c( KEY_NRD, NRD_VisitLink, NRD_DaysToEvent,
                                      DX1, DXCCS1, DX2, DXCCS2, PR1, PRCCS1, PR2, PRCCS2))
colnames(readmits)[colnames(readmits) == "KEY_NRD"] <- "KEY_NRD_R"
colnames(readmits)[colnames(readmits) == "DX1"] <- "DX1_R"
colnames(readmits)[colnames(readmits) == "DXCCS1"] <- "DXCCS1_R"
colnames(readmits)[colnames(readmits) == "DX2"] <- "DX2_R"
colnames(readmits)[colnames(readmits) == "DXCCS2"] <- "DXCCS2_R"
colnames(readmits)[colnames(readmits) == "PR1"] <- "PR1_R"
colnames(readmits)[colnames(readmits) == "PRCCS1"] <- "PRCCS1_R"
colnames(readmits)[colnames(readmits) == "PR2"] <- "PR2_R"
colnames(readmits)[colnames(readmits) == "PRCCS2"] <- "PRCCS2_R"


# Inner join index events with candidate readmissions
readmits <- merge(indexE, readmits, by = "NRD_VisitLink")

# Subset to 30-day readmissions
# Criteria: days to next visit (NRD_DaysToEvent) greater than Pseudo Discharge Date (PseudoDDate),
#           next admission within 30 days of discharge of index,
#           always planned CCS,
#           potentially planned CCS
alwaysDxCCS <- c("45","194","196","254")
alwaysPrCCS <- c("64","105","134","135","176")
potentialPrCCS <- c("1", "3", "5", "9", "10", "12", "33", "36", "38", "40", "43", "44", "45", "49",
                 "51", "52", "53", "55", "56", "59", "66", "67", "74", "78", "79", "84", "85",
                 "86", "99", "104", "106", "107", "109", "112", "113", "114", "119", "120",
                 "124", "129", "132", "142", "152", "153", "154", "158", "159", "166", "167",
                 "170", "172")
potentialPr <- c("301","3029","303","304","3174","346","3818","5503","5504","9426","9427")
potentialAcuteDxCCS <- c("1", "2", "3", "4", "5", "7", "8", "9", "54", "55", "60", "61", "63", "76",
                     "77", "78", "82", "83", "84", "85", "87", "89", "90", "91", "92", "93", "99",
                     "100", "102", "104", "107", "109", "112", "116", "118", "120", "122", "123",
                     "124", "125", "126", "127", "128", "129", "130", "131", "135", "137", "139",
                     "140", "142", "145", "146", "148", "153", "154", "157", "159", "165", "168",
                     "172", "197", "198", "225", "226", "227", "228", "229", "230", "232", "233",
                     "234", "235", "237", "238", "239", "240", "241", "242", "243", "244", "245",
                     "246", "247", "249", "250", "251", "252", "253", "259", "650", "651", "652",
                     "653", "656", "658", "660", "661", "662", "663", "670")
potentialAcuteDx <- c("03282 ", "03640 ", "03641 ", "03642 ", "03643 ", "07420 ", "07421 ",
                      "07422 ", "07423 ", "11281", "11503", "11504", "11513", "11514", "11593",
                      "11594", "1303", "391", "3911", "3912", "3918", "3919", "392", "398",
                      "3989", "39899", "420", "4209", "42091", "42099", "421", "4211", "4219",
                      "422", "4229", "42291", "42292", "42293", "42299", "423", "4231", "4232",
                      "4233", "429", "426", "4261", "42611", "42612", "42613", "4262", "4263",
                      "4264", "4265", "42651", "42652", "42653", "42654", "4266", "4267",
                      "42681", "42682", "4269", "4272", "42769", "42789", "4279", "785",
                      "39891", "428", "4281", "4282", "42821", "42823", "4283", "42831",
                      "42833", "4284", "42841", "42843", "4289", "574", "57401", "5743",
                      "57431", "5746", "57461", "5748", "57481", "575", "57512", "5761", "577")


readmits <- subset( readmits, KEY_NRD != KEY_NRD_R &
                      #Exclude Primary and Secondary Diag CCS that are in Always Planned
                      (!(DXCCS1_R %in% alwaysDxCCS) | 
                         !(DXCCS2_R %in% alwaysDxCCS) |
                         #Exclude Primary and Secondary Proc CCS that are in Always Planned
                         !(PRCCS1_R %in% alwaysPrCCS) | 
                         !(PRCCS2_R %in% alwaysPrCCS) |
                         #Exclude Primary and Secondary Proc CCS that are in Potentially Planned
                         #Not(Yes Potentially Planned and Not(Acute))
                         !( (PRCCS1_R %in% potentialPrCCS) & !(DXCCS1_R %in% potentialAcuteDxCCS) ) |
                         !( (PRCCS1_R %in% potentialPrCCS) & !(DX1_R %in% potentialAcuteDx) ) |
                         !( (PRCCS2_R %in% potentialPrCCS) & !(DXCCS1_R %in% potentialAcuteDxCCS) ) |
                         !( (PRCCS2_R %in% potentialPrCCS) & !(DX1_R %in% potentialAcuteDx) ) |
                         #Exclude Primary and Secondary Proc that are in Potentially Planned
                         #Not(Yes Potentially Planned and Not(Acute))
                         !( (PR1_R %in% potentialPr) & !(DXCCS1_R %in% potentialAcuteDxCCS) ) |
                         !( (PR1_R %in% potentialPr) & !(DX1_R %in% potentialAcuteDx) ) |
                         !( (PR2_R %in% potentialPr) & !(DXCCS1_R %in% potentialAcuteDxCCS) ) |
                         !( (PR2_R %in% potentialPr) & !(DX1_R %in% potentialAcuteDx) ) |
                         #Exclude Maryland Readmissions with Psych Diag
                         !(grepl("^[Vv]553",DX1_R)) |
                         !( (grepl("^29",DX1_R) | grepl("^30",DX1_R) | grepl("^31",DX1_R) ) &
                              ((NRD_DaysToEvent - PseudoDDate) %in% c("0","1")) )
                      )&
                      NRD_DaysToEvent >= PseudoDDate &
                      NRD_DaysToEvent <= ( PseudoDDate + 30 ),
                    select = c( HOSP_NRD, KEY_NRD ))


# De-duplicate readmissions and flag 30-day readmission events
readmits <- unique(readmits)
readmits$readmit <- 1

# Merge readmission events with NRD2 file
NRD2 <- NRD2[ with( NRD2, order( HOSP_NRD, KEY_NRD )), ]
readmits <- readmits[ with( readmits, order( HOSP_NRD, KEY_NRD )), ]
NRD2 <- merge( NRD2, readmits, by = c( "HOSP_NRD", "KEY_NRD" ), all.x = TRUE)
NRD2$readmit[is.na(NRD2$readmit)] <- 0


# Create alternate readmit measure
readmitsCAL <- subset( NRD2, select = c( KEY_NRD, NRD_VisitLink, NRD_DaysToEvent, ELECTIVE, HCUP_ED ))
colnames(readmitsCAL)[colnames(readmitsCAL) == "KEY_NRD"] <- "KEY_NRD_R"
colnames(readmitsCAL)[colnames(readmitsCAL) == "ELECTIVE"] <- "ELECTIVE_R"
colnames(readmitsCAL)[colnames(readmitsCAL) == "HCUP_ED"] <- "HCUP_ED_R"

readmitsCAL <- merge(indexE, readmitsCAL, by = "NRD_VisitLink")

readmitsCAL <- subset( readmitsCAL, KEY_NRD != KEY_NRD_R &
                      #Exclude Admissions Deemed Elective:
                        #0	Non-elective admission
                      (ELECTIVE_R != 0 |
                      #Exclude Admissions with ED Charges
                        #1 Emergency Department revenue code on record
                        #2 Positive Emergency Department charge (when revenue center codes are not available)
                        #3 Emergency Department CPT procedure code on record
                        #4 Condition code P7 indication of ED admission, point of origin of ED, or admission source of ED
                        HCUP_ED_R %in% c("1","2","3","4")) & 
                      NRD_DaysToEvent >= PseudoDDate &
                      NRD_DaysToEvent <= ( PseudoDDate + 30 ),
                    select = c( HOSP_NRD, KEY_NRD ))

readmitsCAL <- unique(readmitsCAL)
readmitsCAL$readmitsCAL <- 1

NRD2 <- NRD2[ with( NRD2, order( HOSP_NRD, KEY_NRD )), ]
readmitsCAL <- readmitsCAL[ with( readmitsCAL, order( HOSP_NRD, KEY_NRD )), ]
NRD2 <- merge( NRD2, readmitsCAL, by = c( "HOSP_NRD", "KEY_NRD" ), all.x = TRUE)
NRD2$readmitsCAL[is.na(NRD2$readmitsCAL)] <- 0


# Tabulate (unweighted) readmissions
cat("Frequency of readmit : \n"); table(NRD2$readmit)

# National estimates based on NRD design

# Install and load the survey package
install.packages("survey")
library(survey)

# Specify the sampling design with sampling weights DISCWT,

# hospital clusters HOSP_NRD, and stratification NRD_STRATUM
strnrddsgn <- svydesign( ids = ~HOSP_NRD, weights = ~DISCWT,
                         strata= ~NRD_STRATUM, data = NRD2 )
cat( "Total readmissions : \n" ); svytotal( ~readmit, strnrddsgn )
cat( "Readmissions rate : \n" ); svymean( ~readmit, strnrddsgn )

# Subset on index events
strnrddsgn.sub <- subset( strnrddsgn, IndexEvent )
cat( "Total readmissions (over All index events) : \n" )
svytotal( ~readmit, strnrddsgn.sub )
cat( "Readmissions rate (over All index events) : \n")
svymean( ~readmit, strnrddsgn.sub )

#rm(strnrddsgn,strnrddsgn.sub)

install.packages("clusterCrit")
library(clusterCrit)


#Subset data to only include cases eligible for readmission
NRD_Final <- unique(NRD2)
NRD_Final <- subset( NRD_Final, IndexEvent == 1)


NRD_Diff <- subset(NRD_Final, readmitsCAL == 1)


#Create 2x2 table for inter-rater reliability
install.packages("gmodels")
library(gmodels)
CrossTable(NRD_Final$readmit, NRD_Final$readmitsCAL)

#Compare PPV, NPV, Overall Agreement, and Kappa
install.packages("CompareTests")
library(CompareTests)

diag <- CompareTests(NRD_Final$readmit, NRD_Final$readmitsCAL, goldstd = "NRD_Final$readmit")


#receiver operator curve comparing readmits to readmitsCAL
install.packages("ggplot2")
library(ggplot2)

install.packages("plotROC")
library(plotROC)
plotROC(NRD_Readmit$readmit, predicted)



#Join NRD_Final data frame with NRD_Hospitals to obtain hospital-level factors
library(dplyr)
NRD_Final <- full_join(NRD_Final, NRD_Hospitals, by = c("HOSP_NRD" = "HOSP_NRD", "NRD_STRATUM" = "NRD_STRATUM"))
NRD_Final <- unique(NRD_Final)
names(NRD_Final)
str(NRD_Final)

#Subset to retain variables for correlation matrix
NRD_Final <- NRD_Final[,c("readmit", "AGE", "FEMALE", "NPR", "NCHRONIC", "PAY1", "PL_NCHS", "IPINC_QRTL", "H_CONTRL", "HOSP_BEDSIZE", "HOSP_URCAT4", "HOSP_UR_TEACH")]
head(NRD_Final)

install.packages("corrplot")
library("corrplot")

#corrmatrix <- cor(NRD_Final, method = "pearson", use = "everything")
#corrmatrix <- cor(NRD_Final, method = "pearson", use = "all.obs")
corrmatrix <- cor(NRD_Final, method = "pearson", use = "complete.obs")
corrmatrix <- cor(NRD_Final, method = "pearson", use = "na.or.complete")

corrmatrix <- cor(NRD_Final)

round(corrmatrix,2)
corrplot(corrmatrix, method="number")



#create factors for independent variables to be included in Ordinal Logistic Regression

  install.packages("rpart")
  library("rpart")
  temp <- rpart(NRD_Final$readmit ~ NRD_Final$NCHRONIC)
  plot(temp)
  text(temp)

  #Age
  levels(NRD_Final$AGE)
  is.factor(NRD_Final$AGE)
  table(NRD_Final$AGE)  
  NRD_Final$AGECat<-cut(NRD_Final$AGE, c(18,25,35,45,55,65,75,Inf), right=FALSE,
                        labels = c("Age 18-24",
                                   "Age 25-34",
                                   "Age 35-44",
                                   "Age 45-54",
                                   "Age 55-64",
                                   "Age 65-74",
                                   "Age 75+"))
  
  levels(NRD_Final$AGECat)
  NRD_Final$AGECat <- factor(NRD_Final$AGECat, levels = c("Age 65-74",
                                                          "Age 18-24",
                                                          "Age 25-34",
                                                          "Age 35-44",
                                                          "Age 45-54",
                                                          "Age 55-64",
                                                          "Age 75+"))
  
  table(NRD_Final$AGECat, NRD_Final$readmit)
  

  #NPR - Number of Procedures
  levels(NRD_Final$NPR)
  is.factor(NRD_Final$NPR)
  table(NRD_Final$NPR)  
  NRD_Final$NPRCat<-cut(NRD_Final$NPR, c(-Inf,1,4,Inf), right=FALSE,
                        labels = c("Procedures: 0",
                                   "Procedures: 1-3",
                                   "Procedures: 4+"))
  table(NRD_Final$NPRCat)

  #NCHRONIC - Number of Chronic Conditions
  levels(NRD_Final$NCHRONIC)
  is.factor(NRD_Final$NCHRONIC)
  table(NRD_Final$NCHRONIC)  
  NRD_Final$NCHRONICCat<-cut(NRD_Final$NCHRONIC, c(-Inf,1,3,6,Inf), right=FALSE,
                        labels = c("CCs: 0",
                                   "CCs: 1-2",
                                   "CCs: 3-5",
                                   "CCs: 6+"))
  table(NRD_Final$NPRCat)  
  
  #Primary Payer
  #1	Medicare
  #2	Medicaid
  #3	Private insurance
  #4	Self-pay
  #5	No charge
  #6	Other
  NRD_Final$PAYER <- NRD_Final$PAY1
  
  levels(NRD_Final$PAYER)
  is.factor(NRD_Final$PAYER)
  table(NRD_Final$PAYER)
  
  NRD_Final$PAYER[NRD_Final$PAYER %in% c("-9","-8")] = NA
  NRD_Final$PAYER <- factor(NRD_Final$PAYER, levels = c("3", "1", "2", "4","5","6"),
                                  labels = c("Private",
                                             "Medicare",
                                             "Medicaid",
                                             "Self-pay",
                                             "No charge",
                                             "Other"))
  
  #PL_NCHS - Patient Location: NCHS Urban-Rural Code
  #1	"Central" counties of metro areas of >=1 million population
  #2	"Fringe" counties of metro areas of >=1 million population
  #3	Counties in metro areas of 250,000-999,999 population
  #4	Counties in metro areas of 50,000-249,999 population
  #5	Micropolitan counties
  #6	Not metropolitan or micropolitan counties
  NRD_Final$PTLoc <- NRD_Final$PL_NCHS
  
  levels(NRD_Final$PTLoc)
  is.factor(NRD_Final$PTLoc)
  table(NRD_Final$PTLoc)
  
  NRD_Final$PTLoc[NRD_Final$PTLoc %in% c("-99")] = NA
  NRD_Final$PTLocCat <- factor(NRD_Final$PTLoc, levels = c("6","5","4","3","2","1"),
                            labels = c("Not metropolitan",
                                       "Micropolitan",
                                       "Population 50K-249K",
                                       "Population 250K-999K",
                                       "Population >= 1M (Fringe counties)",
                                       "Population >= 1M (Central counties"))  
  
  
  #Gender (0=Male, 1=Female)
  levels(NRD_Final$FEMALE)
  is.factor(NRD_Final$FEMALE)
  table(NRD_Final$FEMALE)
  
  NRD_Final$Gender <- NRD_Final$FEMALE
  
  NRD_Final$Gender <- factor(NRD_Final$Gender, levels = c("0", "1"),
                             labels = c("Male",
                                        "Female"))
  
  #Median household income of residents in the patient's ZIP Code
  #1	0-25th percentile
  #2	26th to 50th percentile (median)
  #3	51st to 75th percentile
  #4	76th to 100th percentile
  #NRD_Final$IPINC_QRTL[NRD_Final$IPINC_QRTL %in% c("-9","-8")] = NA
  
  NRD_Final$ZIPINC_QRTL <- NRD_Final$IPINC_QRTL
  
  levels(NRD_Final$ZIPINC_QRTL)
  is.factor(NRD_Final$ZIPINC_QRTL)
  table(NRD_Final$ZIPINC_QRTL)
  
  NRD_Final$ZIPINC_QRTL[NRD_Final$ZIPINC_QRTL %in% c("-9","-8")] = NA
  NRD_Final$ZIPINC_QRTL <- factor(NRD_Final$ZIPINC_QRTL, levels = c("1", "2", "3", "4"),
                                   labels = c("Percentile 0-25",
                                              "Percentile 26-50",
                                              "Percentile 51-75",
                                              "Percentile 75-100"))
  
  
  #AHA hospital's ownership/control category
  #1	Government, nonfederal
  #2	Private, not-profit
  #3	Private, invest-own

  levels(NRD_Final$H_CONTRL)
  is.factor(NRD_Final$H_CONTRL)
  table(NRD_Final$H_CONTRL)
  
  NRD_Final$H_CONTRL <- factor(NRD_Final$H_CONTRL, levels = c("1", "2", "3"),
                               labels = c("Government, nonfederal",
                                          "Private, not-profit",
                                          "Private, invest-own"))
  
  #Bedsize of hospital
  #1	Small
  #2	Medium
  #3	Large

  levels(NRD_Final$HOSP_BEDSIZE)
  is.factor(NRD_Final$HOSP_BEDSIZE)
  table(NRD_Final$HOSP_BEDSIZE)
  
  NRD_Final$HOSP_BEDSIZE <- factor(NRD_Final$HOSP_BEDSIZE, levels = c("1", "2", "3"),
                                    labels = c("Small",
                                               "Medium",
                                               "Large"))
  
  #AHA urban-rural designation of the hospital and is based on the county of the hospital
  #1	Large metropolitan areas with at least 1 million residents
  #2	Small metropolitan areas with less than 1 million residents
  #3	Micropolitan areas
  #4	Not metropolitan or micropolitan (non-urban residual)

  levels(NRD_Final$HOSP_URCAT4)
  is.factor(NRD_Final$HOSP_URCAT4)
  table(NRD_Final$HOSP_URCAT4)
  
  NRD_Final$HOSP_URCAT4 <- factor(NRD_Final$HOSP_URCAT4, levels = c("4", "3", "2","1"),
                                   labels = c("Not metropolitan",
                                              "Micropolitan",
                                              "Small metropolitan < 1M residents",
                                              "Large metropolitan >= 1M residents"))
                      
  
  #AHA hospital's teaching status
  #0	Metropolitan non-teaching
  #1	Metropolitan teaching
  #2	Non-metropolitan hospital
  
  levels(NRD_Final$HOSP_UR_TEACH)
  is.factor(NRD_Final$HOSP_UR_TEACH)
  table(NRD_Final$HOSP_UR_TEACH)
  table(NRD_Final$HOSP_UR_TEACH, NRD_Final$readmit)
  
  NRD_Final$HOSP_UR_TEACH <- factor(NRD_Final$HOSP_UR_TEACH, levels = c("2", "0", "1"),
                                    labels = c("Non-metropolitan hospital",
                                               "Metropolitan non-teaching",
                                               "Metropolitan teaching"))
  

  #NRD_Final$HOSP_UR_TEACH <- ordered(NRD_Final$HOSP_UR_TEACH, levels = c("4", "3", "2","1"))  
      
  NRD_Final <- NRD_Final[,c("readmit", "AGECat", "Gender", "NPRCat", "NCHRONICCat", "PAYER", "PTLoc", "ZIPINC_QRTL", "H_CONTRL", "HOSP_BEDSIZE", "HOSP_URCAT4", "HOSP_UR_TEACH")]
  

#create logistic regression for dependent variable (readmit) vs. independent variables (pt and hospital-level)

install.packages("gee")
library("gee")

NRD_Readmit <- gee(readmit ~ AGECat +
                     FEMALE +
                     NPRCat +
                     NCHRONICCat +
                     PAYER +
                     PTLocCat +
                     ZIPINC_QRTL + 
                     H_CONTRL +
                     HOSP_BEDSIZE + 
                     HOSP_URCAT4 +
                     HOSP_UR_TEACH,
                   data = NRD_Final, id = NRD_VisitLink, family = binominal, corstr = "unstructured")

test <- glm(readmit ~ AGECat +
                      FEMALE +
                      HOSP_UR_TEACH,
                      family=binomial(link=logit), data=NRD_Final)
                   
NRD_Readmit <- glm(readmit ~ AGECat +
                     FEMALE +
                     #NPRCat +
                     NCHRONICCat +
                     PAYER +
                     PTLocCat +
                     ZIPINC_QRTL + 
                     H_CONTRL +
                     HOSP_BEDSIZE + 
                     #HOSP_URCAT4 +
                     HOSP_UR_TEACH,
                   #data = NRD_Final, id = NRD_VisitLink, family = binominal, corstr = "unstructured")
                   family=binomial(link=logit), data=NRD_Final)

summary(NRD_Readmit)

exp(cbind(OR = coef(NRD_Readmit), confint(NRD_Readmit)))

NRD_Readmit$coefficients

exp(coef(NRD_Readmit))
exp(confint(NRD_Readmit))




#regression diagnostics

  #variance inflation factors
  install.packages("faraway")
  library(faraway)
  vif(NRD_Readmit)
  
  #While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
  install.packages("pscl")
  library(pscl)
  pR2(NRD_Readmit)


table(NRD_Final$AGE)
ggplot(NRD_Final, aes(x = NRD_Final$AGE)) + geom_bar()

table(NRD_Final$FEMALE)

table(NRD_Final$NPR)
ggplot(NRD_Final, aes(x = NRD_Final$NPR)) + geom_bar()

table(NRD_Final$NCHRONIC)
ggplot(NRD_Final, aes(x = NRD_Final$NCHRONIC)) + geom_bar()

table(NRD_Final$IPINC_QRTL)
ggplot(NRD_Final, aes(x = NRD_Final$IPINC_QRTL)) + geom_bar()

table(NRD_Final$HOSP_BEDSIZE)
ggplot(NRD_Final, aes(x = NRD_Final$HOSP_BEDSIZE)) + geom_bar()

table(NRD_Final$H_CONTRL)
table(NRD_Final$HOSP_URCAT4)
table(NRD_Final$HOSP_UR_TEACH)

CrossTable(NRD_Final$H_CONTRL, NRD_Final$readmit)

CrossTable(NRD_Final$HOSP_UR_TEACH, NRD_Final$readmit)
ggplot(NRD_Final, aes(x = NRD_Final$HOSP_UR_TEACH, fill = NRD_Final$readmit)) + geom_bar(position = "fill")

CrossTable(NRD_Final$HOSP_URCAT4, NRD_Final$readmit)

