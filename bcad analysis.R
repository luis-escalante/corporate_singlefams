library(readxl)
#install.packages("stringi")
library(stringi)
library(data.table)

f3_fields<-read_xlsx("BCAD DATA/Appraisal Export Layout - 8.0.25.xlsx",range = "A55:F486",col_names = T)
types<-gsub(pattern = "\\s*\\([^\\)]+\\)",replacement = "",x = f3_fields$Datatype)

#Variables needed for analysis: (will need to extract these columns from each year's appraisal data)
  #prop_id            #Property ID
  #prop_type_cd       #Property Type Code: R = Real P = Business Personal Property M = Mobile Home MN = Mineral A = Automobiles
  #prop_val_yr        #Appraisal or Tax Year
#  #geo_id             #Geographic ID - did not use in extracting
#  #py_owner_id        #Property Year Owner ID – PACS Internal ID  - did not use in extracting
  #py_owner_name      #Property Year Owner Name
  #py_addr_line1      #Property Year Owner Address Line 1
  #py_addr_line2      #Property Year Owner Address Line 2
  #py_addr_city       #Property Year Owner Address City
  #py_addr_state      #Property Year Owner Address State
  #py_addr_zip        #Property Year Owner Address Zip (Zip Only)
  #py_addr_zip_cass   #Property Year Owner Address Zip Cass (Cass Only)
  #tract_or_lot       #Tract/lot
  #appraised_val      #price of home if put on the market
  #assessed_val       #value used to determine property taxes
#  #deed_book_id       #Deed Book ID  - did not use in extracting
#  #deed_book_page     #Deed Book Page  - did not use in extracting
#  #deed_dt            #Deed Date - did not use in extracting
  #mortage_co_name    #Name of Mortgage Company
  #mortgage_acct_id   #Mortgage Loan Number
  #hs_exempt          #Homestead Exemption    (‘T’ or ‘F’) ‘T’ indicates that the property has the exemption
#  #land_acres         #Sum of the acres based on land segments - did not use in extracting
  #market_value       #Property market value
  #ht_exempt          #Historical Exemption (‘T’ or ‘F’)

#The line of code to extract just the variables/columns needed. Do this for each year's dataset: 
#x<-df[, .(prop_id,prop_type_cd,py_owner_name,
#                    py_owner_name, py_addr_line1,py_addr_line2,
#                    py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
#                    tract_or_lot,appraised_val,assessed_val,mortage_co_name,
#                    mortgage_acct_id,hs_exempt,market_value,ht_exempt)] 


##How I am defining Corporate Landlords--
#If the 'py_owner_name'column has any of the following, it is to be considered as a Corporate Landlord:
  #LLC
  #LTD
  #LP
  #Inc
  #Trust
  #Corp
  #Homes
  #Assoc
  #Properties
  #Estate
  #Property Management

#This way of identifying corporate landlords may capture small mom and pop investors, which isn't so much the issue.
#



###DATA LOADING#

# 2022 Data Read
DT_2022<- fread("BCAD DATA/2022_2022-10-11_006099_APPRAISAL_INFO.TXT", header = FALSE, sep = "\n")

DT22<-DT_2022[ , lapply(seq_len(length(f3_fields$Start)), function(ii) {
  stringi::stri_sub(V1, f3_fields$Start[ii], f3_fields$End[ii])
})]

names(DT22)<-f3_fields$`Field Name` # adding names

names(DT22)<-f3_fields$'Field Name'
# END 2022 Data Read


# 2021 Data Read
DT_2021<- fread("BCAD DATA/2021_2022-10-26_006100_APPRAISAL_INFO.TXT", header = FALSE, sep = "\n")

DT21<-DT_2021[ , lapply(seq_len(length(f3_fields$Start)), function(ii) {
  stringi::stri_sub(V1, f3_fields$Start[ii], f3_fields$End[ii])
})]

names(DT21)<-f3_fields$`Field Name` # adding names

names(DT21)<-f3_fields$'Field Name'
# END 2021 Data Read


# 2020 Data Read
DT_2020<- fread("BCAD DATA/2020_2022-10-27_006101_APPRAISAL_INFO.TXT", header = FALSE, sep = "\n")

DT20<-DT_2020[ , lapply(seq_len(length(f3_fields$Start)), function(ii) {
  stringi::stri_sub(V1, f3_fields$Start[ii], f3_fields$End[ii])
})]

names(DT20)<-f3_fields$`Field Name` # adding names

names(DT20)<-f3_fields$'Field Name'
# END 2020 Data Read

# 2019 Data Read
DT_2019<- fread("BCAD DATA/2019_2022-10-27_006102_APPRAISAL_INFO.TXT", header = FALSE, sep = "\n")

DT19<-DT_2019[ , lapply(seq_len(length(f3_fields$Start)), function(ii) {
  stringi::stri_sub(V1, f3_fields$Start[ii], f3_fields$End[ii])
})]

names(DT19)<-f3_fields$`Field Name` # adding names

names(DT19)<-f3_fields$'Field Name'
# END 2019 Data Read


# 2018 Data Read
DT_2018<- fread("BCAD DATA/2018_2021-09-10_005774_APPRAISAL_INFO.TXT", header = FALSE, sep = "\n")

DT18<-DT_2018[ , lapply(seq_len(length(f3_fields$Start)), function(ii) {
  stringi::stri_sub(V1, f3_fields$Start[ii], f3_fields$End[ii])
})]

names(DT18)<-f3_fields$`Field Name` # adding names

names(DT18)<-f3_fields$'Field Name'
# END 2019 Data Read

#[END DATA LOADING]###





###[Data Cleanup section]
# Each year's appraisal datatable will be cleaned up to show a few columns of data.

Apprsl_22<-DT22[, .(prop_id,prop_type_cd,py_owner_name,
                    py_owner_name, py_addr_line1,py_addr_line2,
                    py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
                    tract_or_lot,appraised_val,assessed_val,mortage_co_name,
                    mortgage_acct_id,hs_exempt,market_value,ht_exempt)] 

Apprsl_21<-DT21[, .(prop_id,prop_type_cd,py_owner_name,
                    py_owner_name, py_addr_line1,py_addr_line2,
                    py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
                    tract_or_lot,appraised_val,assessed_val,mortage_co_name,
                    mortgage_acct_id,hs_exempt,market_value,ht_exempt)] 

Apprsl_20<-DT20[, .(prop_id,prop_type_cd,py_owner_name,
                    py_owner_name, py_addr_line1,py_addr_line2,
                    py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
                    tract_or_lot,appraised_val,assessed_val,mortage_co_name,
                    mortgage_acct_id,hs_exempt,market_value,ht_exempt)] 

Apprsl_19<-DT19[, .(prop_id,prop_type_cd,py_owner_name,
                    py_owner_name, py_addr_line1,py_addr_line2,
                    py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
                    tract_or_lot,appraised_val,assessed_val,mortage_co_name,
                    mortgage_acct_id,hs_exempt,market_value,ht_exempt)] 

Apprsl_18<-DT18[, .(prop_id,prop_type_cd,py_owner_name,
                    py_owner_name, py_addr_line1,py_addr_line2,
                    py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
                    tract_or_lot,appraised_val,assessed_val,mortage_co_name,
                    mortgage_acct_id,hs_exempt,market_value,ht_exempt)] 



#Names for Corporate Landlords in the pyowner name column
#2022
LLC_sum22<-Apprsl_22[,sum(grepl(pattern = "LLC",x = py_owner_name))]
LTD_sum22<-Apprsl_22[,sum(grepl(pattern = "LTD",x = py_owner_name))]
LP_sum22<-Apprsl_22[,sum(grepl(pattern = "LP",x = py_owner_name))]
Inc_sum22<-Apprsl_22[,sum(grepl(pattern = "INC",x = py_owner_name))]
Trust_sum22<-Apprsl_22[,sum(grepl(pattern = "TRUST",x = py_owner_name))]
Corp_sum22<-Apprsl_22[,sum(grepl(pattern = "CORP",x = py_owner_name))]
Homes_sum22<-Apprsl_22[,sum(grepl(pattern = "HOMES",x = py_owner_name))]
Assoc_sum22<-Apprsl_22[,sum(grepl(pattern = "ASSOC",x = py_owner_name))]
Prprts_sum22<-Apprsl_22[,sum(grepl(pattern = "PROPERTIES",x = py_owner_name))]
Estate_sum22<-Apprsl_22[,sum(grepl(pattern = "ESTATE",x = py_owner_name))]
PrprtyMngmnt_sum22<-Apprsl_22[,sum(grepl(pattern = "PROPERTY MANAGEMENT",x = py_owner_name))]

#2021
LLC_sum21<-Apprsl_21[,sum(grepl(pattern = "LLC",x = py_owner_name))]
LTD_sum21<-Apprsl_21[,sum(grepl(pattern = "LTD",x = py_owner_name))]
LP_sum21<-Apprsl_21[,sum(grepl(pattern = "LP",x = py_owner_name))]
Inc_sum21<-Apprsl_21[,sum(grepl(pattern = "INC",x = py_owner_name))]
Trust_sum21<-Apprsl_21[,sum(grepl(pattern = "TRUST",x = py_owner_name))]
Corp_sum21<-Apprsl_21[,sum(grepl(pattern = "CORP",x = py_owner_name))]
Homes_sum21<-Apprsl_21[,sum(grepl(pattern = "HOMES",x = py_owner_name))]
Assoc_sum21<-Apprsl_21[,sum(grepl(pattern = "ASSOC",x = py_owner_name))]
Prprts_sum21<-Apprsl_21[,sum(grepl(pattern = "PROPERTIES",x = py_owner_name))]
Estate_sum21<-Apprsl_21[,sum(grepl(pattern = "ESTATE",x = py_owner_name))]
PrprtyMngmnt_sum21<-Apprsl_21[,sum(grepl(pattern = "PROPERTY MANAGEMENT",x = py_owner_name))]

#2020
LLC_sum20<-Apprsl_20[,sum(grepl(pattern = "LLC",x = py_owner_name))]
LTD_sum20<-Apprsl_20[,sum(grepl(pattern = "LTD",x = py_owner_name))]
LP_sum20<-Apprsl_20[,sum(grepl(pattern = "LP",x = py_owner_name))]
Inc_sum20<-Apprsl_20[,sum(grepl(pattern = "INC",x = py_owner_name))]
Trust_sum20<-Apprsl_20[,sum(grepl(pattern = "TRUST",x = py_owner_name))]
Corp_sum20<-Apprsl_20[,sum(grepl(pattern = "CORP",x = py_owner_name))]
Homes_sum20<-Apprsl_20[,sum(grepl(pattern = "HOMES",x = py_owner_name))]
Assoc_sum20<-Apprsl_20[,sum(grepl(pattern = "ASSOC",x = py_owner_name))]
Prprts_sum20<-Apprsl_20[,sum(grepl(pattern = "PROPERTIES",x = py_owner_name))]
Estate_sum20<-Apprsl_20[,sum(grepl(pattern = "ESTATE",x = py_owner_name))]
PrprtyMngmnt_sum20<-Apprsl_20[,sum(grepl(pattern = "PROPERTY MANAGEMENT",x = py_owner_name))]

#2019
LLC_sum19<-Apprsl_19[,sum(grepl(pattern = "LLC",x = py_owner_name))]
LTD_sum19<-Apprsl_19[,sum(grepl(pattern = "LTD",x = py_owner_name))]
LP_sum19<-Apprsl_19[,sum(grepl(pattern = "LP",x = py_owner_name))]
Inc_sum19<-Apprsl_19[,sum(grepl(pattern = "INC",x = py_owner_name))]
Trust_sum19<-Apprsl_19[,sum(grepl(pattern = "TRUST",x = py_owner_name))]
Corp_sum19<-Apprsl_19[,sum(grepl(pattern = "CORP",x = py_owner_name))]
Homes_sum19<-Apprsl_19[,sum(grepl(pattern = "HOMES",x = py_owner_name))]
Assoc_sum19<-Apprsl_19[,sum(grepl(pattern = "ASSOC",x = py_owner_name))]
Prprts_sum19<-Apprsl_19[,sum(grepl(pattern = "PROPERTIES",x = py_owner_name))]
Estate_sum19<-Apprsl_19[,sum(grepl(pattern = "ESTATE",x = py_owner_name))]
PrprtyMngmnt_sum19<-Apprsl_19[,sum(grepl(pattern = "PROPERTY MANAGEMENT",x = py_owner_name))]

#2018
LLC_sum18<-Apprsl_18[,sum(grepl(pattern = "LLC",x = py_owner_name))]
LTD_sum18<-Apprsl_18[,sum(grepl(pattern = "LTD",x = py_owner_name))]
LP_sum18<-Apprsl_18[,sum(grepl(pattern = "LP",x = py_owner_name))]
Inc_sum18<-Apprsl_18[,sum(grepl(pattern = "INC",x = py_owner_name))]
Trust_sum18<-Apprsl_18[,sum(grepl(pattern = "TRUST",x = py_owner_name))]
Corp_sum18<-Apprsl_18[,sum(grepl(pattern = "CORP",x = py_owner_name))]
Homes_sum18<-Apprsl_18[,sum(grepl(pattern = "HOMES",x = py_owner_name))]
Assoc_sum18<-Apprsl_18[,sum(grepl(pattern = "ASSOC",x = py_owner_name))]
Prprts_sum18<-Apprsl_18[,sum(grepl(pattern = "PROPERTIES",x = py_owner_name))]
Estate_sum18<-Apprsl_18[,sum(grepl(pattern = "ESTATE",x = py_owner_name))]
PrprtyMngmnt_sum18<-Apprsl_18[,sum(grepl(pattern = "PROPERTY MANAGEMENT",x = py_owner_name))]

# x<-Apprsl_18[Apprsl_18$py_owner_name == "LLC", ] # Attempting to extract just the rows withs LLC in it. but this line of code doesn't work


#Counting Corporate Landlords for Each Year
totalCL22<-sum(LLC_sum22,LTD_sum22,LP_sum22,Inc_sum22,Trust_sum22,Corp_sum22, Homes_sum22, Assoc_sum22,Prprts_sum22,Estate_sum22, PrprtyMngmnt_sum22)
    #146,937 Corproate Landlords as identified with the above names IN 2022
totalCL21<-sum(LLC_sum21,LTD_sum21,LP_sum21,Inc_sum21,Trust_sum21,Corp_sum21, Homes_sum21, Assoc_sum21,Prprts_sum21,Estate_sum21, PrprtyMngmnt_sum21)
    #
totalCL20<-sum(LLC_sum20,LTD_sum20,LP_sum20,Inc_sum20,Trust_sum20,Corp_sum20, Homes_sum20, Assoc_sum20,Prprts_sum20,Estate_sum20, PrprtyMngmnt_sum20)
    #
totalCL19<-sum(LLC_sum19,LTD_sum19,LP_sum19,Inc_sum19,Trust_sum19,Corp_sum19, Homes_sum19, Assoc_sum19,Prprts_sum19,Estate_sum19, PrprtyMngmnt_sum19)
    #
totalCL18<-sum(LLC_sum18,LTD_sum18,LP_sum18,Inc_sum18,Trust_sum18,Corp_sum18, Homes_sum18, Assoc_sum18,Prprts_sum18,Estate_sum18, PrprtyMngmnt_sum18)
    #112584 TOTAL CL 




