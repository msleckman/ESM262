##########################
# ESM 262 - Assignment 1
# Margaux Sleckman
#
#########################

#---Libraries---#

library(readr)
library(tidyverse)
library(dplyr)
install.packages("skimr")
library(skimr)


#---read in file---# 
View(head(parcels, 50))
# parcels <- read_csv("Assignment1/Santa_Barbara_County_parcels_2011.csv")
# View(head(parcels, 50))
parcels_raw <- read_csv("Assignment1/Santa_Barbara_County_parcels_2011.csv")
View(head(parcels_raw, 50))
str(parcels)
skim(parcels)

parcel_select <- select(parcels_raw, 
                        APN, 
                        Home_address = Situs1,
                        City_adress = Situs2,
                        Acreage, 
                        UseCode, 
                        NonTaxCode, 
                        AgPres, 
                        Land_Value = LandValue,
                        Net_Impr, 
                        Net_AV, 
                        Mail_Address1 = M_Address1, 
                        Mail_Address2 = M_Address2)

dim(parcel_select)


# Lets look at the data now that its 
skim(parcel_select)

lapply(parcel_select, class)
anyNA(lapply(parcel_select, unique))

View(head(parcel_select, 50))
### defined as type character : 
# 1. City_address --> makes sense there are words
# 2. Home_address --> makes sense there are words 
# 3. M_Address1 --> address with words 
# 4. M_address2 --> address with words 
# 5. nontaxcode --> letter plus numbers 
# 6. usecode --> letters plus numbers 
### Defined as numeric: 
# 6. Acreage --> number, unit ... 
# 7. Land_Value --> number, unit $$ 
# 8. Net_AV --> number, idk what unit 
# 9. Net_Impr --> number, idk what unit

skim(as.tibble(parcel_select[,6:10]))
skim(as.tibble(parcel_select[,1:5]))


# for(i in nrow(parcel_select){
#   if(is.na(parcel_select)
    
    
  }
    
  
}
    