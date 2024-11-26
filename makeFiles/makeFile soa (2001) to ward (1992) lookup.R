# soa code to ward lookup
## All 1992 wards are same as wards or subdivision of wards except 6 SOAs in Moyle
## which are mergers of small wards
## All SOA code contain their 6 digit ward code (with exceptions in moyle)

library(tidyverse)
library(readODS)

## check ward to SOA looks
lookup <- 
  read_ods(
    'data/Lookup Table - SA, OA and SOA (statistical geographies).ods',
    sheet = 3,
    skip = 4
  )

ward_codes <-
  'data/Area-Classification-(administrative geographies)-Ward-2001.ods' %>% 
  read_ods(
    sheet = 1,
    skip = 2
  )

## I think there's a relationship between ward code and SOA code 

# lookup$`SOA Code` 
# ward_codes

lookup %>%
  filter(
    !(`SOA Code` %>% substr(1,6) %in% ward_codes$`Ward Code`)
  )
### Okay so only 6 SOAs are missing -- all in Moyle and this is because they are 
### mergers of wards 

# SOA Code                            SOA Name          HSCT       AA1998       AA2008 LGD1992NAME           WARD1992(1)
# 1 95UU99C1    Armoy and Moss-Side and Moyarget Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE                 ARMOY
# 2 95UU99C2            Ballylough and Bushmills Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE            BALLYLOUGH
# 3 95UU99C3 Bonamargy and Rathlin and Glenshesk Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE BONAMARGY AND RATHLIN
# 4 95UU99C4            Carnmoon and Dunseverick Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE              CARNMOON
# 5 95UU99C5                 Glenaan and Glendun Northern HSCT NORTH ANTRIM  EAST ANTRIM       MOYLE               GLENAAN
# 6 95UU99C6              Glentaisie and Kinbane Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE            GLENTAISIE


# --------------------------------
lookup <-
  lookup %>%
  rename(
    soa_code = 'SOA Code',
    soa_name = 'SOA Name'
  ) %>%
  mutate(
    ward_code = soa_code %>% substr(1,6)
  ) %>%
  left_join(
    ward_codes, by = c(ward_code = 'Ward Code')
  )
## manual input

exceptions_df <-
  list(
    data.frame(soa_code = '95UU99C1', soa_name =    'Armoy and Moss-Side and Moyarget', ward_code = '95UU01'),  #Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE                 ARMOY
    data.frame(soa_code = '95UU99C1', soa_name =    'Armoy and Moss-Side and Moyarget', ward_code = '95UU15'), #Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE                 ARMOY
    data.frame(soa_code = '95UU99C2', soa_name =            'Ballylough and Bushmills', ward_code = '95UU02'),#Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE            BALLYLOUGH
    data.frame(soa_code = '95UU99C2', soa_name =            'Ballylough and Bushmills', ward_code = '95UU04'),#Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE            BALLYLOUGH
    data.frame(soa_code = '95UU99C3', soa_name = 'Bonamargy and Rathlin and Glenshesk', ward_code = '95UU03'),#Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE BONAMARGY AND RATHLIN
    data.frame(soa_code = '95UU99C3', soa_name = 'Bonamargy and Rathlin and Glenshesk', ward_code = '95UU11'),#Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE BONAMARGY AND RATHLIN
    data.frame(soa_code = '95UU99C4', soa_name =            'Carnmoon and Dunseverick', ward_code = '95UU05'),#Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE              CARNMOON
    data.frame(soa_code = '95UU99C4', soa_name =            'Carnmoon and Dunseverick', ward_code = '95UU07'),#Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE              CARNMOON
    data.frame(soa_code = '95UU99C5', soa_name =                 'Glenaan and Glendun', ward_code = '95UU08'),#Northern HSCT NORTH ANTRIM  EAST ANTRIM       MOYLE               GLENAAN
    data.frame(soa_code = '95UU99C5', soa_name =                 'Glenaan and Glendun', ward_code = '95UU10'),#Northern HSCT NORTH ANTRIM  EAST ANTRIM       MOYLE               GLENAAN
    data.frame(soa_code = '95UU99C6', soa_name =              'Glentaisie and Kinbane', ward_code = '95UU12'),#Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE            GLENTAISIE
    data.frame(soa_code = '95UU99C6', soa_name =              'Glentaisie and Kinbane', ward_code = '95UU13')#Northern HSCT NORTH ANTRIM NORTH ANTRIM       MOYLE            GLENTAISIE
  ) %>%
  bind_rows()
  

## add

lookup <-
  lookup %>% 
  filter(!is.na(`Ward name`))


lookup <-
  lookup %>%
  bind_rows(exceptions_df)

lookup %>% tail

## write out the data
lookup %>%
  select(soa_code, ward_code) %>%
  write_csv('outputs/soa (2001) to ward (1992) lookup.csv')
