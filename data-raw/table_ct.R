## code to prepare `table_ct` dataset goes here

library(data.table)
table_ct <- data.table(
  category = c(
    "Origin, US-born",
    "Origin, foreign-born",
    "Disability, any disability",
    "Age, 10-19",
    "Age, under 18",
    "Age, 18-39",
    "Age, 40-64",
    "Age, 65 and over",
    "Race, White",
    "Race, Black",
    "Race, Asian",
    "Race, American Indian",
    "Race, Pacific Islander",
    "Race, Other",
    "Race, Multiracial",
    "Ethnicity, Hispanic",
    "Ethnicity, Not Hispanic",
    "Income, Median Household Income"
  ),
  column = c(
    "usborncit_percent",
    "forborn_percent",
    "anydis_percent",
    "age_10_19_percent",
    "ageunder18_percent",
    "age18_39_percent",
    "age40_64_percent",
    "age65up_percent",
    "whitenh_percent",
    "blacknh_percent",
    "asiannh_percent",
    "amindnh_percent",
    "pacificnh_percent",
    "othernh_percent",
    "multracenh_percent",
    "hisppop_percent",
    "nothisppop_percent",
    "medianhhi"
  )
)



usethis::use_data(table_ct, overwrite = TRUE)
