library(readxl)
setwd("/Users/hp/Documents/SME_lab/NDMC_TB/Comorbidity/")

#2017

Jan_17 <- read_excel("ComorbidityRegister_2017_January.xlsx")
colnames(Jan_17) <- Jan_17[5, ]
Jan_17 <- Jan_17[-(1:5),]
Jan_17$Month <- "January"
Jan_17$Year <- "2017"

Feb_17 <- read_excel("ComorbidityRegister_2017_February.xlsx")
colnames(Feb_17) <- Feb_17[5, ]
Feb_17 <- Feb_17[-(1:5),]
Feb_17$Month <- "February"
Feb_17$Year <- "2017"

Mar_17 <- read_excel("ComorbidityRegister_2017_March.xlsx")
colnames(Mar_17) <- Mar_17[5, ]
Mar_17 <- Mar_17[-(1:5),]
Mar_17$Month <- "March"
Mar_17$Year <- "2017"

Apr_17 <- read_excel("ComorbidityRegister_2017_April.xlsx")
colnames(Apr_17) <- Apr_17[5, ]
Apr_17 <- Apr_17[-(1:5),]
Apr_17$Month <- "April"
Apr_17$Year <- "2017"


May_17 <- read_excel("ComorbidityRegister_2017_May.xlsx")
colnames(May_17) <- May_17[5, ]
May_17 <- May_17[-(1:5),]
May_17$Month <- "May"
May_17$Year <- "2017"

Jun_17 <- read_excel("ComorbidityRegister_2017_June.xlsx")
colnames(Jun_17) <- Jun_17[5, ]
Jun_17 <- Jun_17[-(1:5),]
Jun_17$Month <- "June"
Jun_17$Year <- "2017"

Jul_17 <- read_excel("ComorbidityRegister_2017_July.xlsx")
colnames(Jul_17) <- Jul_17[5, ]
Jul_17 <- Jul_17[-(1:5),]
Jul_17$Month <- "July"
Jul_17$Year <- "2017"

Aug_17 <- read_excel("ComorbidityRegister_2017_August.xlsx")
colnames(Aug_17) <- Aug_17[5, ]
Aug_17 <- Aug_17[-(1:5),]
Aug_17$Month <- "August"
Aug_17$Year <- "2017"

Sept_17 <- read_excel("ComorbidityRegister_2017_September.xlsx")
colnames(Sept_17) <- Sept_17[5, ]
Sept_17 <- Sept_17[-(1:5),]
Sept_17$Month <- "September"
Sept_17$Year <- "2017"

Oct_17 <- read_excel("ComorbidityRegister_2017_October.xlsx")
colnames(Oct_17) <- Oct_17[5, ]
Oct_17 <- Oct_17[-(1:5),]
Oct_17$Month <- "October"
Oct_17$Year <- "2017"

Nov_17 <- read_excel("ComorbidityRegister_2017_November.xlsx")
colnames(Nov_17) <- Nov_17[5, ]
Nov_17 <- Nov_17[-(1:5),]
Nov_17$Month <- "November"
Nov_17$Year <- "2017"

Dec_17 <- read_excel("ComorbidityRegister_2017_December.xlsx")
colnames(Dec_17) <- Dec_17[5, ]
Dec_17 <- Dec_17[-(1:5),]
Dec_17$Month <- "December"
Dec_17$Year <- "2017"


#2018

Jan_18 <- read_excel("ComorbidityRegister_2018_January.xlsx")
colnames(Jan_18) <- Jan_18[5, ]
Jan_18 <- Jan_18[-(1:5),]
Jan_18$Month <- "January"
Jan_18$Year <- "2018"

Feb_18 <- read_excel("ComorbidityRegister_2018_February.xlsx")
colnames(Feb_18) <- Feb_18[5, ]
Feb_18 <- Feb_18[-(1:5),]
Feb_18$Month <- "February"
Feb_18$Year <- "2018"

Mar_18 <- read_excel("ComorbidityRegister_2018_March.xlsx")
colnames(Mar_18) <- Mar_18[5, ]
Mar_18 <- Mar_18[-(1:5),]
Mar_18$Month <- "March"
Mar_18$Year <- "2018"

Apr_18 <- read_excel("ComorbidityRegister_2018_April.xlsx")
colnames(Apr_18) <- Apr_18[5, ]
Apr_18 <- Apr_18[-(1:5),]
Apr_18$Month <- "April"
Apr_18$Year <- "2018"


May_18 <- read_excel("ComorbidityRegister_2018_May.xlsx")
colnames(May_18) <- May_18[5, ]
May_18 <- May_18[-(1:5),]
May_18$Month <- "May"
May_18$Year <- "2018"

Jun_18 <- read_excel("ComorbidityRegister_2018_June.xlsx")
colnames(Jun_18) <- Jun_18[5, ]
Jun_18 <- Jun_18[-(1:5),]
Jun_18$Month <- "June"
Jun_18$Year <- "2018"

Jul_18 <- read_excel("ComorbidityRegister_2018_July.xlsx")
colnames(Jul_18) <- Jul_18[5, ]
Jul_18 <- Jul_18[-(1:5),]
Jul_18$Month <- "July"
Jul_18$Year <- "2018"

Aug_18 <- read_excel("ComorbidityRegister_2018_August.xlsx")
colnames(Aug_18) <- Aug_18[5, ]
Aug_18 <- Aug_18[-(1:5),]
Aug_18$Month <- "August"
Aug_18$Year <- "2018"

Sept_18 <- read_excel("ComorbidityRegister_2018_September.xlsx")
colnames(Sept_18) <- Sept_18[5, ]
Sept_18 <- Sept_18[-(1:5),]
Sept_18$Month <- "September"
Sept_18$Year <- "2018"

Oct_18 <- read_excel("ComorbidityRegister_2018_October.xlsx")
colnames(Oct_18) <- Oct_18[5, ]
Oct_18 <- Oct_18[-(1:5),]
Oct_18$Month <- "October"
Oct_18$Year <- "2018"

Nov_18 <- read_excel("ComorbidityRegister_2018_November.xlsx")
colnames(Nov_18) <- Nov_18[5, ]
Nov_18 <- Nov_18[-(1:5),]
Nov_18$Month <- "November"
Nov_18$Year <- "2018"

Dec_18 <- read_excel("ComorbidityRegister_2018_December.xlsx")
colnames(Dec_18) <- Dec_18[5, ]
Dec_18 <- Dec_18[-(1:5),]
Dec_18$Month <- "December"
Dec_18$Year <- "2018"

#2019

Jan_19 <- read_excel("ComorbidityRegister_2019_January.xlsx")
colnames(Jan_19) <- Jan_19[5, ]
Jan_19 <- Jan_19[-(1:5),]
Jan_19$Month <- "January"
Jan_19$Year <- "2019"

Feb_19 <- read_excel("ComorbidityRegister_2019_February.xlsx")
colnames(Feb_19) <- Feb_19[5, ]
Feb_19 <- Feb_19[-(1:5),]
Feb_19$Month <- "February"
Feb_19$Year <- "2019"

Mar_19 <- read_excel("ComorbidityRegister_2019_March.xlsx")
colnames(Mar_19) <- Mar_19[5, ]
Mar_19 <- Mar_19[-(1:5),]
Mar_19$Month <- "March"
Mar_19$Year <- "2019"

Apr_19 <- read_excel("ComorbidityRegister_2019_April.xlsx")
colnames(Apr_19) <- Apr_19[5, ]
Apr_19 <- Apr_19[-(1:5),]
Apr_19$Month <- "April"
Apr_19$Year <- "2019"


May_19 <- read_excel("ComorbidityRegister_2019_May.xlsx")
colnames(May_19) <- May_19[5, ]
May_19 <- May_19[-(1:5),]
May_19$Month <- "May"
May_19$Year <- "2019"

Jun_19 <- read_excel("ComorbidityRegister_2019_June.xlsx")
colnames(Jun_19) <- Jun_19[5, ]
Jun_19 <- Jun_19[-(1:5),]
Jun_19$Month <- "June"
Jun_19$Year <- "2019"

Jul_19 <- read_excel("ComorbidityRegister_2019_July.xlsx")
colnames(Jul_19) <- Jul_19[5, ]
Jul_19 <- Jul_19[-(1:5),]
Jul_19$Month <- "July"
Jul_19$Year <- "2019"

Aug_19 <- read_excel("ComorbidityRegister_2019_August.xlsx")
colnames(Aug_19) <- Aug_19[5, ]
Aug_19 <- Aug_19[-(1:5),]
Aug_19$Month <- "August"
Aug_19$Year <- "2019"

Sept_19 <- read_excel("ComorbidityRegister_2019_September.xlsx")
colnames(Sept_19) <- Sept_19[5, ]
Sept_19 <- Sept_19[-(1:5),]
Sept_19$Month <- "September"
Sept_19$Year <- "2019"

Oct_19 <- read_excel("ComorbidityRegister_2019_October.xlsx")
colnames(Oct_19) <- Oct_19[5, ]
Oct_19 <- Oct_19[-(1:5),]
Oct_19$Month <- "October"
Oct_19$Year <- "2019"

Nov_19 <- read_excel("ComorbidityRegister_2019_November.xlsx")
colnames(Nov_19) <- Nov_19[5, ]
Nov_19 <- Nov_19[-(1:5),]
Nov_19$Month <- "November"
Nov_19$Year <- "2019"

Dec_19 <- read_excel("ComorbidityRegister_2019_December.xlsx")
colnames(Dec_19) <- Dec_19[5, ]
Dec_19 <- Dec_19[-(1:5),]
Dec_19$Month <- "December"
Dec_19$Year <- "2019"

#2020

Jan_20 <- read_excel("ComorbidityRegister_2020_January.xlsx")
colnames(Jan_20) <- Jan_20[5, ]
Jan_20 <- Jan_20[-(1:5),]
Jan_20$Month <- "January"
Jan_20$Year <- "2020"

Feb_20 <- read_excel("ComorbidityRegister_2020_February.xlsx")
colnames(Feb_20) <- Feb_20[5, ]
Feb_20 <- Feb_20[-(1:5),]
Feb_20$Month <- "February"
Feb_20$Year <- "2020"

Mar_20 <- read_excel("ComorbidityRegister_2020_March.xlsx")
colnames(Mar_20) <- Mar_20[5, ]
Mar_20 <- Mar_20[-(1:5),]
Mar_20$Month <- "March"
Mar_20$Year <- "2020"

Apr_20 <- read_excel("ComorbidityRegister_2020_April.xlsx")
colnames(Apr_20) <- Apr_20[5, ]
Apr_20 <- Apr_20[-(1:5),]
Apr_20$Month <- "April"
Apr_20$Year <- "2020"


May_20 <- read_excel("ComorbidityRegister_2020_May.xlsx")
colnames(May_20) <- May_20[5, ]
May_20 <- May_20[-(1:5),]
May_20$Month <- "May"
May_20$Year <- "2020"

Jun_20 <- read_excel("ComorbidityRegister_2020_June.xlsx")
colnames(Jun_20) <- Jun_20[5, ]
Jun_20 <- Jun_20[-(1:5),]
Jun_20$Month <- "June"
Jun_20$Year <- "2020"

Jul_20 <- read_excel("ComorbidityRegister_2020_July.xlsx")
colnames(Jul_20) <- Jul_20[5, ]
Jul_20 <- Jul_20[-(1:5),]
Jul_20$Month <- "July"
Jul_20$Year <- "2020"

Aug_20 <- read_excel("ComorbidityRegister_2020_August.xlsx")
colnames(Aug_20) <- Aug_20[5, ]
Aug_20 <- Aug_20[-(1:5),]
Aug_20$Month <- "August"
Aug_20$Year <- "2020"

#Sept_20 <- read_excel("ComorbidityRegister_2020_September.xlsx")
#colnames(Sept_20) <- Sept_20[5, ]
#Sept_20 <- Sept_20[-(1:5),]
#Sept_20$Month <- "September"
#Sept_20$Year <- "2020"

Oct_20 <- read_excel("ComorbidityRegister_2020_October.xlsx")
colnames(Oct_20) <- Oct_20[5, ]
Oct_20 <- Oct_20[-(1:5),]
Oct_20$Month <- "October"
Oct_20$Year <- "2020"

Nov_20 <- read_excel("ComorbidityRegister_2020_November.xlsx")
colnames(Nov_20) <- Nov_20[5, ]
Nov_20 <- Nov_20[-(1:5),]
Nov_20$Month <- "November"
Nov_20$Year <- "2020"

Dec_20 <- read_excel("ComorbidityRegister_2020_December.xlsx")
colnames(Dec_20) <- Dec_20[5, ]
Dec_20 <- Dec_20[-(1:5),]
Dec_20$Month <- "December"
Dec_20$Year <- "2020"

#2021

Jan_21 <- read_excel("ComorbidityRegister_2021_January.xlsx")
colnames(Jan_21) <- Jan_21[5, ]
Jan_21 <- Jan_21[-(1:5),]
Jan_21$Month <- "January"
Jan_21$Year <- "2021"

Feb_21 <- read_excel("ComorbidityRegister_2021_February.xlsx")
colnames(Feb_21) <- Feb_21[5, ]
Feb_21 <- Feb_21[-(1:5),]
Feb_21$Month <- "February"
Feb_21$Year <- "2021"

Mar_21 <- read_excel("ComorbidityRegister_2021_March.xlsx")
colnames(Mar_21) <- Mar_21[5, ]
Mar_21 <- Mar_21[-(1:5),]
Mar_21$Month <- "March"
Mar_21$Year <- "2021"

Apr_21 <- read_excel("ComorbidityRegister_2021_April.xlsx")
colnames(Apr_21) <- Apr_21[5, ]
Apr_21 <- Apr_21[-(1:5),]
Apr_21$Month <- "April"
Apr_21$Year <- "2021"


May_21 <- read_excel("ComorbidityRegister_2021_May.xlsx")
colnames(May_21) <- May_21[5, ]
May_21 <- May_21[-(1:5),]
May_21$Month <- "May"
May_21$Year <- "2021"

Jun_21 <- read_excel("ComorbidityRegister_2021_June.xlsx")
colnames(Jun_21) <- Jun_21[5, ]
Jun_21 <- Jun_21[-(1:5),]
Jun_21$Month <- "June"
Jun_21$Year <- "2021"

Jul_21 <- read_excel("ComorbidityRegister_2021_July.xlsx")
colnames(Jul_21) <- Jul_21[5, ]
Jul_21 <- Jul_21[-(1:5),]
Jul_21$Month <- "July"
Jul_21$Year <- "2021"

Aug_21 <- read_excel("ComorbidityRegister_2021_August.xlsx")###
colnames(Aug_21) <- Aug_21[5, ]
Aug_21 <- Aug_21[-(1:5),]
Aug_21$Month <- "August"
Aug_21$Year <- "2021"

Sept_21 <- read_excel("ComorbidityRegister_2021_September.xlsx")
colnames(Sept_21) <- Sept_21[5, ]
Sept_21 <- Sept_21[-(1:5),]
Sept_21$Month <- "September"
Sept_21$Year <- "2021"

Oct_21 <- read_excel("ComorbidityRegister_2021_October.xlsx")
colnames(Oct_21) <- Oct_21[5, ]
Oct_21 <- Oct_21[-(1:5),]
Oct_21$Month <- "October"
Oct_21$Year <- "2021"

Nov_21 <- read_excel("ComorbidityRegister_2021_November.xlsx")###
colnames(Nov_21) <- Nov_21[5, ]
Nov_21 <- Nov_21[-(1:5),]
Nov_21$Month <- "November"
Nov_21$Year <- "2021"

Dec_21 <- read_excel("ComorbidityRegister_2021_December.xlsx")####
colnames(Dec_21) <- Dec_21[5, ]
Dec_21 <- Dec_21[-(1:5),]
Dec_21$Month <- "December"
Dec_21$Year <- "2021"

#2022

Jan_22 <- read_excel("ComorbidityRegister_2022_January.xlsx")
colnames(Jan_22) <- Jan_22[5, ]
Jan_22 <- Jan_22[-(1:7),]
Jan_22$Month <- "January"
Jan_22$Year <- "2022"

Feb_22 <- read_excel("ComorbidityRegister_2022_February.xlsx")###
colnames(Feb_22) <- Feb_22[5, ]
Feb_22 <- Feb_22[-(1:5),]
Feb_22$Month <- "February"
Feb_22$Year <- "2022"

Mar_22 <- read_excel("ComorbidityRegister_2022_March.xlsx")
colnames(Mar_22) <- Mar_22[5, ]
Mar_22 <- Mar_22[-(1:5),]
Mar_22$Month <- "March"
Mar_22$Year <- "2022"

Apr_22 <- read_excel("ComorbidityRegister_2022_April.xlsx")
colnames(Apr_22) <- Apr_22[5, ]
Apr_22 <- Apr_22[-(1:5),]
Apr_22$Month <- "April"
Apr_22$Year <- "2022"


May_22 <- read_excel("ComorbidityRegister_2022_May.xlsx")###
colnames(May_22) <- May_22[5, ]
May_22 <- May_22[-(1:5),]
May_22$Month <- "May"
May_22$Year <- "2022"

Jun_22 <- read_excel("ComorbidityRegister_2022_June.xlsx")
colnames(Jun_22) <- Jun_22[5, ]
Jun_22 <- Jun_22[-(1:5),]
Jun_22$Month <- "June"
Jun_22$Year <- "2022"

Jul_22 <- read_excel("ComorbidityRegister_2022_July.xlsx")
colnames(Jul_22) <- Jul_22[5, ]
Jul_22 <- Jul_22[-(1:5),]
Jul_22$Month <- "July"
Jul_22$Year <- "2022"

Aug_22 <- read_excel("ComorbidityRegister_2022_August.xlsx")
colnames(Aug_22) <- Aug_22[5, ]
Aug_22 <- Aug_22[-(1:5),]
Aug_22$Month <- "August"
Aug_22$Year <- "2022"

Sept_22 <- read_excel("ComorbidityRegister_2022_September.xlsx")###
colnames(Sept_22) <- Sept_22[5, ]
Sept_22 <- Sept_22[-(1:5),]
Sept_22$Month <- "September"
Sept_22$Year <- "2022"

Oct_22 <- read_excel("ComorbidityRegister_2022_October.xlsx")
colnames(Oct_22) <- Oct_22[5, ]
Oct_22 <- Oct_22[-(1:5),]
Oct_22$Month <- "October"
Oct_22$Year <- "2022"

Nov_22 <- read_excel("ComorbidityRegister_2022_November.xlsx")
colnames(Nov_22) <- Nov_22[5, ]
Nov_22 <- Nov_22[-(1:5),]
Nov_22$Month <- "November"
Nov_22$Year <- "2022"

Dec_22 <- read_excel("ComorbidityRegister_2022_December.xlsx")
colnames(Dec_22) <- Dec_22[5, ]
Dec_22 <- Dec_22[-(1:5),]
Dec_22$Month <- "December"
Dec_22$Year <- "2022"

#2023

Jan_23 <- read_excel("ComorbidityRegister_2023_January.xlsx")
colnames(Jan_23) <- Jan_23[5, ]
Jan_23 <- Jan_23[-(1:5),]
Jan_23$Month <- "January"
Jan_23$Year <- "2023"

Feb_23 <- read_excel("ComorbidityRegister_2023_February.xlsx")###
colnames(Feb_23) <- Feb_23[5, ]
Feb_23 <- Feb_23[-(1:5),]
Feb_23$Month <- "February"
Feb_23$Year <- "2023"

Mar_23 <- read_excel("ComorbidityRegister_2023_March.xlsx")###
colnames(Mar_23) <- Mar_23[5, ]
Mar_23 <- Mar_23[-(1:5),]
Mar_23$Month <- "March"
Mar_23$Year <- "2023"

Apr_23 <- read_excel("ComorbidityRegister_2023_April.xlsx")###
colnames(Apr_23) <- Apr_23[5, ]
Apr_23 <- Apr_23[-(1:5),]
Apr_23$Month <- "April"
Apr_23$Year <- "2023"


May_23 <- read_excel("ComorbidityRegister_2023_May.xlsx")###
colnames(May_23) <- May_23[5, ]
May_23 <- May_23[-(1:5),]
May_23$Month <- "May"
May_23$Year <- "2023"

#Jun_23 <- read_excel("ComorbidityRegister_2023_June.xlsx")
#colnames(Jun_23) <- Jun_23[5, ]
#Jun_23 <- Jun_23[-(1:5),]
#Jun_23$Month <- "June"
#Jun_23$Year <- "2023"

#Jul_23 <- read_excel("ComorbidityRegister_2023_July.xlsx")
#colnames(Jul_23) <- Jul_23[5, ]
#Jul_23 <- Jul_23[-(1:5),]
#Jul_23$Month <- "July"
#Jul_23$Year <- "2023"

Aug_23 <- read_excel("ComorbidityRegister_2023_August.xlsx")
colnames(Aug_23) <- Aug_23[5, ]
Aug_23 <- Aug_23[-(1:5),]
Aug_23$Month <- "August"
Aug_23$Year <- "2023"

Sept_23 <- read_excel("ComorbidityRegister_2023_September.xlsx")###
colnames(Sept_23) <- Sept_23[5, ]
Sept_23 <- Sept_23[-(1:5),]
Sept_23$Month <- "September"
Sept_23$Year <- "2023"

Oct_23 <- read_excel("ComorbidityRegister_2023_October.xlsx")
colnames(Oct_23) <- Oct_23[5, ]
Oct_23 <- Oct_23[-(1:5),]
Oct_23$Month <- "October"
Oct_23$Year <- "2023"

Nov_23 <- read_excel("ComorbidityRegister_2023_November.xlsx")###
colnames(Nov_23) <- Nov_23[5, ]
Nov_23 <- Nov_23[-(1:5),]
Nov_23$Month <- "November"
Nov_23$Year <- "2023"

Dec_23 <- read_excel("ComorbidityRegister_2023_December.xlsx")
colnames(Dec_23) <- Dec_23[5, ]
Dec_23 <- Dec_23[-(1:5),]
Dec_23$Month <- "December"
Dec_23$Year <- "2023"

#2024

Jan_24 <- read_excel("ComorbidityRegister_2024_January.xlsx")###
colnames(Jan_24) <- Jan_24[5, ]
Jan_24 <- Jan_24[-(1:5),]
Jan_24$Month <- "January"
Jan_24$Year <- "2024"

Feb_24 <- read_excel("ComorbidityRegister_2024_February.xlsx")
colnames(Feb_24) <- Feb_24[5, ]
Feb_24 <- Feb_24[-(1:5),]
Feb_24$Month <- "February"
Feb_24$Year <- "2024"

Mar_24 <- read_excel("ComorbidityRegister_2024_March.xlsx")
colnames(Mar_24) <- Mar_24[5, ]
Mar_24 <- Mar_24[-(1:5),]
Mar_24$Month <- "March"
Mar_24$Year <- "2024"

Apr_24 <- read_excel("ComorbidityRegister_2024_April.xlsx")
colnames(Apr_24) <- Apr_24[5, ]
Apr_24 <- Apr_24[-(1:5),]
Apr_24$Month <- "April"
Apr_24$Year <- "2024"


May_24 <- read_excel("ComorbidityRegister_2024_May.xlsx")
colnames(May_24) <- May_24[5, ]
May_24 <- May_24[-(1:5),]
May_24$Month <- "May"
May_24$Year <- "2024"

Jun_24 <- read_excel("ComorbidityRegister_2024_June.xlsx")
colnames(Jun_24) <- Jun_24[5, ]
Jun_24 <- Jun_24[-(1:5),]
Jun_24$Month <- "June"
Jun_24$Year <- "2024"

Jul_24 <- read_excel("ComorbidityRegister_2024_july.xlsx")
colnames(Jul_24) <- Jul_24[5, ]
Jul_24 <- Jul_24[-(1:5),]
Jul_24$Month <- "July"
Jul_24$Year <- "2024"

Aug_24 <- read_excel("ComorbidityRegister_2024_august.xlsx")
colnames(Aug_24) <- Aug_24[5, ]
Aug_24 <- Aug_24[-(1:5),]
Aug_24$Month <- "August"
Aug_24$Year <- "2024"

Sept_24 <- read_excel("ComorbidityRegister_2024_September.xlsx")
colnames(Sept_24) <- Sept_24[5, ]
Sept_24 <- Sept_24[-(1:5),]
Sept_24$Month <- "September"
Sept_24$Year <- "2024"



















Oct_23 <- read_excel("ComorbidityRegister_2023_October.xlsx")
colnames(Oct_23) <- Oct_23[5, ]
Oct_23 <- Oct_23[-(1:5),]
Oct_23$Month <- "October"
Oct_23$Year <- "2023"

Nov_23 <- read_excel("ComorbidityRegister_2023_Novembar.xlsx")
colnames(Nov_23) <- Nov_23[5, ]
Nov_23 <- Nov_23[-(1:5),]
Nov_23$Month <- "November"
Nov_23$Year <- "2023"

Dec_23 <- read_excel("ComorbidityRegister_2023Decembar.xlsx")
colnames(Dec_23) <- Dec_23[5, ]
Dec_23 <- Dec_23[-(1:5),]
Dec_23$Month <- "December"
Dec_23$Year <- "2023"



merged_data <- rbind(Jan_21,Feb_21,Mar_21,Apr_21,May_21,Jun_21,Jul_21,Aug_21,Sept_21,Oct_21,Nov_21,Dec_21,
                     Jan_22,Feb_22,Mar_22,Apr_22,May_22,Jun_22,Jul_22,Aug_22,Sept_22,Oct_22,Nov_22,Dec_22,
                     Jan_23,Feb_23,Mar_23,Apr_23,May_23,Aug_23,Sept_23,Oct_23,Nov_23,Dec_23,
                     Jan_24,Feb_24,Mar_24,Apr_24,May_24,Jun_24,Jul_24,Aug_24,Sept_24)
                     

write.csv(merged_data,"merged_Cormobidity_Notification_data.csv")
