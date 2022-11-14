library(tidyverse)
library(readxl)


## loading in the documentation file
documentation <- read_xlsx("/Users/michaeltopper/Desktop/Heather RA/documentation/Final Documentation by year.xlsx")

## cleaning the names to be lowercase 
documentation <- janitor::clean_names(documentation)

## years
years <- c(2002:2018)

## relocating the variables so that I can loop through them easier.
documentation <- documentation %>% 
  relocate(x2002, x2003, x2004, x2005, x2006, x2007, x2008, x2009, x2010, x2011, x2012, x2013, x2014, x2015, x2016,
           x2017, x2018, stata_name)

## finding the index of the stata_name column
stata_name <- which(colnames(documentation) == "stata_name")
label <- which(colnames(documentation) == "covariate")

## finding the index of the alpha_numeric column so I can extract values inside of the for loop.
column_type <- which(colnames(documentation) == "alpha_numeric")



## file paths for the Z: drive:
raw_birth_path_2002to2003 <- "Z:\\Raw_data\\births\\BirthCohort2002-2003\\BirthCohort"
raw_birth_path_2004to2006 <-  "Z:\\Raw_data\\births\\BirthCohort2004-2006\\BirthCohort"
raw_birth_path_2007to2012 <- "Z:\\Raw_data\\births\\BirthCohort2007-2012_Birth2013\\Data Files\\BirthCohort"
raw_birth_path_2013to2018 <- "Z:\\Raw_data\\births\\BirthCohort2013-2018\\Royer_BC_" ##careful - these are .dat files not .txt!


export_path <- "Z:\\Created_Data\\birth_data\\births_by_year\\"
export_csv_path <- "X:\\births_raw_csv\\"

## file path for saving in my local drive:
local_path <- "/Users/michaeltopper/Desktop/Heather RA/documentation/txt_for_raw_data/stata_code"

### for 2002 - 2003
## The i index takes care of the column index. Hence why I relocated every year to the front.
## Thus, i = 1 is 2002, i = 2 is 2003, etc. 
for (i in 1:2) {
  file_path <- paste("/Users/michaeltopper/Desktop/Heather RA/documentation/txt_for_raw_data/stata_code", years[i], ".txt", sep = "")
  write_lines("clear", file = file_path, sep = "\n", append = F)
  write_lines("\n", file = file_path, append = T)
  for (j in 1:length(documentation$stata_name)) {
    if (documentation[[j, column_type]] == "A" & j == 1 ){
      write_lines(paste("infix str", documentation[[j,stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] != "A" & j == 1 ){
      write_lines(paste("infix ", documentation[[j, stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] == "A" & j != 1 & documentation[[j,i]] != "NA") {
      write_lines(paste("       str", documentation[[j, stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] != "A" & j != 1 & documentation[[j,i]] != "NA"){
      write_lines(paste("      ",documentation[[j, stata_name]], documentation[[j,i]], "///"), file = file_path, sep = "\n", append = T)
    }
  }
  write_lines(paste("      ", "using \"", raw_birth_path_2002to2003, years[i], ".txt\"", sep = ""), file = file_path, sep = "\n", append = T)
  write_lines("\n", file = file_path, append = T)
  for (j in 1: length(documentation$stata_name)){
    if (documentation[[j,i]] != "NA") {
      write_lines(paste("label variable ", documentation[[j, stata_name]], " \"", documentation[[j, label]], "\"", sep = ""), file = file_path, sep = "\n", append = T)
    }
  }
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("gen year = ", years[i]), file = file_path, append = T)
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("export delimited using \"", export_csv_path, "birth_data_", years[i], ".csv\"", ", replace", sep = "" ), file = file_path, append = T)
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("save \"", export_path, "birth_data_", years[i], "\"", ", replace", sep = ""), file = file_path, append = T)
}




### for 2004 - 2006
## The i index takes care of the column index. Hence why I relocated every year to the front.
## Thus, i = 1 is 2002, i = 2 is 2003, etc. 
for (i in 3:5) {
  file_path <- paste("/Users/michaeltopper/Desktop/Heather RA/documentation/txt_for_raw_data/stata_code", years[i], ".txt", sep = "")
  write_lines("clear", file = file_path, sep = "\n", append = F)
  write_lines("\n", file = file_path, append = T)
  for (j in 1:length(documentation$stata_name)) {
    if (documentation[[j, column_type]] == "A" & j == 1 ){
      write_lines(paste("infix str", documentation[[j,stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] != "A" & j == 1 ){
      write_lines(paste("infix ", documentation[[j, stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] == "A" & j != 1 & documentation[[j,i]] != "NA") {
      write_lines(paste("       str", documentation[[j, stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] != "A" & j != 1 & documentation[[j,i]] != "NA"){
      write_lines(paste("      ",documentation[[j, stata_name]], documentation[[j,i]], "///"), file = file_path, sep = "\n", append = T)
    }
  }
  write_lines(paste("      ", "using \"", raw_birth_path_2004to2006, years[i], ".txt\"", sep = ""), file = file_path, sep = "\n", append = T)
  write_lines("\n", file = file_path, append = T)
  for (j in 1: length(documentation$stata_name)){
    if (documentation[[j,i]] != "NA") {
      write_lines(paste("label variable ", documentation[[j, stata_name]], " \"", documentation[[j, label]], "\"", sep = ""), file = file_path, sep = "\n", append = T)
    }
  }
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("gen year = ", years[i]), file = file_path, append = T)
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("export delimited using \"", export_csv_path, "birth_data_", years[i], ".csv\"", ", replace", sep = "" ), file = file_path, append = T)
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("save \"", export_path, "birth_data_", years[i], "\"", ", replace", sep = ""), file = file_path, append = T)
}



### for 2007 - 2012
for (i in 6:11) {
  file_path <- paste("/Users/michaeltopper/Desktop/Heather RA/documentation/txt_for_raw_data/stata_code", years[i], ".txt", sep = "")
  write_lines("clear", file = file_path, sep = "\n", append = F)
  write_lines("\n", file = file_path, append = T)
  for (j in 1:length(documentation$stata_name)) {
    if (documentation[[j, column_type]] == "A" & j == 1 ){
      write_lines(paste("infix str", documentation[[j,stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] != "A" & j == 1 ){
      write_lines(paste("infix ", documentation[[j, stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] == "A" & j != 1 & documentation[[j,i]] != "NA") {
      write_lines(paste("       str", documentation[[j, stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] != "A" & j != 1 & documentation[[j,i]] != "NA"){
      write_lines(paste("      ",documentation[[j, stata_name]], documentation[[j,i]], "///"), file = file_path, sep = "\n", append = T)
    }
  }
  write_lines(paste("      ", "using \"", raw_birth_path_2007to2012, years[i], ".txt\"", sep = ""), file = file_path, sep = "\n", append = T)
  write_lines("\n", file = file_path, append = T)
  for (j in 1: length(documentation$stata_name)){
    if (documentation[[j,i]] != "NA") {
      write_lines(paste("label variable ", documentation[[j, stata_name]], " \"", documentation[[j, label]], "\"", sep = ""), file = file_path, sep = "\n", append = T)
    }
  }
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("gen year = ", years[i]), file = file_path, append = T)
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("export delimited using \"", export_csv_path, "birth_data_", years[i], ".csv\"", ", replace", sep = "" ), file = file_path, append = T)
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("save \"", export_path, "birth_data_", years[i], "\"", ", replace", sep = ""), file = file_path, append = T)
}

### For yeasr 2013 - 2018
for (i in 12:length(years)) {
  file_path <- paste("/Users/michaeltopper/Desktop/Heather RA/documentation/txt_for_raw_data/stata_code", years[i], ".txt", sep = "")
  write_lines("clear", file = file_path, sep = "\n", append = F)
  write_lines("\n", file = file_path, append = T)
  for (j in 1:length(documentation$stata_name)) {
    if (documentation[[j, column_type]] == "A" & j == 1 ){
      write_lines(paste("infix str", documentation[[j,stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] != "A" & j == 1 ){
      write_lines(paste("infix ", documentation[[j, stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] == "A" & j != 1 & documentation[[j,i]] != "NA") {
      write_lines(paste("       str", documentation[[j, stata_name]], documentation[[j,i]],  "///"), file = file_path,  sep = "\n", append = T)
    }
    if (documentation[[j, column_type]] != "A" & j != 1 & documentation[[j,i]] != "NA"){
      write_lines(paste("      ",documentation[[j, stata_name]], documentation[[j,i]], "///"), file = file_path, sep = "\n", append = T)
    }
  }
  write_lines(paste("      ", "using \"", raw_birth_path_2013to2018, years[i], ".dat\"", sep = ""), file = file_path, sep = "\n", append = T)
  write_lines("\n", file = file_path, append = T)
  for (j in 1: length(documentation$stata_name)){
    if (documentation[[j,i]] != "NA") {
      write_lines(paste("label variable ", documentation[[j, stata_name]], " \"", documentation[[j, label]], "\"", sep = ""), file = file_path, sep = "\n", append = T)
    }
  }
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("gen year = ", years[i]), file = file_path, append = T)
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("export delimited using \"", export_csv_path, "birth_data_", years[i], ".csv\"", ", replace", sep = "" ), file = file_path, append = T)
  write_lines("\n", file = file_path, append = T)
  write_lines(paste("save \"", export_path, "birth_data_", years[i], "\"", ", replace", sep = ""), file = file_path, append = T)
}




### below this I have done it for 1 year only. This is good for reference since looking at a huge loop is pretty daunting
# ## writing the file
# file_path <- "/Users/michaeltopper/Desktop/Heather RA/documentation/stata_code.txt"
# write_lines("clear", file = file_path, sep = "\n", append = F)
# write_lines("\n", file = file_path, append = T)
# for (i in 1:length(documentation$stata_name)) {
#   if (documentation$alpha_numeric[i] == "A" & i == 1 ){
#     write_lines(paste("infix str", documentation$stata_name[i], documentation$x2012[i],  "///"), file = file_path,  sep = "\n", append = T)
#   }
#   if (documentation$alpha_numeric[i] != "A" & i == 1 ){
#     write_lines(paste("infix ", documentation$stata_name[i], documentation$x2012[i],  "///"), file = file_path,  sep = "\n", append = T)
#   }
#   if (documentation$alpha_numeric[i] == "A" & i != 1) {
#      write_lines(paste("       str", documentation$stata_name[i], documentation$x2012[i],  "///"), file = file_path,  sep = "\n", append = T)
#     }
#   if (documentation$alpha_numeric[i] != "A" & i != 1){
#     write_lines(paste("      ",documentation$stata_name[i], documentation$x2012[i], "///"), file = file_path, sep = "\n", append = T)
#   }
# }
# write_lines(paste("      ", "using ", file_path, sep = ""), file = file_path, sep = "\n", append = T)
# write_lines("\n", file = file_path, append = T)
# write_lines(paste("gen year = ", 2012), file = file_path, append = T)
# write_lines("\n", file = file_path, append = T)
# write_lines(paste("export delimited using", file_path), file = file_path, append = T)




