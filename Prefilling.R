library(XML)

#####
###
###MAIN PROGRAMME
###
#####

###
#SET GLOBAL VARIABLES
###

path <- "U:/B5_SHARED_DISK/Metadata/Automated procedures/Prefilling/"
template.rel.path <- "Template/"
params.rel.path <- "Parameters/params.csv"
input.rel.path <- "Input/"
output.rel.path <- "Output/"

###
#LOAD PARAMETER FILE
###

params <- read.csv(paste0(path, params.rel.path), header=FALSE, sep = ";", row.names = 1, stringsAsFactors = FALSE)

if(!("Separator" %in% row.names(params))) stop("No row named Separator in parameter file!")

input.sep <- params["Separator", 1]

###
#LOAD TEMPLATE FILE
###

template.list <- list.files(paste0(path,template.rel.path), pattern = ".+(\\.xml)$")

if(length(template.list) != 1) {
  
  stop("There should be exactly one .xml file in the Template folder!")

  } else {
  
    template.name <- template.list[[1]]
  
    if(grepl("^[A-Z]*_[A-Z]*_.{1}_.{2}_[0-9]{4}_[0-9]{4}\\.sdmx\\.xml$", template.name)) {
    
      template.doc <- xmlInternalTreeParse(paste0(path, template.rel.path, template.list[[1]]))
  
      } else{
    
        stop("The name of the template does not follow the expected pattern!")
    }
}

###
#LOAD INPUT FILE
###

input.list <- list.files(paste0(path,input.rel.path), pattern = ".+(\\.csv)$")

if(length(input.list) != 1) {
  
  stop("There should be exactly one .csv file in the Input folder!")

  } else {
  
    input <- read.csv(paste0(path, input.rel.path, input.list[[1]]), header=FALSE, sep = input.sep, row.names = 1) 
}

if(!("Country" %in% row.names(input))) stop("No row named Country in input file!")
if(!("Organisation" %in% row.names(input))) stop("No row named Organisation in input file!")
if(!("Year" %in% row.names(input))) stop("No row named Year in input file!")
      
input.concept.names <- row.names(input)[!(row.names(input) %in% c("Country", "Year", "Organisation"))]

template.concept.names <- xpathSApply(template.doc, "//genericmetadata:ReportedAttribute", xmlGetAttr, "conceptID")

if(sum(input.concept.names %in% template.concept.names) != length(input.concept.names)) stop(paste0("The following concepts in the input file do not exist in the template!\n", paste(input.concept.names[!(input.concept.names %in% template.concept.names)], collapse = "\n")))

###
#PREFILL TEMPLATE
###

for (i in 1:ncol(input)) {
  
  prefilled.file <- xmlInternalTreeParse(paste0(path, template.rel.path, template.list[[1]]))
  prefilled.file.title <- template.name
  
  for(j in 1:nrow(input)){
    
    if(row.names(input)[j] == "Country"){
      
      if(grepl("^[A-Z0-9]{2}$", input[j,i])) {
        
        country.pos <- gregexpr("_", prefilled.file.title)[[1]][3]
        prefilled.file.title <- paste0(substr(prefilled.file.title, 1, country.pos), input[j,i], substr(prefilled.file.title, country.pos+3, nchar(prefilled.file.title)))
      
        } else {
        
          stop(paste0("Country code has wrong pattern in column ", i))
      
        }
      
    } else if(row.names(input)[j] == "Organisation"){
      
      xpathSApply(prefilled.file, "//genericmetadata:ComponentValue[@component='DATA_PROVIDER']", `xmlValue<-`, value = input[j,i])
      xpathSApply(prefilled.file, "//message:Sender", namespaces = c(message = "http://www.SDMX.org/resources/SDMXML/schemas/v2_0/message"), fun = `xmlAttrs<-`, value = c(id = paste0(input[j,i])))
      
    } else if(row.names(input)[j] == "Year"){

        if(grepl("^[0-9]{4}$", input[j,i])) {
        
          year.pos <- gregexpr("_", prefilled.file.title)[[1]][4]
		      freq.pos <- gregexpr("_", prefilled.file.title)[[1]][2]
          prefilled.file.title <- paste0(substr(prefilled.file.title, 1, year.pos), input[j,i], substr(prefilled.file.title, year.pos+5, nchar(prefilled.file.title)))
		  
			
          xpathSApply(prefilled.file, "//genericmetadata:ComponentValue[@component='TIME_PERIOD']", `xmlValue<-`, value = paste0(input[j,i], "-", substr(prefilled.file.title, freq.pos+1, freq.pos+1), "0"))
                    
        } else {
      
          stop(paste0("Year has wrong pattern in column ", i))
      
        }
      
    } else{
      
      xpathSApply(prefilled.file, paste0("//genericmetadata:ReportedAttribute[@conceptID='", row.names(input)[j], "']/genericmetadata:Value"), `xmlValue<-`, value = input[j,i])
      
    }
    
    
  }
  
  xpathSApply(prefilled.file, "//message:Name", namespaces = c(message = "http://www.SDMX.org/resources/SDMXML/schemas/v2_0/message"), fun = `xmlValue<-`, value = gsub(".sdmx.xml", "", prefilled.file.title))
  
  prefilled.file.text <- toString.XMLNode(prefilled.file)
  prefilled.file.text <- gsub("<Value>", "<genericmetadata:Value>", prefilled.file.text)
  prefilled.file.text <- gsub("</Value>", "</genericmetadata:Value>", prefilled.file.text)
  
  cat(prefilled.file.text, file=paste0(path,output.rel.path,prefilled.file.title))
  
}