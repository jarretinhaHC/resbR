
# Set files and folders

# Main data folder and files
dataDir <- file.path('/health/CNES/NESCON')
files <- Sys.glob(paste(dataDir, 'professionals/psv/*.psv', sep='/'))

# Devel folder
devDir <- file.path('/home/jarretinha/dev/resbR')

# Static data folder, provisory location
refDir <- file.path(paste(devDir, 'static', sep='/'))

# Results folder
resultsDir <- file.path('/health/CNES/NESCON/professionals/results')

# CNES data arrived without headers
# So, we need to add them at some point
# These files were hand made/curated
# Even the data dict came with many errors
header <- read.table(file.path(paste(refDir, 'carga_horaria_header', sep='/')),
                     header=F,
                     sep='\t',
                     as.is=TRUE,
                     stringsAsFactor=FALSE)[[1]]

# Description of all brazilian health regions 
regions <- read.table(file.path(paste(refDir, 'CIR_BR.tsv', sep='/')),
                      sep='\t',
                      header=TRUE,
                      stringsAsFactor=FALSE,
                      as.is=TRUE,
                      quote="")

# Selected health regions for this study
selected <- read.table(file.path(paste(refDir, 'selected_CIR.tsv', sep='/')),
                       sep='\t',
                       header=TRUE,
                       stringsAsFactor=FALSE,
                       as.is=TRUE,
                       quote="")

# Some name mappings

# We need this because excel sheet names are limited to 31 chars
to_from_CBO <- read.table(file.path(paste(refDir,'de_para_CBO.csv', sep='/')),
                          sep=',',
                          header=TRUE,
                          stringsAsFactor=FALSE,
                          row.names='DS_CBO_OCUPACAO',
                          as.is=TRUE,
                          quote="")

raw_data <- bulk_data_loader(files, header)

# Clean up memory
rm(tmp)
gc()


