
# Filter, map, rename, clean data 

# Selected to be used in filters
selected_regions <- regions[which(regions$CO_CIR %in% selected$CO_CIR), ]

selected_cirs <- as.character(unique(selected_regions$CO_CIR))

write.csv(selected_regions, file=paste(resultsDir, 'Selecionadas.csv', sep='/'), row.names=F)

data <- with(raw_data, raw_data[which(grepl('^MÉDICO ', DS_CBO_OCUPACAO)
                                      & DS_CBO_OCUPACAO != 'MÉDICO VETERINÁRIO')])

data$DS_CBO_OCUPACAO <- to_from_CBO[data$DS_CBO_OCUPACAO, ]

data$CO_MUNICIPIO_GESTOR <- as.character(data$CO_MUNICIPIO_GESTOR)

data$CO_CIR <- as.character(regions[data$CO_MUNICIPIO_GESTOR, ]$CO_CIR)

competences <- as.character(unique(data$NU_COMPETENCIA))

