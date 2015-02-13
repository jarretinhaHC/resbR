
# Filter, map, rename, clean data 


# Selected to be used in filters
selected_regions <- regions[which(regions$CO_CIR %in% selected$CO_CIR), ]

# Filter physicians
data <- with(raw_data, raw_data[which(CO_MUNICIPIO_GESTOR
                               %in% selected_regions$CO_MUNICIPIO
                               & grepl('^MÉDICO ', DS_CBO_OCUPACAO)
                               & DS_CBO_OCUPACAO != 'MÉDICO VETERINÁRIO')])

data$DS_CBO_OCUPACAO <- to_from_CBO[data$DS_CBO_OCUPACAO, ]

