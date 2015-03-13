
### Basic barcharts  ###

### Focus on professionals

region_name <- 'Brasil'
# Create folder to store results
focalDir <- file.path(paste(resultsDir, region_name, sep='/'))
dir.create(focalDir)

# Select focal columns
tmp <- data[, c('CO_CNES', 'NU_COMPETENCIA', 'CO_CPF'), with=F]
counts <- tmp[, {v <- charter(CO_CNES, CO_CPF);
              list(VÍNCULOS=as.integer(names(v)),
                   CONTAGEM=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
counts <- counts[, SOMA := NULL]

wb_name <- paste0(region_name, ' - Vínculos por profissional.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){

    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)

}

saveWorkbook(wb)

# Bar chart of places per professional per competence
# Select focal columns
tmp <- data[, c('CO_CNES', 'NU_COMPETENCIA', 'CO_CPF'), with=F]

# Professional based description must use unique
tmp <- unique(tmp)
counts <- tmp[, {v <- charter(CO_CNES, CO_CPF);
              list(ESTABELECIMENTOS=as.integer(names(v)),
                   CONTAGEM=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
counts <- counts[, SOMA := NULL]

wb_name <- paste0(region_name, ' - Estabelecimentos por profissional.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){

    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)

}

saveWorkbook(wb)

# Cities per professional per competence
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]
tmp <- unique(tmp)

counts <- tmp[, {v <- charter(MUN.NO_MUNICIPIO, CO_CPF);
              list(MUNICÍPIOS=names(v),
                   CONTAGEM=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
counts <- counts[, SOMA := NULL]

wb_name <- paste0(region_name, ' - Municípios por profissional.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){

    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)

}

saveWorkbook(wb)

# Occupations per professional per competence
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'DS_CBO_OCUPACAO'), with=F]
tmp <- unique(tmp)

counts <- tmp[, {v <- charter(DS_CBO_OCUPACAO, CO_CPF);
              list(OCUPAÇÕES=names(v),
                   CONTAGEM=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
counts <- counts[, SOMA := NULL]

wb_name <- paste0(region_name, ' - Ocupações por profissional.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
}

saveWorkbook(wb)

# CIR per professional per competence
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_CIR'), with=F]
tmp <- unique(tmp)

counts <- tmp[, {v <- charter(CO_CIR, CO_CPF);
              list(REGIÕES=names(v),
                   CONTAGEM=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
counts <- counts[, SOMA := NULL]

wb_name <- paste0(region_name, ' - Regíões por profissional.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
}

saveWorkbook(wb)

