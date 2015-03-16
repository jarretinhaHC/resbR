
region_name <- 'Brasil'
# Create folder to store results
focalDir <- file.path(paste(resultsDir, region_name, sep='/'))
dir.create(focalDir)

# Basic setup
brks <- seq(0, 1000000, 1)

vcounts_list <- list()
pcounts_list <- list()
# Contracts empirical distribution per place
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_CNES'), with=F]

counts <- tmp[, {v <- tapply(CO_CPF, CO_CNES, length);
              list(ESTABELECIMENTO=names(v),
                   VÍNCULOS=as.vector(v))},
              by=NU_COMPETENCIA]

counts[, bins := findInterval(counts$VÍNCULOS, brks)]
counts <- counts[, list(.N), by=c('NU_COMPETENCIA', 'bins')]
setorder(counts, NU_COMPETENCIA, bins, -N)

counts <- counts[, S := sum(N), by=NU_COMPETENCIA]
counts <- counts[, "F" := N / S ]
counts <- counts[, S := NULL]

vcounts_list[['place']] <- counts

wb_name <- paste0(region_name, ' - Vínculos por estabelecimento.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

# Professionals empirical distribution per place
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_CNES'), with=F]
tmp <- unique(tmp)

counts <- tmp[, {v <- tapply(CO_CPF, CO_CNES, length);
              list(ESTABELECIMENTO=names(v),
                   PROFISSIONAIS=as.vector(v))},
              by=NU_COMPETENCIA]

counts[, bins := findInterval(counts$PROFISSIONAIS, brks)]
counts <- counts[, list(.N), by=c('NU_COMPETENCIA', 'bins')]
setorder(counts, NU_COMPETENCIA, bins, -N)

counts <- counts[, S := sum(N), by=NU_COMPETENCIA]
counts <- counts[, "F" := N / S]
counts <- counts[, S := NULL]

pcounts_list[['place']] <- counts

wb_name <- paste0(region_name, ' - Profissionais por estabelecimento.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

# Contracts empirical distribution per city 
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]

counts <- tmp[, {v <- tapply(CO_CPF, MUN.NO_MUNICIPIO, length);
              list(MUNICIPIO=names(v),
                   VÍNCULOS=as.vector(v))},
              by=NU_COMPETENCIA]

counts[, bins := findInterval(counts$VÍNCULOS, brks)]
counts <- counts[, list(.N), by=c('NU_COMPETENCIA', 'bins')]
setorder(counts, NU_COMPETENCIA, bins, -N)

counts <- counts[, S := sum(N), by=NU_COMPETENCIA]
counts <- counts[, "F" := N / S ]
counts <- counts[, S := NULL]

vcounts_list[['city']] <- counts

wb_name <- paste0(region_name, ' - Vínculos por município.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

# Professionals empirical distribution per place
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]
tmp <- unique(tmp)

counts <- tmp[, {v <- tapply(CO_CPF, MUN.NO_MUNICIPIO, length);
              list(MUNICÍPIO=names(v),
                   PROFISSIONAIS=as.vector(v))},
              by=NU_COMPETENCIA]

counts[, bins := findInterval(counts$PROFISSIONAIS, brks)]
counts <- counts[, list(.N), by=c('NU_COMPETENCIA', 'bins')]
setorder(counts, NU_COMPETENCIA, bins, -N)

counts <- counts[, S := sum(N), by=NU_COMPETENCIA]
counts <- counts[, "F" := N / S]
counts <- counts[, S := NULL]

pcounts_list[['city']] <- counts

wb_name <- paste0(region_name, ' - Profissionais por município.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

# Contracts empirical distribution per region 
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_CIR'), with=F]

counts <- tmp[, {v <- tapply(CO_CPF, CO_CIR, length);
              list(REGIÃO=names(v),
                   VÍNCULOS=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, S := sum(VÍNCULOS), by=NU_COMPETENCIA]
counts <- counts[, "F" := VÍNCULOS / S ]
counts <- counts[, S := NULL]
setorder(counts, -VÍNCULOS)

vcounts_list[['cir']] <- counts

wb_name <- paste0(region_name, ' - Vínculos por região.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

# Professionals empirical distribution per região
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_CIR'), with=F]
tmp <- unique(tmp)

counts <- tmp[, {v <- tapply(CO_CPF, CO_CIR, length);
              list(REGIÃO=names(v),
                   PROFISSIONAIS=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, S := sum(PROFISSIONAIS), by=NU_COMPETENCIA]
counts <- counts[, "F" := PROFISSIONAIS / S]
counts <- counts[, S := NULL]
setorder(counts, -PROFISSIONAIS)

pcounts_list[['cir']] <- counts

wb_name <- paste0(region_name, ' - Profissionais por região.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

# Contracts empirical distribution per UF 
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'UF.SG_UF'), with=F]

counts <- tmp[, {v <- tapply(CO_CPF, UF.SG_UF, length);
              list(UF=names(v),
                   VÍNCULOS=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, S := sum(VÍNCULOS), by=NU_COMPETENCIA]
counts <- counts[, "F" := VÍNCULOS / S ]
counts <- counts[, S := NULL]
counts <- setorder(counts, -VÍNCULOS)

vcounts_list[['UF']] <- counts

wb_name <- paste0(region_name, ' - Vínculos por UF.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

# Professionals empirical distribution per UF
tmp <- data[, c('NU_COMPETENCIA', 'CO_CPF', 'UF.SG_UF'), with=F]
tmp <- unique(tmp)

counts <- tmp[, {v <- tapply(CO_CPF, UF.SG_UF, length);
              list(UF=names(v),
                   PROFISSIONAIS=as.vector(v))},
              by=NU_COMPETENCIA]

counts <- counts[, S := sum(PROFISSIONAIS), by=NU_COMPETENCIA]
counts <- counts[, "F" := PROFISSIONAIS / S]
counts <- counts[, S := NULL]
setorder(counts, -PROFISSIONAIS)

pcounts_list[['UF']] <- counts

wb_name <- paste0(region_name, ' - Profissionais por UF.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

