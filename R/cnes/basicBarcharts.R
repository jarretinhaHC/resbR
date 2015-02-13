
### Basic barcharts  ###

### Focus on professionals
for(cir in unique(selected_regions$CO_CIR)){

    # Isolate focal health region
    region <- with(selected_regions, selected_regions[which(CO_CIR == cir), ])
    region_name <- unique(region$NM_CIR)

    # Create folder to store results
    focalDir <- file.path(paste(resultsDir, region_name, sep='/'))
    dir.create(focalDir)

    #Filter things by region
    fdata  <- with(data, data[which(CO_MUNICIPIO_GESTOR %in% region$CO_MUNICIPIO)])

    # Bar chart of contracts per professional per competence
    # Select focal columns
    tmp <- fdata[, c('CO_CNES', 'NU_COMPETENCIA', 'CO_CPF'), with=F]
    counts <- tmp[, {v <- charter(CO_CNES, CO_CPF);
                list(VÍNCULOS=as.integer(names(v)),
                     CONTAGEM=as.vector(v))},
                by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
    counts <- counts[, PERCENTUAL := CONTAGEM/SOMA]
    counts <- counts[, SOMA := NULL]

    wb_name <- paste0(region_name, ' - Seção 01 - Vínculos por profissional.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }
    saveWorkbook(wb)
    rm(tmp, counts)
    gc()

    # Bar chart of places per professional per competence
    # Select focal columns
    tmp <- fdata[, c('CO_CNES', 'NU_COMPETENCIA', 'CO_CPF'), with=F]

    # Professional based description must use unique    
    tmp <- unique(tmp)
    counts <- [, {v <- charter(CO_CNES, CO_CPF);
                list(ESTABELECIMENTOS=as.integer(names(v)),
                     CONTAGEM=as.vector(v))},
                by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
    counts <- counts[, PERCENTUAL := CONTAGEM/SOMA]
    counts <- counts[, SOMA := NULL]

    wb_name <- paste0(region_name, ' - Seção 01 - Estabelecimentos por profissional.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }
    saveWorkbook(wb)

    # Bar chart, cities per professional per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]
    tmp <- unique(tmp)

    counts <- tmp[, {v <- charter(MUN.NO_MUNICIPIO, CO_CPF);
                  list(MUNICÍPIOS=names(v),
                       CONTAGEM=as.vector(v))},
                  by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
    counts <- counts[, PERCENTUAL := CONTAGEM/SOMA]
    counts <- counts[, SOMA := NULL]

    wb_name <- paste0(region_name, ' - Seção 01 - Municípios por profissional.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)


}

### Focus on cities ### 
for(cir in unique(selected_regions$CO_CIR)){

    # Isolate focal health region
    region <- with(selected_regions, selected_regions[which(CO_CIR == cir), ])
    region_name <- unique(region$NM_CIR)

    # Create folder to store results
    focalDir <- file.path(paste(resultsDir, region_name, sep='/'))
    dir.create(focalDir)

    #Filter things by region
    fdata  <- with(data, data[which(CO_MUNICIPIO_GESTOR %in% region$CO_MUNICIPIO)])

    # Contracts per city per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]

    counts <- tmp[, {v <- charter(MUN.NO_MUNICIPIO, CO_CPF);
                  list(MUNICÍPIOS=names(v),
                       VÍNCULOS=as.vector(v))},
                  by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(VÍNCULOS), by=NU_COMPETENCIA]
    counts <- counts[, "VÍNCULOS (%)" := VÍNCULOS/SOMA]
    counts <- counts[, SOMA := NULL]

    # Professionals per city per competence
    tmp <- unique(tmp)
    tmp <- tmp[, {v <- charter(MUN.NO_MUNICIPIO, CO_CPF);
               list(MUNICÍPIOS=names(v),
                    PROFISSIONAIS=as.vector(v))},
               by=NU_COMPETENCIA]

    tmp <- tmp[, SOMA := sum(PROFISSIONAIS), by=NU_COMPETENCIA]
    tmp <- tmp[, "PROFISSIONAIS (%)" := PROFISSIONAIS/SOMA]
    tmp <- tmp[, SOMA := NULL]

    counts$PROFISSIONAIS <- tmp$PROFISSIONAIS
    counts$"PROFISSIONAIS (%)" <- tmp$"PROFISSIONAIS (%)"

    # Places per city per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CNES', 'MUN.NO_MUNICIPIO'), with=F]
    tmp <- unique(tmp)

    tmp <- tmp[, {v <- tapply(CO_CNES, MUN.NO_MUNICIPIO, length);
               list(MUNICÍPIOS=names(v),
                    ESTABELECIMENTOS=as.vector(v))},
               by=NU_COMPETENCIA]

    tmp <- tmp[, SOMA := sum(ESTABELECIMENTOS), by=NU_COMPETENCIA]
    tmp <- tmp[, "ESTABELECIMENTOS (%)" := ESTABELECIMENTOS/SOMA]
    tmp <- tmp[, SOMA := NULL]

    counts$ESTABELECIMENTOS <- tmp$ESTABELECIMENTOS
    counts$"ESTABELECIMENTOS (%)" <- tmp$"ESTABELECIMENTOS (%)"

    wb_name <- paste0(region_name, ' - Seção 01 - Contagens básicas por município.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

}
