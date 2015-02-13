### Basic barcharts ###

### Focus on  place types

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
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'DS_TIPO_UNIDADE'), with=F]

    counts <- tmp[, {v <- tapply(CO_CPF, DS_TIPO_UNIDADE, length);
                  list(UNIDADE=names(v),
                       VÍNCULOS=as.vector(v))},
                  by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(VÍNCULOS), by=NU_COMPETENCIA]
    counts <- counts[, "VÍNCULOS (%)" := sprintf('%2.3f', VÍNCULOS/SOMA * 100)]
    counts <- counts[, SOMA := NULL]

    # Professionals per city per competence
    tmp <- unique(tmp)
    tmp <- tmp[, {v <- tapply(CO_CPF, DS_TIPO_UNIDADE, length);
               list(UNIDADE=names(v),
                    PROFISSIONAIS=as.vector(v))},
               by=NU_COMPETENCIA]

    tmp <- tmp[, SOMA := sum(PROFISSIONAIS), by=NU_COMPETENCIA]
    tmp <- tmp[, "PROFISSIONAIS (%)" := sprintf('%2.3f', PROFISSIONAIS/SOMA *
                                                100)]
    tmp <- tmp[, SOMA := NULL]

    counts$PROFISSIONAIS <- tmp$PROFISSIONAIS
    counts$"PROFISSIONAIS (%)" <- tmp$"PROFISSIONAIS (%)"

    # Places per city per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CNES', 'DS_TIPO_UNIDADE'), with=F]
    tmp <- unique(tmp)

    tmp <- tmp[, {v <- tapply(CO_CNES, DS_TIPO_UNIDADE, length);
               list(UNIDADE=names(v),
                    ESTABELECIMENTOS=as.vector(v))},
               by=NU_COMPETENCIA]

    tmp <- tmp[, SOMA := sum(ESTABELECIMENTOS), by=NU_COMPETENCIA]
    tmp <- tmp[, "ESTABELECIMENTOS (%)" := sprintf('%2.3f',
                                                   ESTABELECIMENTOS/SOMA * 100)]
    tmp <- tmp[, SOMA := NULL]

    counts$ESTABELECIMENTOS <- tmp$ESTABELECIMENTOS
    counts$"ESTABELECIMENTOS (%)" <- tmp$"ESTABELECIMENTOS (%)"

    counts$VP <- with(counts, sprintf('%2.2f', VÍNCULOS/PROFISSIONAIS))
    counts$VE <- with(counts, sprintf('%2.2f', VÍNCULOS/ESTABELECIMENTOS))
    counts$PE <- with(counts, sprintf('%2.2f', PROFISSIONAIS/ESTABELECIMENTOS))

    wb_name <- paste0(region_name, ' - Seção 01 - Contagens básicas por tipo de unidade.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

}
