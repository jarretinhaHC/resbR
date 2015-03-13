
### Basic barcharts  ###

### Focus on professionals
competences <-  as.character(unique(data$NU_COMPETENCIA))

for(cir in unique(selected_regions$CO_CIR)){

    # Isolate focal health region
    region <- with(selected_regions, selected_regions[which(CO_CIR == cir), ])
    region_name <- unique(region$NM_CIR)

    # Create folder to store results
    focalDir <- file.path(paste(resultsDir, region_name, sep='/'))
    dir.create(focalDir)

    #Filter things by region
    fdata <- with(data, data[which(CO_CIR == cir)])
 

    # Filter things by CPF
    # CPF with touch smth outside focal region per competence
    cpf_out <- unique(data[which(CO_CIR != cir), c('NU_COMPETENCIA', 'CO_CPF'),
                      with=F])
    cpf_out <- split(cpf_out, cpf_out$NU_COMPETENCIA)
    # CPF wich are restriced to the focal region per competence
    cpf_ins <- unique(fdata[, c('NU_COMPETENCIA', 'CO_CPF'), with=F])
    cpf_ins <- split(cpf_ins, cpf_ins$NU_COMPETENCIA)

    cpf_exc <-list()
    for(c in competences){

        cpf_exc[[c]] <- filter(cpf_ins[[c]], !cpf_ins[[c]]$CO_CPF %in%
                               cpf_out[[c]]$CO_CPF)

    }

    # Bar chart of contracts per professional per competence
    # Select focal columns
    tmp <- data[, c('CO_CNES', 'NU_COMPETENCIA', 'CO_CPF'), with=F]
    tmp_all <- tmp[which(tmp$CO_CPF %in% fdata$CO_CPF)]
    tmp_ins <-  
    counts <- tmp[, {v <- charter(CO_CNES, CO_CPF);
                  list(VÍNCULOS=as.integer(names(v)),
                       CONTAGEM=as.vector(v))},
                  by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
    counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
    counts <- counts[, SOMA := NULL]

    wb_name <- paste0(region_name, ' - Seção 01 - Vínculos por profissional.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }
    saveWorkbook(wb)

    # Bar chart of places per professional per competence
    # Select focal columns
    tmp <- fdata[, c('CO_CNES', 'NU_COMPETENCIA', 'CO_CPF'), with=F]

    # Professional based description must use unique    
    tmp <- unique(tmp)
    counts <- tmp[, {v <- charter(CO_CNES, CO_CPF);
               list(ESTABELECIMENTOS=as.integer(names(v)),
                    CONTAGEM=as.vector(v))},
               by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
    counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
    counts <- counts[, SOMA := NULL]

    wb_name <- paste0(region_name, ' - Seção 01 - Estabelecimentos por profissional.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }
    saveWorkbook(wb)

    # Cities per professional per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]
    tmp <- unique(tmp)

    counts <- tmp[, {v <- charter(MUN.NO_MUNICIPIO, CO_CPF);
                  list(MUNICÍPIOS=names(v),
                       CONTAGEM=as.vector(v))},
                  by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
    counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
    counts <- counts[, SOMA := NULL]

    wb_name <- paste0(region_name, ' - Seção 01 - Municípios por profissional.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

    # Occupations per professional per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'DS_CBO_OCUPACAO'), with=F]
    tmp <- unique(tmp)

    counts <- tmp[, {v <- charter(DS_CBO_OCUPACAO, CO_CPF);
                  list(OCUPAÇÕES=names(v),
                       CONTAGEM=as.vector(v))},
                  by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(CONTAGEM), by=NU_COMPETENCIA]
    counts <- counts[, PERCENTUAL := sprintf('%2.3f', CONTAGEM/SOMA * 100)]
    counts <- counts[, SOMA := NULL]

    wb_name <- paste0(region_name, ' - Seção 01 - Ocupações por profissional.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

}

