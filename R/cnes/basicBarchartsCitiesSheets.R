
### Basic barcharts ###

### Focus on cities 

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

    counts <- tmp[, {v <- tapply(CO_CPF, MUN.NO_MUNICIPIO, length);
                  list(MUNICÍPIOS=names(v),
                       VÍNCULOS=as.vector(v))},
                  by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(VÍNCULOS), by=NU_COMPETENCIA]
    counts <- counts[, "VÍNCULOS (%)" := sprintf('%2.3f', VÍNCULOS/SOMA * 100)]
    counts <- counts[, SOMA := NULL]

    # Professionals per city per competence
    tmp <- unique(tmp)
    tmp <- tmp[, {v <- tapply(CO_CPF, MUN.NO_MUNICIPIO, length);
               list(MUNICÍPIOS=names(v),
                    PROFISSIONAIS=as.vector(v))},
               by=NU_COMPETENCIA]

    tmp <- tmp[, SOMA := sum(PROFISSIONAIS), by=NU_COMPETENCIA]
    tmp <- tmp[, "PROFISSIONAIS (%)" := sprintf('%2.3f', PROFISSIONAIS/SOMA *
                                                100)]
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
    tmp <- tmp[, "ESTABELECIMENTOS (%)" := sprintf('%2.3f',
                                                   ESTABELECIMENTOS/SOMA * 100)]
    tmp <- tmp[, SOMA := NULL]

    counts$ESTABELECIMENTOS <- tmp$ESTABELECIMENTOS
    counts$"ESTABELECIMENTOS (%)" <- tmp$"ESTABELECIMENTOS (%)"

    counts$VP <- with(counts, sprintf('%2.2f', VÍNCULOS/PROFISSIONAIS))
    counts$VE <- with(counts, sprintf('%2.2f', VÍNCULOS/ESTABELECIMENTOS))
    counts$PE <- with(counts, sprintf('%2.2f', PROFISSIONAIS/ESTABELECIMENTOS))
    counts$PH <- with(counts, sprintf('%2.2f', PROFISSIONAIS/region$POPULACAO * 1000))
    counts$VH <- with(counts, sprintf('%2.2f', VÍNCULOS/region$POPULACAO * 1000))
    counts$EH <- with(counts, sprintf('%2.2f', ESTABELECIMENTOS/region$POPULACAO * 1000))

    wb_name <- paste0(region_name, ' - Contagens básicas por município.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

    ### Beware!!! Complicated manipulations below
    # Bar panel, cities per professional per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_MUNICIPIO_GESTOR'), with=F]
    tmp <- unique(tmp)

    # Clever table with lists of cities per professional
    tmp <- split(tmp, tmp$NU_COMPETENCIA)
    for(s in names(tmp)){

        tmp[[s]] <- tmp[[s]][, {
            v <- tapply(tmp[[s]]$CO_MUNICIPIO_GESTOR,
                        tmp[[s]]$CO_CPF,
                        function(x) unique(c(x)));
        list(NU_COMPETENCIA=s,
             CO_CPF=names(v),
             S=sapply(v, c),
             L=sapply(v, length))},]

    }

    tmp <- rbindlist(tmp)

    cities <- unique(as.vector(unlist(tmp$S)))
    cities <- as.character(cities[order(cities)])

    # Split by number of cities
    df_list <- list()
    tmp <- split(tmp, tmp$L)
    for(l in tmp){
        # Split by competence
        v <- split(l, l$NU_COMPETENCIA)
        for(d in v){
            df <- data.frame(NM_MUNICIPIO=region[cities, ]$NM_MUNICIPIO,
                             CONTAGEM=0,
                             PERCENTUAL=0,
                             QTD_MUNICIPIOS=unique(d$L),
                             NU_COMPETENCIA=unique(d$NU_COMPETENCIA),
                             GRUPO=unique(region$GRUPO),
                             row.names=cities)

            tbl <- table(unlist(d$S))
            S <- sum(tbl)
            for(s in names(tbl)){

                df[s, ]$CONTAGEM <- tbl[[s]]
                df[s, ]$PERCENTUAL <- tbl[[s]] / S * 100

            }

            df_list[[length(df_list) + 1]] <- df

        }

    }

    counts <- rbindlist(df_list)

    wb_name <- paste0(region_name, ' - Municípios por profissional - Contribuição.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

    ### Beware!!! Complicated manipulations below
    # Bar panel, cities per professional per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_MUNICIPIO_GESTOR'), with=F]

    # Clever table with lists of cities per professional
    tmp <- split(tmp, tmp$NU_COMPETENCIA)
    for(s in names(tmp)){

        tmp[[s]] <- tmp[[s]][, {
            v <- tapply(tmp[[s]]$CO_MUNICIPIO_GESTOR,
                        tmp[[s]]$CO_CPF,
                        c);
        list(NU_COMPETENCIA=s,
             CO_CPF=names(v),
             S=sapply(v, c),
             L=sapply(v, length))},]

    }

    tmp <- rbindlist(tmp)

    cities <- unique(as.vector(unlist(tmp$S)))
    cities <- as.character(cities[order(cities)])

    # Split by number of cities
    df_list <- list()
    tmp <- split(tmp, tmp$L)
    for(l in tmp){
        # Split by competence
        v <- split(l, l$NU_COMPETENCIA)
        for(d in v){
            df <- data.frame(NM_MUNICIPIO=region[cities, ]$NM_MUNICIPIO,
                             CONTAGEM=0,
                             PERCENTUAL=0,
                             QTD_VINCULOS=unique(d$L),
                             NU_COMPETENCIA=unique(d$NU_COMPETENCIA),
                             GRUPO=unique(region$GRUPO),
                             row.names=cities)

            tbl <- table(unlist(d$S))
            S <- sum(tbl) 
            for(s in names(tbl)){

                df[s, ]$CONTAGEM <- tbl[[s]]
                df[s, ]$PERCENTUAL <- tbl[[s]] / S * 100

            }

            df_list[[length(df_list) + 1]] <- df

        }

    }

    counts <- rbindlist(df_list)

    wb_name <- paste0(region_name, ' - Municípios por vínculo - Contribuição.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

}
