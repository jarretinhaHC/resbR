
# Basic setup
brks <- seq(0, 1000000, 1)

for(cir in selected_cirs){

    region <- with(selected_regions, selected_regions[which(CO_CIR == cir), ])
    region_name <- unique(region$NM_CIR)

    # Create folder to store results
    focalDir <- file.path(paste(resultsDir, region_name, sep='/'))
    dir.create(focalDir)

    #Filter things by region
    fdata <- with(data, data[which(CO_CIR == cir)])

    # Contracts empirical distribution per place
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_CNES'), with=F]

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

    wb_name <- paste0(region_name, ' - Vínculos por estabelecimento - escala.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){
    
        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)
    
    }

    saveWorkbook(wb)

    # Professionals empirical distribution per place
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'CO_CNES'), with=F]
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

    wb_name <- paste0(region_name, ' - Profissionais por estabelecimento - escala.xls')
    wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){
    
        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)
    
    }

    saveWorkbook(wb)

}
