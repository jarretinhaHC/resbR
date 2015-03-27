
region_name <- 'Brasil'
# Create folder to store results
focalDir <- file.path(paste(resultsDir, region_name, sep='/'))
dir.create(focalDir)

# Basic setup
brks <- seq(0, 1000000, 1)

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

wb_name <- paste0(region_name, ' - Vínculos por estabelecimento.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)
counts <- do.call('rbind', counts)

# Let's plot this thing
plt <- ggplot()
plt <- plt + geom_point(data=counts, aes(bins, F), color='red', size=I(2), alpha=I(0.4))

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


wb_name <- paste0(region_name, ' - Profissionais por estabelecimento.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)
counts <- do.call('rbind', counts)

# Let's plot this thing
plt <- plt + geom_point(data=counts, aes(bins, F), color='black', size=I(2), alpha=I(0.4))
plt <- plt + scale_x_log10('Tamanho',
                           labels=scientific10,
                           expand=c(0,0))
plt <- plt + scale_y_log10('Frequência relativa',
                           labels=scientific10,
                           breaks=c(c(1, 5) %o% 10^(-5:0)),
                           expand=c(0, 0))

plt <- plt + theme_classic() + theme(panel.margin=unit(2, 'lines')) + facet_wrap(~NU_COMPETENCIA)

pdf_name <- paste(focalDir, paste0(region_name, ' - Escala por estabelecimento.pdf'), sep='/')
pdf(pdf_name)
print(plt)
dev.off()

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

wb_name <- paste0(region_name, ' - Vínculos por município.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)
counts <- do.call('rbind', counts)

# Let's plot this thing
plt <- ggplot()
plt <- plt + geom_point(data=counts, aes(bins, F), color='red', size=I(2), alpha=I(0.4))

# Professionals empirical distribution per city
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

wb_name <- paste0(region_name, ' - Profissionais por município.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)
counts <- do.call('rbind', counts)

# Let's plot this thing
plt <- plt + geom_point(data=counts, aes(bins, F), color='black', size=I(2), alpha=I(0.4))
plt <- plt + scale_x_log10('Tamanho',
                           labels=scientific10,
                           expand=c(0,0))
plt <- plt + scale_y_log10('Frequência relativa',
                           labels=scientific10,
                           breaks=c(c(1, 5) %o% 10^(-5:0)),
                           expand=c(0, 0))

plt <- plt + theme_classic() + theme(panel.margin=unit(2, 'lines')) + facet_wrap(~NU_COMPETENCIA)

pdf_name <- paste(focalDir, paste0(region_name, ' - Escala por município.pdf'), sep='/')
pdf(pdf_name)
print(plt)
dev.off()

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
counts <- counts[, R := 1:length(F), by=NU_COMPETENCIA]

wb_name <- paste0(region_name, ' - Vínculos por região.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)
counts <- do.call('rbind', counts)

# Let's plot this thing
plt <- ggplot()
plt <- plt + geom_point(data=counts, aes(R, F), color='red', size=I(2), alpha=I(0.4))

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
counts <- counts[, R := 1:length(F), by=NU_COMPETENCIA]

wb_name <- paste0(region_name, ' - Profissionais por região.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)
counts <- do.call('rbind', counts)

# Let's plot this thing
plt <- plt + geom_point(data=counts, aes(R, F), color='black', size=I(2), alpha=I(0.4))
plt <- plt + scale_x_continuous('Tamanho',
                                limits=c(0, 500),
                                expand=c(0,0))
plt <- plt + scale_y_log10('Frequência relativa',
                           labels=scientific10,
                           breaks=c(c(1, 5) %o% 10^(-5:0)),
                           expand=c(0, 0))

plt <- plt + theme_classic() + theme(panel.margin=unit(2, 'lines')) + facet_wrap(~NU_COMPETENCIA)

pdf_name <- paste(focalDir, paste0(region_name, ' - Escala por região.pdf'), sep='/')
pdf(pdf_name)
print(plt)
dev.off()

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
counts <- counts[, R := 1:length(F), by=NU_COMPETENCIA]

wb_name <- paste0(region_name, ' - Vínculos por UF.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)

counts <- do.call('rbind', counts)

# Let's plot this thing
plt <- ggplot()
plt <- plt + geom_point(data=counts, aes(R, F), color='red', size=I(2), alpha=I(0.4))

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
counts <- counts[, R := 1:length(F), by=NU_COMPETENCIA]

wb_name <- paste0(region_name, ' - Profissionais por UF.xls')
wb <- loadWorkbook(paste(focalDir, wb_name, sep='/'), create=TRUE)
counts <- split(counts, counts$NU_COMPETENCIA)

for(s in names(counts)){
    
    createSheet(wb, s)
    writeWorksheet(wb, counts[[s]], s)
    
}

saveWorkbook(wb)
counts <- do.call('rbind', counts)

# Let's plot this thing
plt <- plt + geom_point(data=counts, aes(R, F), color='black', size=I(2), alpha=I(0.4))
plt <- plt + scale_x_continuous('Tamanho',
                                limits=c(0, 30),
                                expand=c(0,0))
plt <- plt + scale_y_log10('Frequência relativa',
                           labels=scientific10,
                           breaks=c(c(1, 5) %o% 10^(-5:0)),
                           expand=c(0, 0))

plt <- plt + theme_classic() + theme(panel.margin=unit(2, 'lines')) + facet_wrap(~NU_COMPETENCIA)

pdf_name <- paste(focalDir, paste0(region_name, ' - Escala por UF.pdf'), sep='/')
pdf(pdf_name)
print(plt)
dev.off()

