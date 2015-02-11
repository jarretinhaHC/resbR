#Load data
library(XLConnect)
library(naturalsort)
library(data.table)
library(reshape2)
library(plyr)
library(scales)
library(grid)
library(ggplot2)
library(RColorBrewer)

files <- Sys.glob('/health/CNES/NESCON/professionals/*.psv')

mainDir <- file.path('/home/jarretinha/dev/resbR/scratch/selected')
setwd(mainDir)

header <- read.table('/home/jarretinha/dev/resbR/R/static/carga_horaria_header',
                     header=F,
                     sep='\t',
                     stringsAsFactor=FALSE)[[1]]

regions <- read.table('/home/jarretinha/dev/resbR/R/static/CIR_BR.tsv',
                      sep='\t',
                      header=TRUE,
                      stringsAsFactor=FALSE,
                      quote="")

selected <- read.table('/home/jarretinha/dev/resbR/R/static/selected_CIR.tsv',
                       sep='\t',
                       header=TRUE,
                       stringsAsFactor=FALSE,
                       quote="")

selected$CO_CIR <- as.character(selected$CO_CIR)

to_from_CBO <- read.table('/home/jarretinha/dev/resbR/R/static/de_para_CBO.csv',
                          sep=',',
                          header=TRUE,
                          stringsAsFactor=FALSE,
                          row.names='DS_CBO_OCUPACAO',
                          quote="")


standard_breaks=trans_breaks('identity', function(t) t, n=10)

# Auxiliary function to count counts
charter <- function(idx, val){

    tmp <- tapply(idx, val, length)
    tapply(names(tmp), as.vector(tmp), length)

}

# Load all data
tmp <- list()
for(f in files){

    competence <- unlist(strsplit(f, '[._]'))[4]
    tmp[[competence]] <- fread(f,
                               sep='|',
                               integer64='character',
                               header=FALSE,
                               stringsAsFactor=FALSE)

    setnames(tmp[[competence]], c(header, 'V57'))

}

workload <- rbindlist(tmp)

# Clean up unused data
rm(tmp)
gc()

# Prepare for the ride...
# Big for loop!!!

# Selected to be used in filters
regions <- regions[which(regions$CO_CIR %in% selected$CO_CIR), ]

workload <- with(workload,
                  workload[which(CO_MUNICIPIO_GESTOR
                                 %in% regions$CO_MUNICIPIO
                                 & grepl('^MÉDICO ', DS_CBO_OCUPACAO)
                                 & DS_CBO_OCUPACAO != 'MÉDICO VETERINÁRIO')])

### Basic description ###
for(cir in unique(regions$CO_CIR)){

    region <- regions[which(regions$CO_CIR == cir), ]
    REG <- unique(region$NM_CIR)

    subDir <- file.path(paste(mainDir, REG, sep='/'))
    dir.create(subDir)

    #Filter things by region
    rworkload  <- with(workload, workload[which(CO_MUNICIPIO_GESTOR %in% region$CO_MUNICIPIO)])

    excel <- paste0(REG,' - Seção 01 - Contagens básicas.xls')
    wb1 <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    createSheet(wb1, 'Contagens básicas')

    # Data frame with NU_COMPETENCE as guide
    df <- data.frame(NU_COMPETENCIA=unique(workload$NU_COMPETENCIA))

    # Professionals per competence
    df$PROFISSIONAIS <- with(rworkload, tapply(CO_CPF, NU_COMPETENCIA, function(x) length(unique(x))))

    # Contracts per competence
    df$VÍNCULOS <- with(rworkload, tapply(CO_CPF, NU_COMPETENCIA, function(x) length(x)))

    # Places per competence
    df$ESTABELECIMENTOS <- with(rworkload, tapply(CO_CNES, NU_COMPETENCIA, function(x) length(unique(x))))

    # Contracts per professional ratio
    df$VP <-with(df, sprintf('%2.2f',VÍNCULOS/PROFISSIONAIS))

    # Places per professional ratio
    df$PE <-with(df, sprintf('%2.2f', PROFISSIONAIS/ESTABELECIMENTOS))

    tmp <- rworkload[, c('CO_CNES', 'NU_COMPETENCIA', 'CO_CPF'), with=F]
    t <- tmp
    u <- unique(t)

    # Bar chart of places per professional per competence
    counts <- u[, {v <- charter(CO_CNES, CO_CPF);
                list(ESTABELECIMENTOS=as.integer(names(v)),
                     CONTAGEM=as.vector(v))},
                by=NU_COMPETENCIA]

    excel <- paste0(REG, ' - Seção 01 - Estabelecimentos por professional.xls')
    wb <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }
    saveWorkbook(wb)

    # Bar chart of contracts per professional per competence
    counts <- t[, {v <- charter(CO_CNES, CO_CPF);
                list(VÍNCULOS=as.integer(names(v)),
                     CONTAGEM=as.vector(v))},
                by=NU_COMPETENCIA]

    excel <- paste0(REG, ' - Seção 01 - Vínculos por professional.xls')
    wb <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }
    saveWorkbook(wb)

    # Unique contracts ratio per competence
    t <- tapply(t$CO_CPF, t$NU_COMPETENCIA, length)
    u <- tapply(u$CO_CPF, u$NU_COMPETENCIA, length)
    r <- u/t
    df$R <- r

    writeWorksheet(wb1, df, 'Contagens básicas')
    saveWorkbook(wb1)

    rm(t, u, r)
    gc()

    # Places per city per competence
    tmp <- rworkload[, c('NU_COMPETENCIA', 'CO_CNES', 'MUN.NO_MUNICIPIO'), with=F]
    tmp <- unique(tmp)

    counts <- tmp[, {t <- tapply(CO_CNES, MUN.NO_MUNICIPIO, length);
                  list(MUNICÍPIOS=names(t),
                       CONTAGEM=as.vector(t))},
                  by=NU_COMPETENCIA]

    excel <- paste0(REG, ' - Seção 01 - Estabelecimentos por município.xls')
    wb <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

    # Bar chart, cities per professional per competence
    tmp <- rworkload[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]
    tmp <- unique(tmp)

    counts <- tmp[, {t <- charter(MUN.NO_MUNICIPIO, CO_CPF);
                  list(MUNICÍPIOS=names(t),
                       CONTAGEM=as.vector(t))},
                  by=NU_COMPETENCIA]

    excel <- paste0(REG, ' - Seção 01 - Municípios por profissional.xls')
    wb <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }
    saveWorkbook(wb)

    # Bar chart, cities per professional per CBO per competence
    # Partitioned by competence
    tmp <- rworkload[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO',
                        'DS_CBO_OCUPACAO'), with=F]
    tmp <- unique(tmp)

    counts <- tmp[, {t <- charter(MUN.NO_MUNICIPIO, CO_CPF);
                  list(MUNICÍPIOS=names(t),
                       CONTAGEM=as.vector(t))},
                  by=c('NU_COMPETENCIA', 'DS_CBO_OCUPACAO')]

    excel <- paste0(REG, ' - Seção 01 - Municípios por profissional, competência e ocupação.xls')
    wb <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }
    saveWorkbook(wb)

    # Bar chart, cities per professional per CBO per competence
    # Partitioned by occupation
    tmp <- rworkload[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO',
                        'DS_CBO_OCUPACAO'), with=F]
    tmp <- unique(tmp)

    counts <- tmp[, {t <- charter(MUN.NO_MUNICIPIO, CO_CPF);
                  list(MUNICÍPIOS=names(t),
                       CONTAGEM=as.vector(t))},
                  by=c('NU_COMPETENCIA', 'DS_CBO_OCUPACAO')]

    excel <- paste0(REG, ' - Seção 01 - Municípios por profissional, ocupação e competência.xls')
    wb <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    counts <- split(counts, counts$DS_CBO_OCUPACAO)

    for(s in names(counts)){

        sheet <- to_from_CBO[s, ]
        createSheet(wb, sheet)
        writeWorksheet(wb, counts[[s]], sheet)

    }
    saveWorkbook(wb)

    ### Beware!!! Complicated manipulations below
    # Bar panel, cities per professional per competence
    tmp <- rworkload[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]
    tmp <- unique(tmp)

    # Clever table with lists of cities per professional
    tmp <- split(tmp, tmp$NU_COMPETENCIA)
    for(s in names(tmp)){

        tmp[[s]] <- tmp[[s]][, {
            v <- tapply(tmp[[s]]$MUN.NO_MUNICIPIO,
                        tmp[[s]]$CO_CPF,
                        function(x) unique(c(x)));
        list(NU_COMPETENCIA=s,
             CO_CPF=names(v),
             S=sapply(v, c),
             L=sapply(v, length))},]

    }

    tmp <- rbindlist(tmp)

    cities <- unique(as.vector(unlist(tmp$S)))
    cities <- cities[order(cities)]

    # Split by number of cities
    df_list <- list()
    tmp <- split(tmp, tmp$L)
    for(l in tmp){
        # Split by competence
        v <- split(l, l$NU_COMPETENCIA)
        for(d in v){
            df <- data.frame(NM_MUNICIPIO=region$NM_MUNICIPIO,
                             CONTAGEM=0,
                             PERCENTUAL=0,
                             QTD_MUNICIPIOS=unique(d$L),
                             NU_COMPETENCIA=unique(d$NU_COMPETENCIA),
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

    excel <- paste0(REG, ' - Seção 02 - Municípios por profissional - Contribuição.xls')
    wb <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

    ### Beware!!! Complicated manipulations below
    # Bar panel, cities per professional per competence
    tmp <- rworkload[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]

    # Clever table with lists of cities per professional
    tmp <- split(tmp, tmp$NU_COMPETENCIA)
    for(s in names(tmp)){

        tmp[[s]] <- tmp[[s]][, {
            v <- tapply(tmp[[s]]$MUN.NO_MUNICIPIO,
                        tmp[[s]]$CO_CPF,
                        c);
        list(NU_COMPETENCIA=s,
             CO_CPF=names(v),
             S=sapply(v, c),
             L=sapply(v, length))},]

    }

    tmp <- rbindlist(tmp)

    cities <- unique(as.vector(unlist(tmp$S)))
    cities <- cities[order(cities)]

    # Split by number of cities
    df_list <- list()
    tmp <- split(tmp, tmp$L)
    for(l in tmp){
        # Split by competence
        v <- split(l, l$NU_COMPETENCIA)
        for(d in v){
            df <- data.frame(NM_MUNICIPIO=region$NM_MUNICIPIO,
                             CONTAGEM=0,
                             PERCENTUAL=0,
                             QTD_VINCULOS=unique(d$L),
                             NU_COMPETENCIA=unique(d$NU_COMPETENCIA),
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

    excel <- paste0(REG, ' - Seção 02 - Municípios por vínculo - Contribuição.xls')
    wb <- loadWorkbook(paste(subDir, excel, sep='/'), create=TRUE)
    counts <- split(counts, counts$NU_COMPETENCIA)

    for(s in names(counts)){

        createSheet(wb, s)
        writeWorksheet(wb, counts[[s]], s)

    }

    saveWorkbook(wb)

}


plt <- ggplot(data=dt_list[[1]], aes(Cities, Percent)) +
geom_bar(stat='identity') + facet_wrap(~L + Competence)

pdf('tmp.pdf'); plt; dev.off()

