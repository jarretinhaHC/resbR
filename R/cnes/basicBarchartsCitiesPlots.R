﻿
for(cir in selected_cirs){

    # Isolate focal health region
    region <- with(selected_regions, selected_regions[which(CO_CIR == cir), ])
    region_name <- unique(region$NM_CIR)

    # Create folder to store results
    focalDir <- file.path(paste(resultsDir, region_name, sep='/'))
    dir.create(focalDir)

    #Filter things by region
    fdata  <- with(data, data[which(CO_CIR ==  cir)])

    # Contracts per city per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CPF', 'MUN.NO_MUNICIPIO'), with=F]
    p <- tmp[, list(P=length(unique(CO_CPF)), by=NU_COMPETENCIA]

    counts <- tmp[, {v <- tapply(CO_CPF, MUN.NO_MUNICIPIO, length);
                  list(MUNICÍPIOS=names(v),
                       VÍNCULOS=as.vector(v))},
                  by=NU_COMPETENCIA]

    counts <- counts[, SOMA := sum(VÍNCULOS), by=NU_COMPETENCIA]
    counts <- counts[, pVÍNCULOS := VÍNCULOS / SOMA * 100]
    counts <- counts[, SOMA := NULL]

    # Professionals per city per competence
    tmp <- unique(tmp)

    tmp <- tmp[, {v <- tapply(CO_CPF, MUN.NO_MUNICIPIO, length);
               list(MUNICÍPIOS=names(v),
                    PROFISSIONAIS=as.vector(v))},
               by=NU_COMPETENCIA]

    tmp <- split(tmp, tmp$NU_COMPETENCIA)
    for(c in names(tmp)){tmp[[c]]$pPROFISSIONAIS <- tmp[[c]]$PROFISSIONAIS / p[[c]]$P * 100}
    tmp <- do.call('rbind', tmp)

    counts$PROFISSIONAIS <- tmp$PROFISSIONAIS
    counts$pPROFISSIONAIS <- tmp$pPROFISSIONAIS

    # Places per city per competence
    tmp <- fdata[, c('NU_COMPETENCIA', 'CO_CNES', 'MUN.NO_MUNICIPIO'), with=F]
    tmp <- unique(tmp)

    tmp <- tmp[, {v <- tapply(CO_CNES, MUN.NO_MUNICIPIO, length);
               list(MUNICÍPIOS=names(v),
                    ESTABELECIMENTOS=as.vector(v))},
               by=NU_COMPETENCIA]

    tmp <- tmp[, SOMA := sum(ESTABELECIMENTOS), by=NU_COMPETENCIA]
    tmp <- tmp[, pESTABELECIMENTOS := ESTABELECIMENTOS/SOMA * 100]
    tmp <- tmp[, SOMA := NULL]

    counts$ESTABELECIMENTOS <- tmp$ESTABELECIMENTOS
    counts$pESTABELECIMENTOS <- tmp$pESTABELECIMENTOS

    counts$VP <- with(counts, VÍNCULOS/PROFISSIONAIS)
    counts$VE <- with(counts, VÍNCULOS/ESTABELECIMENTOS)
    counts$PE <- with(counts, PROFISSIONAIS/ESTABELECIMENTOS)
    counts$PH <- with(counts, PROFISSIONAIS/region$POPULACAO * 1000)
    counts$VH <- with(counts, VÍNCULOS/region$POPULACAO * 1000)
    counts$EH <- with(counts, ESTABELECIMENTOS/region$POPULACAO * 1000)

    # Ratios, ratios, ratios
    standard_breaks=trans_breaks('identity', function(t) t, n=10)

    # Convert to long format
    df.long <- melt(counts,
                    id.vars=c('NU_COMPETENCIA', 'MUNICÍPIOS'),
                    measure.vars=c('VP', 'VE', 'PE', 'VH', 'PH', 'EH'),
                    variable.name='RAZÃO',
                    value.name='R')

    # Create ggplot structure and define aesthetics
    plt <- ggplot(data=df.long, aes(x=MUNICÍPIOS, y=R, fill=RAZÃO))
    # Add geoms
    plt <- plt + geom_bar(stat='identity',
                          color='black',
                          position='dodge',
                          size=0.1)
    # Set up x scale
    plt <- plt + scale_x_discrete(name='Município',
                                  expand=c(0, 0))
 
    # Set up y scale
    plt <- plt + scale_y_continuous(name='Razão',
                                    breaks=standard_breaks,
                                    expand=c(0, 0))

    # Set up fill scale
    plt <- plt + scale_fill_brewer('',
                                   palette='Set1')
#                                   labels=c('V/P',
#                                            'V/E',
#                                            'P/E',
#                                            ''))

    # Set up theme stuff
    plt <- plt + theme_classic()
    
    # Basic text
    plt <- plt + theme(text=element_text(size=16))
    plt <- plt + theme(axis.text.x=element_text(size=6,
                                                 angle=90,
                                                 vjust=0.5,
                                                 hjust=1))
 
    # Legend adjustments
    plt <- plt + theme(legend.position='right',
                       legend.direction='vertical')


    # Set up faceting
    plt <- plt + facet_wrap(~NU_COMPETENCIA, ncol=2)

    pdf_name <- paste0(region_name, ' - Profissionais, vínculos e estabelecimentos por município - razões.pdf')
    pdf(paste(focalDir, pdf_name, sep='/'), height=7, width=7)
    print(plt)
    dev.off()

    # Professional and contracts, counts
    standard_breaks=trans_breaks('identity', function(t) t, n=10)

    # Convert to long format
    df.long <- melt(counts,
                    id.vars=c('NU_COMPETENCIA', 'MUNICÍPIOS'),
                    measure.vars=c('PROFISSIONAIS', 'VÍNCULOS'),
                    variable.name='CONTAGEM',
                    value.name='C')

    # Create ggplot structure and define aesthetics
    plt <- ggplot(data=df.long, aes(x=MUNICÍPIOS, y=C, fill=CONTAGEM))
    # Add geoms
    plt <- plt + geom_bar(stat='identity',
                          color='black',
                          position='dodge',
                          size=0.1)
    # Set up x scale
    plt <- plt + scale_x_discrete(name='Município',
                                  expand=c(0, 0))
 
    # Set up y scale
    plt <- plt + scale_y_continuous(name='Contagem',
                                    breaks=standard_breaks,
                                    expand=c(0, 0))

    # Set up fill scale
    plt <- plt + scale_fill_brewer('',
                                   palette='Set1',
                                   labels=c('Profissionais', 'Vínculos'))

    # Set up theme stuff
    plt <- plt + theme_classic()
    
    #Basic text
    plt <- plt + theme(text=element_text(size=16))
    plt <- plt + theme(axis.text.x=element_text(size=6,
                                                 angle=90,
                                                 vjust=0.5,
                                                 hjust=1))


    # Set up faceting
    plt <- plt + facet_wrap(~NU_COMPETENCIA, ncol=2)

    # Plot
    pdf_name <- paste0(region_name, ' - Profissionais e vínculos por município - contagem.pdf')
    pdf(paste(focalDir, pdf_name, sep='/'), height=7, width=7)
    print(plt)
    dev.off()

    # Professional and contracts, counts
    standard_breaks=trans_breaks('identity', function(t) t, n=10)
 
    # Convert to long format
    df.long <- melt(counts,
                    id.vars=c('NU_COMPETENCIA', 'MUNICÍPIOS'),
                    measure.vars=c('pPROFISSIONAIS', 'pVÍNCULOS'),
                    variable.name='PERCENTUAL',
                    value.name='P')

    # Create ggplot structure and define aesthetics
    plt <- ggplot(data=df.long, aes(x=MUNICÍPIOS, y=P, fill=PERCENTUAL))
    # Add geoms
    plt <- plt + geom_bar(stat='identity',
                          color='black',
                          position='dodge',
                          size=0.1)
    # Set up x scale
    plt <- plt + scale_x_discrete(name='Município',
                                  expand=c(0, 0))
 
    # Set up y scale
    plt <- plt + scale_y_continuous(name='Percentual',
                                    breaks=standard_breaks,
                                    expand=c(0, 0))

    # Set up fill scale
    plt <- plt + scale_fill_brewer('',
                                   palette='Set1',
                                   labels=c('Profissionais', 'Vínculos'))
    
    # Set up theme stuff
    plt <- plt + theme_classic()
    
    #Basic text
    plt <- plt + theme(text=element_text(size=16))
    plt <- plt + theme(axis.text.x=element_text(size=6,
                                                 angle=90,
                                                 vjust=0.5,
                                                 hjust=1))


    # Set up faceting
    plt <- plt + facet_wrap(~NU_COMPETENCIA, ncol=2)

    # Plot
    pdf_name <- paste0(region_name, ' - Profissionais e vínculos por município - percentual.pdf')
    pdf(paste(focalDir, pdf_name, sep='/'), height=7, width=7)
    print(plt)
    dev.off()

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
                             QTD_MUNICIPIOS=as.character(unique(d$L)),
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

    # Professionals, contribution per city
    standard_breaks=trans_breaks('identity', function(t) t, n=10)
 
    # Create ggplot structure and define aesthetics
    plt <- ggplot(data=counts, aes(x=NM_MUNICIPIO,
                                   y=PERCENTUAL,
                                   fill=QTD_MUNICIPIOS))

    # Add geoms
    plt <- plt + geom_bar(stat='identity',
                          color='black',
                          position='dodge',
                          size=0.1,
                          drop=FALSE)

    # Set up x scale
    plt <- plt + scale_x_discrete(name='Município',
                                  expand=c(0, 0))
 
    # Set up y scale
    plt <- plt + scale_y_continuous(name='Percentual',
                                    breaks=standard_breaks,
                                    limits=c(0,100),
                                    expand=c(0, 0))

    # Set up fill scale
    plt <- plt + scale_fill_brewer('Municípios', palette='Set1')
    
    # Set up theme stuff
    plt <- plt + theme_classic()
    
    #Basic text
    plt <- plt + theme(text=element_text(size=16))
    plt <- plt + theme(axis.text.x=element_text(size=6,
                                                 angle=90,
                                                 vjust=0.5,
                                                 hjust=1))
    # Legend adjustments
    plt <- plt + theme(legend.position='top')
    # Set up faceting
    plt <- plt + facet_wrap(~NU_COMPETENCIA, ncol=1)

    # Plot
    pdf_name <- paste0(region_name, ' - Profissionais por município - contribuição percentual.pdf')
    pdf(paste(focalDir, pdf_name, sep='/'), height=7, width=7)
    print(plt)
    dev.off()

    # Professionals, brute contribution per city
    standard_breaks=trans_breaks('identity', function(t) t, n=10)
 
    # Create ggplot structure and define aesthetics
    plt <- ggplot(data=counts, aes(x=NM_MUNICIPIO,
                                   y=CONTAGEM,
                                   fill=QTD_MUNICIPIOS))

    # Add geoms
    plt <- plt + geom_bar(stat='identity',
                          color='black',
                          position='dodge',
                          size=0.1,
                          drop=FALSE)

    # Set up x scale
    plt <- plt + scale_x_discrete(name='Município',
                                  expand=c(0, 0))
 
    # Set up y scale
    plt <- plt + scale_y_continuous(name='Contagem',
                                    breaks=standard_breaks,
                                    expand=c(0, 0))

    # Set up fill scale
    plt <- plt + scale_fill_brewer('Municípios', palette='Set1')
    
    # Set up theme stuff
    plt <- plt + theme_classic()
    
    #Basic text
    plt <- plt + theme(text=element_text(size=16))
    plt <- plt + theme(axis.text.x=element_text(size=6,
                                                 angle=90,
                                                 vjust=0.5,
                                                 hjust=1))
    # Legend adjustments
    plt <- plt + theme(legend.position='top')
    # Set up faceting
    plt <- plt + facet_wrap(~NU_COMPETENCIA, ncol=1)

    # Plot
    pdf_name <- paste0(region_name, ' - Profissionais por município - contribuição bruta.pdf')
    pdf(paste(focalDir, pdf_name, sep='/'), height=7, width=7)
    print(plt)
    dev.off()

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
                             QTD_VINCULOS=as.character(unique(d$L)),
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

    # Contracts, contribution per city
    standard_breaks=trans_breaks('identity', function(t) t, n=10)
    colourCount <- length(unique(counts$QTD_VINCULOS))
    getPalette <- colorRampPalette(brewer.pal(12, 'Set3'))
 
    # Create ggplot structure and define aesthetics
    plt <- ggplot(data=counts, aes(x=NM_MUNICIPIO,
                                   y=PERCENTUAL,
                                   fill=QTD_VINCULOS))

    # Add geoms
    plt <- plt + geom_bar(stat='identity',
                          color='black',
                          position='dodge',
                          size=0.1,
                          drop=FALSE)

    # Set up x scale
    plt <- plt + scale_x_discrete(name='Município',
                                  expand=c(0, 0))
 
    # Set up y scale
    plt <- plt + scale_y_continuous(name='Percentual',
                                    breaks=standard_breaks,
                                    limits=c(0,100),
                                    expand=c(0, 0))

    # Set up fill scale
    plt <- plt + scale_fill_manual('Vínculos', values=getPalette(colourCount))
    
    # Set up theme stuff
    plt <- plt + theme_classic()
    
    #Basic text
    plt <- plt + theme(text=element_text(size=16))
    plt <- plt + theme(axis.text.x=element_text(size=6,
                                                 angle=90,
                                                 vjust=0.5,
                                                 hjust=1))
    # Legend adjustments
    plt <- plt + theme(legend.position='top')
    plt <- plt + guides(fill=guide_legend(nrow=2))
    # Set up faceting
    plt <- plt + facet_wrap(~NU_COMPETENCIA, ncol=1)

    # Plot
    pdf_name <- paste0(region_name, ' - Vínculos por município - contribuição percentual.pdf')
    pdf(paste(focalDir, pdf_name, sep='/'), height=7, width=7)
    print(plt)
    dev.off()

    # Contracts, contribution per city
    standard_breaks=trans_breaks('identity', function(t) t, n=10)
    colourCount <- length(unique(counts$QTD_VINCULOS))
    getPalette <- colorRampPalette(brewer.pal(12, 'Set3'))

    # Create ggplot structure and define aesthetics
    plt <- ggplot(data=counts, aes(x=NM_MUNICIPIO,
                                   y=CONTAGEM,
                                   fill=QTD_VINCULOS))

    # Add geoms
    plt <- plt + geom_bar(stat='identity',
                          color='black',
                          position='dodge',
                          size=0.1,
                          drop=FALSE)

    # Set up x scale
    plt <- plt + scale_x_discrete(name='Município',
                                  expand=c(0, 0))
 
    # Set up y scale
    plt <- plt + scale_y_continuous(name='Contagem',
                                    breaks=standard_breaks,
                                    limits=c(0,100),
                                    expand=c(0, 0))

    # Set up fill scale
    plt <- plt + scale_fill_manual('Vínculos', values=getPalette(colourCount))
    
    # Set up theme stuff
    plt <- plt + theme_classic()
    
    #Basic text
    plt <- plt + theme(text=element_text(size=16))
    plt <- plt + theme(axis.text.x=element_text(size=8,
                                                 angle=90,
                                                 vjust=0.5,
                                                 hjust=1))
    # Legend adjustments
    plt <- plt + theme(legend.position='top')
    plt <- plt + guides(fill=guide_legend(nrow=2))
    # Set up faceting
    plt <- plt + facet_wrap(~NU_COMPETENCIA, ncol=1)

    # Plot
    pdf_name <- paste0(region_name, ' - Vínculos por município - contribuição bruta.pdf')
    pdf(paste(focalDir, pdf_name, sep='/'), height=7, width=7)
    print(plt)
    dev.off()

}
