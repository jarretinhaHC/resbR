# Basic setup for graphics.


standard_breaks=trans_breaks('identity', function(t) t, n=10)


    plt <- ggplot(data=counts, aes(N))
    + geom_bar(binwidth=1,
               origin=-.5,
               colour='black',
               aes(fill=COMPETÊNCIA))
    + scale_x_continuous(name='Número de estabelecimentos',
                         breaks=pretty_breaks(),
                         expand=c(0, 0))
    + scale_y_continuous(name='Profissionais',
                         breaks=standard_breaks, expand=c(0, 0))
    + theme_classic()
    + theme(legend.position='',
            text=element_text(size=16))
    + facet_grid(~COMPETÊNCIA)
    + scale_fill_brewer(palette='Set1')

    filename <- paste0('Estabelecimentos por profissional - Contagem - ', REG, '.pdf')
    pdf(paste(subDir, filename, sep='/')); print(plt); dev.off()


plt <- ggplot(t, aes(n, counts, fill=NU_COMPETENCIA)) + geom_bar(stat='identity', position='dodge')


    tmp <- lapply(professionals_reg, function(x) x[, c('CO_CNES', 'CO_CPF_DUP'), with=F])
    counts <- lapply(tmp, function(x) tapply(x$CO_CNES, x$CO_CPF_DUP, length))
    counts <- lapply(counts, function(x) data.frame(CPF=names(x), N=as.vector(x), stringsAsFactor=F))
    counts <- ldply(counts, .id='COMPETÊNCIA')

    # Contagem
    plt <- ggplot(data=counts, aes(x=N)) + geom_bar(binwidth=1, origin=-.5, colour='black', aes(fill=COMPETÊNCIA)) + scale_x_continuous(name='Vínculos', breaks=pretty_breaks(), expand=c(0, 0)) + scale_y_continuous(name='Profissionais', breaks=standard_breaks, expand=c(0, 0)) + theme_classic() + theme(legend.position='', text=element_text(size=16), axis.text.x=element_text(size=10), axis.text.y=element_text(size=10), plot.margin=unit(rep(0.25, 4), 'lines'), panel.margin=unit(0.75, 'lines')) + facet_grid(~COMPETÊNCIA) + scale_fill_brewer(palette='Set1')

    filename <- paste0('Vínculos por profissional - Contagem - ', REG, '.pdf')
    pdf(file.path(paste(subDir, filename, sep='/'))); print(plt); dev.off()

    # Percentual 
    plt <- ggplot(data=counts, aes(x=N)) + geom_bar(binwidth=1, origin=-.5, colour='black', aes(y=s * (..count..)/sum(..count..), fill=COMPETÊNCIA, s=length(unique(COMPETÊNCIA)))) + scale_x_continuous(name='Vínculos', breaks=pretty_breaks(), expand=c(0, 0)) + scale_y_continuous(name='Profissionais', breaks=standard_breaks, expand=c(0, 0), labels=percent) + theme_classic() + theme(legend.position='', text=element_text(size=16), axis.text.x=element_text(size=10), axis.text.y=element_text(size=10), plot.margin=unit(rep(0.25, 4), 'lines'), panel.margin=unit(0.75, 'lines')) + facet_grid(~COMPETÊNCIA) + scale_fill_brewer(palette='Set1')

    filename <- paste0('Vínculos por profissional - Percentual - ', REG, '.pdf')
    pdf(file.path(paste(subDir, filename, sep='/'))); print(plt); dev.off()

    # Estabelecimentos por município
    tmp <- lapply(professionals_reg, function(x) x[, c('CO_CNES', 'MUN.NO_MUNICIPIO'), with=F])
    tmp <- lapply(tmp, unique)
    counts <- lapply(tmp, function(x) tapply(x$CO_CNES, x$MUN.NO_MUNICIPIO, length))
    counts <- lapply(counts, function(x) data.table(MUNICIPIO=names(x), N=x))
    pcounts <- lapply(counts, function(x) data.table(P=x$N/sum(x$N)))
    counts <- ldply(counts, .id='COMPETÊNCIA')
    pcounts <- ldply(pcounts, .id='COMPETÊNCIA')
    counts$P <- pcounts$P
    rm(pcounts)

    colors <- colorRampPalette(brewer.pal(9, "Set1"))
    ngroups <- length(unique(counts$MUNICIPIO))

    # Contagem
    plt <- ggplot(data=counts, aes(x=MUNICIPIO, y=N, fill=MUNICIPIO)) + geom_bar(stat='identity', position='stack') + scale_x_discrete(name='Município', expand=c(0, 0)) + scale_y_continuous(name='Estabelecimentos', breaks=standard_breaks, expand=c(0, 0)) + theme_classic() + theme(legend.position='', text=element_text(size=16), axis.text.x=element_text(size=6, angle=90, vjust=0.5, hjust=1)) + facet_grid(~COMPETÊNCIA) + scale_fill_manual(values=colors(ngroups))

    filename <- paste0('Estabelecimentos por município gestor - Contagem - ', REG, '.pdf')
    pdf(file.path(paste(subDir, filename, sep='/'))); print(plt); dev.off()

    # Percentual
    plt <- ggplot(data=counts, aes(x=MUNICIPIO, y=P, fill=MUNICIPIO)) + geom_bar(stat='identity', position='stack') + scale_x_discrete(name='Município', expand=c(0, 0)) + scale_y_continuous(name='Estabelecimentos', labels=percent, breaks=standard_breaks, expand=c(0, 0)) + theme_classic() + theme(legend.position='', text=element_text(size=16), axis.text.x=element_text(size=6, angle=90, vjust=0.5, hjust=1)) + facet_grid(~COMPETÊNCIA) + scale_fill_manual(values=colors(ngroups))

    filename <- paste0('Estabelecimentos por município gestor - Percentual - ', REG, '.pdf')
    pdf(file.path(paste(subDir, filename, sep='/'))); print(plt); dev.off()

    # Municípios por profissional
    tmp <- lapply(professionals_reg, function(x) x[, c('CO_CPF_DUP', 'MUN.NO_MUNICIPIO'), with=F])
    tmp <- lapply(tmp, unique)

    counts <- lapply(tmp, function(x) tapply(x$MUN.NO_MUNICIPIO, x$CO_CPF_DUP, length))
    counts <- lapply(counts, function(x) data.frame(CPF=names(x), N=as.vector(x), stringsAsFactors=F))
    counts <- ldply(counts, .id='COMPETÊNCIA')

    # Contagem
    plt <- ggplot(data=counts, aes(x=N)) + geom_bar(binwidth=1, origin=-.5, colour='black', aes(fill=COMPETÊNCIA)) + scale_x_continuous(name='Número de municípios') + scale_y_continuous(name='Profissionais', expand=c(0, 0)) + theme_classic() + theme(legend.position='', text=element_text(size=16)) + facet_grid(~COMPETÊNCIA) + scale_fill_brewer(palette='Set1')

    filename <- paste0('Municípios por profissional - Contagem - ', REG, '.pdf')
    pdf(file.path(paste(subDir, filename, sep='/'))); print(plt); dev.off()

    # Percentual
    plt <- ggplot(data=counts, aes(x=N)) + geom_bar(binwidth=1, origin=-.5, colour='black', aes(y=s * (..count..)/sum(..count..), fill=COMPETÊNCIA, s=length(unique(COMPETÊNCIA)))) + scale_x_continuous(name='Número de municípios') + scale_y_continuous(name='Profissionais', labels=percent, breaks=standard_breaks, expand=c(0, 0)) + theme_classic() + theme(legend.position='', text=element_text(size=16)) + facet_grid(~COMPETÊNCIA) + scale_fill_brewer(palette='Set1')

    filename <- paste0('Municípios por profissional - Percentual - ', REG, '.pdf')
    pdf(file.path(paste(subDir, filename, sep='/'))); print(plt); dev.off()

    filename <- paste0('Sumário regional - ', REG, '.csv')
    write.csv(df, file.path(paste(subDir, filename, sep='/')))

    # Municípios por profissional, particionado por município
    tmp <- lapply(professionals_reg, function(x) x[, c('CO_CPF_DUP', 'MUN.NO_MUNICIPIO'), with=F])
    tmp <- lapply(tmp, unique)

    counts <- lapply(tmp, function(x) tapply(x$MUN.NO_MUNICIPIO, x$CO_CPF_DUP, c))
    counts <- lapply(counts, function(x) data.table(CPF=names(x), S=x, L=sapply(x, length)))
    counts <- ldply(counts, .id='COMPETÊNCIA')

    r <- range(counts$L)

    length_list <- list()

    cities <- unique(as.vector(unlist(counts$S)))
    cities <- cities[order(cities)]

    for(len in r[1]:r[2]){

        competence_list <- list()

        for(competence in competences){

            tmp <- counts[which(counts$COMPETÊNCIA == competence & counts$L == len), ]
            hits <- length(tmp$L)
            df <- data.frame(Cities=region$NM_MUNICIPIO, row.names=cities)
            tbl <- table(as.vector(unlist(tmp$S)))
            df$counts <- 0
            S <- sum(tbl)
            for(s in names(tbl)){

                if(S){

                    df[s, ]$counts <- tbl[[s]] / S * hits * len

                }
            }
            

            S <- sum(df$counts)
            df$freq <- 0
            if(S){

                df$freq <- df$counts / S

            }

            competence_list[[competence]] <- df

        }

        length_list[[as.character(len)]] <- ldply(competence_list, .id='COMPETÊNCIA')

    }

    t <- ldply(length_list, .id='L')

    colors <- colorRampPalette(brewer.pal(9, "Set1"))
    ngroups <- length(unique(t$Cities))

    plt <- ggplot(data=t, aes(x=L, y=freq)) + geom_bar(stat='identity', aes(fill=Cities)) + theme(legend.position='', axis.text.x=element_text(size=6, angle=90, vjust=.5, hjust=1), strip.text=element_text(face='bold')) + facet_wrap(~Cities + COMPETÊNCIA, scales='free_y', ncol=5) + scale_fill_manual(name='Municípios', values=colors(ngroups)) + scale_y_continuous(name='Profissionais', labels=percent, breaks=standard_breaks) + theme_classic() + xlab('Número de municípios')

    filename <- paste0('Municípios por profissional - Contribuição por município - ', REG, '.pdf')
    pdf(file.path(paste(subDir, filename, sep='/')), height=48, width=18); print(plt); dev.off()

}
