

r <- rep(NA, length(selected_cirs))

df <- data.frame(co_cir=r,
                 nm_cir=r,
                 cpf_out_i=r,
                 cpf_out_f=r,
                 cpf_ins_i=r,
                 cpf_ins_f=r,
                 cpf_exc_i=r,
                 cpf_exc_f=r,
                 cpf_not_exc_i=r,
                 cpf_not_exc_f=r,
                 cpf_stb=r,
                 cpf_mbl_out=r,
                 cpf_mbl_in=r)

rownames(df) <- selected_cirs

for(cir in selected_cirs){

    # Isolate focal health region
    region <- with(selected_regions, selected_regions[which(CO_CIR == cir), ])
    region_name <- unique(region$NM_CIR)

    df[cir, ]$co_cir <- cir
    df[cir, ]$nm_cir <- region_name

    #Filter things by region
    fdata <- with(data, data[which(CO_CIR == cir)])
 
    # Filter things by CPF
    # CPF with touch smth outside focal region per competence
    cpf_out <- unique(data[which(CO_CIR != cir), c('NU_COMPETENCIA', 'CO_CPF'),
                      with=F])
    cpf_out <- split(cpf_out, cpf_out$NU_COMPETENCIA)
    cpf_out <- lapply(cpf_out, function(x) x$CO_CPF)

    df[cir, ]$cpf_out_i <- length(cpf_out[[1]])
    df[cir, ]$cpf_out_f <- length(cpf_out[[2]])

    # CPF wich are restriced to the focal region per competence
    cpf_ins <- unique(fdata[, c('NU_COMPETENCIA', 'CO_CPF'), with=F])
    cpf_ins <- split(cpf_ins, cpf_ins$NU_COMPETENCIA)
    cpf_ins <- lapply(cpf_ins, function(x) x$CO_CPF)

    df[cir, ]$cpf_ins_i <- length(cpf_ins[[1]])
    df[cir, ]$cpf_ins_f <- length(cpf_ins[[2]])

    cpf_exc <-list()
    cpf_not_exc <- list()
    for(c in competences){

        cpf_exc[[c]] <- cpf_ins[[c]][!cpf_ins[[c]] %in% cpf_out[[c]]]
        cpf_not_exc[[c]] <- cpf_ins[[c]][!cpf_ins[[c]] %in% cpf_exc[[c]]]

    }

    df[cir, ]$cpf_exc_i <- length(cpf_exc[[1]])
    df[cir, ]$cpf_exc_f <- length(cpf_exc[[2]])

    df[cir, ]$cpf_not_exc_i <- length(cpf_not_exc[[1]])
    df[cir, ]$cpf_not_exc_f <- length(cpf_not_exc[[2]])

    # CPFs which remained throughout the period
    cpf_stb <- cpf_exc[[2]][which(cpf_exc[[2]] %in% cpf_exc[[1]])]
    df[cir, ]$cpf_stb <- length(cpf_stb)

    # CPFs which emmigrated
    cpf_mbl_out <- cpf_exc[[1]][which(cpf_exc[[1]] %in% cpf_out[[2]])]

    df[cir, ]$cpf_mbl_out <- length(cpf_mbl_out)
    # CPFs which immigrated
    cpf_mbl_in <- cpf_exc[[2]][which(cpf_exc[[2]] %in% cpf_out[[1]])]

    df[cir, ]$cpf_mbl_in <- length(cpf_mbl_in)



   
}

wb_name <- 'Mobilidade.xls'
wb <- loadWorkbook(paste(resultsDir, wb_name, sep='/'), create=TRUE)
setStyleAction(wb, XLC$'STYLE_ACTION.NONE')
createSheet(wb, 'Mobilidade')
writeWorksheet(wb, df, 'Mobilidade')
saveWorkbook(wb)

