# Data functions

# Data loader
# Should take care here! Critical information in filenames
# Specifically designed for CNES dump/csv
# Yes, I know that's not good

bulk_data_loader <- function(files, header){

    l <- length(header)

    # Load each file to list
    tmp <- list()
    for(f in files){
        competence <- unlist(strsplit(f, '[._]'))[4]
        tmp[[competence]] <- fread(f,
                                   sep='|',
                                   integer64='character',
                                   header=FALSE,
                                   stringsAsFactor=FALSE)



    }

    # Join everything
    tmp <- rbindlist(tmp)

    # Count how many (un)named columns
    l <- length(header)
    m <- length(names(tmp))

    # Add headers
    setnames(tmp, c(header, paste0('V', (l + 1) : m)))

    return(tmp[, 1:l, with=F])

}

# Auxiliary functions

# Count counts per level
charter <- function(idx, val){

    tmp <- tapply(idx, val, length)
    tapply(names(tmp), as.vector(tmp), length)

}


