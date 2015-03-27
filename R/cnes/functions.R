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
                                   colClasses='character',
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

validateCPF <- function(cpf){

    # Remove non-numeric chars
    cpf <- gsub('[^0-9]', '', cpf)

    # Adjust size with leading zeroes
    # Invalidate strings greater than 11
    if(cpf %in% c(NA, '')) return(NA)

    l <- nchar(cpf)
    if(l < 11) cpf <- paste0(rep('0', 11 - l), cpf)
    if(l > 11) return(NA)

    # Can't have only one type of digit
    tmp <- as.integer(unlist(strsplit(cpf, '')))
    if(length(as.set(tmp)) < 2) return(NA)
    
    # Compute verification digits
    first <- (sum(1:9 * tmp[1:9]) %% 11) %% 10
    second <- ((sum(1:8 * tmp[2:9]) + 9 * first) %% 11) %% 10
    
    # Validate and return the string as integer
    if(first == tmp[10] & second == tmp[11]) {

        return(cpf)
        
    } else {
        
        return(NA)

    }

}

validateCNPJ <- function(cnpj){

    # Remove non-numeric chars
    cnpj <- gsub('[^0-9]', '', cnpj)

    # Adjust size with leading zeroes
    # Invalidate strings greater than 11
    l <- nchar(cnpj)
    if(l < 14) cnpj <- paste0(rep('0', 14 - l), cnpj)
    if(l > 14) return(NA)

    # Can't have only one type of digit
    tmp <- as.integer(unlist(strsplit(cnpj, '')))
    if(length(as.set(tmp)) < 2) return(NA)

    # Compute verification digits
    first <- (sum(1:9 * tmp[1:9]) %% 11) %% 10
    second <- ((sum(1:8 * tmp[2:9]) + 9 * first) %% 11) %% 10
    
    # Validate and return the string as integer
    if(first == tmp[13] & second == tmp[14]) {

        return(as.integer(cnpj))
    
    } else {

        return(NA)

    }

}

scientific10 <- function(x){

    v <- gsub('e', ' %*% 10^', scientific_format()(x))
    v <- gsub('[+]', '', v)
    parse(text=v)
    
}

