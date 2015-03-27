
library(rgdal)
library(rgeos)
library(maptools)

setScale(1e+15)

filenames <- gsub('[.]shp', '', Sys.glob('/health/IBGE/malhas_digitais/municipio_2010/Brasil/MUE/*.shp'))
basenames <- basename(filenames)

# Devel folder
devDir <- file.path('/home/jarretinha/dev/resbR')

# Static data folder, provisory location
refDir <- file.path(paste(devDir, 'static', sep='/'))

# Shapefiles folder
shapesDir <- '/health/IBGE/malhas_digitais/municipio_2010/Brasil/MUE/'

# Read regions data
regions <- read.table(file.path(paste(refDir, 'CIR_BR.tsv', sep='/')),
                      sep='\t',
                      header=TRUE,
                      stringsAsFactor=FALSE,
                      as.is=TRUE,
                      quote="")

# Convert codes from int to char
regions$CO_IBGE <- as.character(regions$CO_IBGE)
rownames(regions) <- regions$CO_IBGE

# Read all shapefiles to list
shp_list <- list()
for(f in basenames){

    shp_list[[f]] <- readOGR(shapesDir, f,
                             encoding='ISO-8859-1',
                             use_iconv=T,
                             stringsAsFactors=F)

}

# Correct overlapping IDs
shp_list <- lapply(shp_list, function(x) spChFIDs(x, x$CD_GEOCODM))

# Rbind all
shp <- do.call('rbind', shp_list)

# Clean up useless elements from map
#tmp <- c()
#for(p in shp@polygons){

#    tmp <- c(tmp, list(Polygons(Filter(function(x){x@ringDir == 1}, p@Polygons), ID=p@ID)))

#}

# Rebuild Polygons
#shp <- SpatialPolygons(tmp)

# Filter elements not shared
#shp <- shp[which(!shp$CO_IBGE %in% setdiff(shp$CO_IBGE, regions$CO_IBGE)),]

# Rebuild SPDF
#spdf <- data.frame(CO_IBGE=row.names(shp), row.names=row.names(shp))
#shp <- SpatialPolygonsDataFrame(shp, spdf)

# Sort identifiers
shp <- shp[order(sh$CD_GEOCODM), ]
regions <- regions[order(regions$CO_IBGE), ]

regions$CO_IBGE <- as.character(regions$CO_IBGE)

# Merge info from regions
shp@data <- data.frame(shp@data,
                       regions[match(shp@data[,'CD_GEOCODM'],regions[,'CO_IBGE']),])

# Set coordinates to WGS84 system
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(shp) <- p4s
shp <- spTransform(shp, p4s)

# Release memory
rm(shp_list, tmp)
gc()

# Create KML file
#writeOGR(shp,
#         dsn="Regiões e redes - Municípios - Brasil.kml",
#         layer= "Municípios",
#         driver="KML",
#         dataset_options=c("NameField=name"))

# Create ESRI Shapefile file
writeOGR(shp,
         dsn="Região e Redes - Municípios - Brasil.shp",
         layer= "Municípios",
         driver="ESRI Shapefile",
         dataset_options=c("NameField=name"))


# Plot experiments
# Initialize object
plt <- ggplot()

# Create map
plt <- + geom_map(data=shp@data,
                  aes(map_id=CO_IBGE, color='red'),
                  map=map)

# Set plot limits
plt <- plt + expand_limits(x=map$long, y=map$lat)

# Scale fill
plt <- plt + scale_fill_gradient(low='blue', high='red') + coord_map()

# Titles
plt <- plt + labs(x='', y='')
title <- paste0(occupation, '\nTeste')
plt <- plt + ggtitle(title)

# Theme/Style
plt <- plt + theme_bw()

# Themes/Panel
plt <- plt + theme(panel.grid.minor=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.border=element_blank())
# Theme/Axis
plt <- plt + theme(axis.ticks=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank())
# Theme/Plot
plt <- plt + theme(plot.margin=unit(c(0, 0, 0, 0), 'in'))
plt <- plt + theme(plot.title=element_text(face="bold", size=20))

