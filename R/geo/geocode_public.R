##############################
# A No Cost Geocode Strategy Using R
##############################
# Neal D. Goldstein
# Amy H. Auchincloss
# Brian K. Lee
# Department of Epidemiology and Biostatistics
# Drexel University School of Public Health
# Philadelphia, PA
##############################
# DISCLAIMER of USE
# The authors make no guarantee of the accuracy of the results
# returned from this utility. The code below was verified as
# working as of 4/27/2014.
##############################
# LICENSE, ATTRIBUTION and TERMS of USE
# This work is licensed under the Creative Commons License: Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0)
# Full license details are available at: http://creativecommons.org/licenses/by-nc/3.0/legalcode
# This work is modified from the original code authored by JoFrhwld and Tony Breyal: http://stackoverflow.com/users/159438/jofrhwld, http://stackoverflow.com/users/300123/tony-breyal
# Original code is available from Stack Overflow: http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
# By use of this utility, users agree to be bound by Google's Term's of Use: https://developers.google.com/maps/terms
# Google's privacy policy is available at: http://www.google.com/privacy.html
##############################

#paste your Google Places API key below (not needed if using Google Maps API); Google API signup available at https://code.google.com/apis/console/
APIkey <- "111222333444555aaabbbcccdddeee"

#RCurl and RJSONIO are required libraries for the connection to the API 
library(RCurl)
library(RJSONIO)

##############################
# Function: gGeoCode
# Use: sends an http request to the appropriate Google API for geocoding
# Argument(s):
#    1) vector of addresses to be geocoded (required)
#    2) Google API (maps or places) for query (optional, default=places)
#    3) verbose boolean to print each address during the geocode (optional, default=FALSE)
#    4) accuracy boolean for maps API (optional, default=FALSE); informs whether the geocode was approximate, interpolated or exact, see https://developers.google.com/maps/documentation/geocoding/#Results
#    5) partial match boolean for maps API (optional, default=FALSE); informs whether the address was completely or partially matched, see https://developers.google.com/maps/documentation/geocoding/#Results
# Return: data frame containing latitude(s), longitude(s), the formatted address(es) used by Google for the actual geocoding, plus any optional components (see arguments)
##############################
gGeoCode <- function(address,api="places",verbose=FALSE,accuracy=FALSE,partmatch=FALSE)
{
  #create empty data frame to store results
  resultDF <- NULL
  
  for (i in 1:length(address))
  {
    #print the parameters in verbose mode
    if (verbose==TRUE)
      cat("\ngGeoCode called with parameters\n-------------------------------\nRaw address:",address[i],"\nAPI: ",api,"\nVerbose:",verbose,"\nAccuracy:",accuracy,"\nPartial match:",partmatch,"\n-------------------------------\n\n")
  
    #create the geocode URL, dependent on the API
    if (tolower(api)=="places")
    {
      root <- "https://maps.googleapis.com/maps/api/place/textsearch/"
      sensor <- "false"
      u <- URLencode(paste(root, "json", "?query=", address[i], "&sensor=", sensor, "&key=", APIkey, sep = ""))
    }
    else if (tolower(api)=="maps")
    {
      root <- "http://maps.google.com/maps/api/geocode/"
      sensor <- "false"
      u <- URLencode(paste(root, "json", "?address=", address[i], "&sensor=", sensor, sep = ""))    
    }
    else
    {
      cat("\nUnrecognized API: ", api, "\nValid APIs are: maps, places\n")
      stop()
    }
    
    #connect to the geocode URL and retrieve results
    doc <- getURL(u, ssl.verifypeer = FALSE)
    
    #de-serialize the JSON object to an R object
    x <- fromJSON(doc,simplify = FALSE)
    
    #check the status of the geocode
    if(x$status=="OK")
    {
      #geocode successful, parse results
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      formatted_address <- x$results[[1]]$formatted_address
      
      #check for accuracy option for maps API
      if (tolower(api)=="maps" && accuracy==TRUE)
        loc_type <- x$results[[1]]$geometry$location_type
      else
        loc_type <- NA
      
      #check for partial match option for maps API
      if (tolower(api)=="maps" && partmatch==TRUE && !is.null(x$results[[1]]$partial_match))
        part_match <- x$results[[1]]$partial_match
      else
        part_match <- NA
      
      #return a vector containing latitude, longitude, the formatted address used by Google for the actual geocoding, plus any optional components (see arguments)
      resultDF <- rbind(resultDF,c(lat, lng, formatted_address, loc_type, part_match))
    }
    else
    {
      #geocode failed  
      resultDF <- rbind(resultDF,c(NA,NA,NA,NA,NA))
    }
    
    #sleep 1/10 of a second between requests per Google's terms of use
    Sys.sleep(.1)
  }
  
  #coerce to data frame, set column names, and return
  resultDF <- as.data.frame(resultDF)
  colnames(resultDF) <- c("Latitude","Longitude","Formatted_Address","Accuracy","Partial_Match")
  return(resultDF)
}

#example geocodes
exAddresses = c("400 N Broad St, Philadelphia, PA",
                "400 N Broad, Philly",
                "1503RACESTREET PHILADELPHIA",
                "Y-HEP",
                "St. Christopher's Hospital for Children")

#try default places api 
gGeoCode(exAddresses)

#now try maps api w/ options
gGeoCode(exAddresses, api="maps", accuracy=TRUE, partmatch=TRUE)

