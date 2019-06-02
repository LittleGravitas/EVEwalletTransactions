getCharacterID <- function(buildAuthcode){
  charInfoRequest <- tryCatch({
    getRequest<-GET("https://esi.evetech.net/verify/", add_headers(Authorization = buildAuthcode))
    stop_for_status(getRequest)
    charInfoParsed <- content(getRequest, "parsed", "application/json")
    charID <- charInfoParsed$CharacterID
    charID
  },
  error = function (e){
    charID = "error: failed to identify with login server"
    return(charID)
  } 
  )
  return(charInfoRequest)
}

getCharacterName <- function(charID){
  receivedName <- GET("https://esi.evetech.net", path=paste0("/latest/characters/",charID,"/?datasource=tranquility", collapse=""))
  namefromresponse <- content(receivedName, "parsed", "application/json")
  return(namefromresponse[['name']])
}

getCharacterImage <- function(charID){
  receivedPaths <- GET("https://esi.evetech.net", path=paste0("/latest/characters/",charID,"/portrait/?datasource=tranquility", collapse=""))
  pathsfromresponse <- content(receivedPaths, "parsed", "application/json")
  return(pathsfromresponse[["px64x64"]])
}