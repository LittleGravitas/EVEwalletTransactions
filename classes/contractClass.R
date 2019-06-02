getContracts <- function(buildAuthcode, CharacterID){
  requestPath <- paste(c("latest/characters/", CharacterID, "/contracts/"), collapse = "")
  contractRequest <- GET("https://esi.evetech.net", path=requestPath, add_headers(Authorization = buildAuthcode))
  stop_for_status(contractRequest)
  contracts <- content(contractRequest, "parsed", "application/json")
  
  return(contracts)
  
}

getItemsFromContracts <- function(buildAuthcode, CharacterID, contractID) {
  requestPath <- paste(c("latest/characters/", CharacterID, "/contracts/", contractID, "/items/"), collapse = "")
  itemRequest <- GET("https://esi.evetech.net", path=requestPath, add_headers(Authorization = buildAuthcode))
  stop_for_status(itemRequest)
  items <- content(itemRequest, "parsed", "application/json")
  
  return(items)
}

extractPublicContracts <- function(contractscsvtable) {
  #contractsDataFrame <- read.csv(contractscsvtable, header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
  contractsDataFrame <- contractscsvtable
  contractsDataFrame <- contractsDataFrame[(contractsDataFrame$availability=="public" & contractsDataFrame$status=="finished"),]
  return(contractsDataFrame)
}

resolveContractItems <- function(TokenNew, CharacterID, publicContracts) {
  buildAuthcode <- TokenNew$buildAuthcode
  refreshcode <- TokenNew$refreshcode
  
  for (i in 1:nrow(publicContracts)) {
    tempRestponse <- getItemsFromContracts(buildAuthcode, CharacterID, publicContracts[i, "contract_id"])
    tempList <- rbindlist(tempRestponse, fill = T)
    
    if (tempRestponse[[1]]$quantity == 1) {
      publicContracts[i,"item_id"] <- tempList$type_id
    } else {
      publicContracts[i,"item_id"] <- 0
    }
    
    #prepare new token
    TokenNew <- getTokenFromRefresh(refreshcode)
    authcode <- TokenNew$authcode
    refreshcode <- TokenNew$refreshcode
    buildAuthcode <- TokenNew$buildAuthcode
  }
  
  return(list(publicContracts, TokenNew))
  
  #joinedContractsItems <- sqldf('SELECT csvtable.date, csvtable.transaction_id, csvtable.quantity, "typeName", csvtable.type_id, csvtable.unit_price, csvtable.client_id, "client_name", csvtable.location_id, "stationName", csvtable.is_buy, csvtable.is_personal, csvtable.journal_ref_id FROM csvtable INNER JOIN invTable ON csvtable.type_id="typeID"')
}