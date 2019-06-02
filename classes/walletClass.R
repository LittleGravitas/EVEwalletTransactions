getWalletTransactions <- function(buildAuthcode, CharacterID){
  requestPath <- paste(c("latest/characters/", CharacterID, "/wallet/transactions/"), collapse = "")
  walletTransactionsRequest <- GET("https://esi.evetech.net", path=requestPath, add_headers(Authorization = buildAuthcode))
  stop_for_status(walletTransactionsRequest)
  walletTransactions <- content(walletTransactionsRequest, "parsed", "application/json")
  if (length(walletTransactions) == 0) {
    walletTransactions = "no transactions"
  }
  
  return(walletTransactions)
  
}

getWalletJournal <- function(buildAuthcode, CharacterID){
  requestPath <- paste(c("latest/characters/", CharacterID, "/wallet/journal/"), collapse = "")
  walletJournalRequest <- tryCatch({
    getRequest <- GET("https://esi.evetech.net", path=requestPath, add_headers(Authorization = buildAuthcode))
    stop_for_status(getRequest)
    walletJournal <- content(walletJournalRequest, "parsed", "application/json")
    walletJournal
  },
  error = function (e){
    walletJournal = "no journal"
    return(walletJournal)
  } 
  )
  
  return(walletJournal)
  
}