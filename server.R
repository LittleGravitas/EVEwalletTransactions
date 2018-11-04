

# To start the app, you have to enter these 2 commands in the Console below:
#
# library(shiny)
# runApp(appDir = getwd(), port = 4355, launch.browser = F, host = getOption("shiny.host", "127.0.0.1"), display.mode = "normal")
#
# If you run it instead with the "Run App" button, the port will be randomly assigned and the CCP auth server cannot send your credentials back to right address.
# If your browser does not open automatically, type in this address: http://127.0.0.1:4355



library(shiny)
library(httr)
library("rjson")
library(data.table)
library(sqldf)
library(ggplot2)
library(openssl)
source("config.R")
source("dark_spaceTheme.R")
theme_set(theme_space())

options(scipen = 999)

function(input, output, session) {


  getTokenNew <- function(loginCode){
    loginCode <- toJSON(loginCode)
    buildPostBody <- paste('{"grant_type": "authorization_code","code":', loginCode, '}')
    r <- POST("https://login.eveonline.com/oauth/token", body = buildPostBody,
              add_headers(Authorization=paste(c("Basic ", base64_encode(charToRaw(paste(c(appClient_ID,":", appSecret), collapse = "")))), collapse = ""), `Content-Type`="application/json"))
    stop_for_status(r)
    rtojson <- content(r, "parsed", "application/json")
    
    authcode <- rtojson$access_token
    refreshcode <- toJSON(rtojson$refresh_token)
    buildAuthcode <- paste(c("Bearer", authcode), collapse = " ")
    returnList <- list("authcode" = authcode, "refreshcode" = refreshcode, "buildAuthcode" = buildAuthcode)
    return(returnList)
  
  }
  
  getTokenFromRefresh <- function(refreshcode){
    buildPostBody <- paste('{"grant_type": "refresh_token","refresh_token":', refreshcode, '}')
    r <- POST("https://login.eveonline.com/oauth/token", body = buildPostBody,
              add_headers(Authorization=paste(c("Basic ", base64_encode(charToRaw(paste(c(appClient_ID,":", appSecret), collapse = "")))), collapse = ""), `Content-Type`="application/json"))
    stop_for_status(r)
    rtojson <- content(r, "parsed", "application/json")
    
    authcode <- rtojson$access_token
    refreshcode <- toJSON(rtojson$refresh_token)
    buildAuthcode <- paste(c("Bearer", authcode), collapse = " ")
    returnList <- list("authcode" = authcode, "refreshcode" = refreshcode, "buildAuthcode" = buildAuthcode)
    return(returnList)
    
  }
  
  
  
  getCharacterID <- function(buildAuthcode){
    charInfoRequest <- tryCatch({
    getRequest<-GET("https://esi.tech.ccp.is/verify/", add_headers(Authorization = buildAuthcode))
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
    receivedPaths <- GET("https://esi.tech.ccp.is", path=paste0("/latest/characters/",charID,"/portrait/?datasource=tranquility", collapse=""))
    pathsfromresponse <- content(receivedPaths, "parsed", "application/json")
    return(pathsfromresponse[["px64x64"]])
  }
  
  getWalletTransactions <- function(buildAuthcode, CharacterID){
    requestPath <- paste(c("latest/characters/", CharacterID, "/wallet/transactions/"), collapse = "")
    walletTransactionsRequest <- GET("https://esi.tech.ccp.is", path=requestPath, add_headers(Authorization = buildAuthcode))
    stop_for_status(walletTransactionsRequest)
    walletTransactions <- content(walletTransactionsRequest, "parsed", "application/json")
    
    return(walletTransactions)
    
  }
  
  getWalletJournal <- function(buildAuthcode, CharacterID){
    requestPath <- paste(c("latest/characters/", CharacterID, "/wallet/journal/"), collapse = "")
    walletJournalRequest <- GET("https://esi.tech.ccp.is", path=requestPath, add_headers(Authorization = buildAuthcode))
    stop_for_status(walletJournalRequest)
    walletJournal <- content(walletJournalRequest, "parsed", "application/json")
    
    return(walletJournal)
    
  }
  
  getContracts <- function(buildAuthcode, CharacterID){
    requestPath <- paste(c("latest/characters/", CharacterID, "/contracts/"), collapse = "")
    contractRequest <- GET("https://esi.tech.ccp.is", path=requestPath, add_headers(Authorization = buildAuthcode))
    stop_for_status(contractRequest)
    contracts <- content(contractRequest, "parsed", "application/json")
    
    return(contracts)
    
  }
  
  getItemsFromContracts <- function(buildAuthcode, CharacterID, contractID) {
    requestPath <- paste(c("latest/characters/", CharacterID, "/contracts/", contractID, "/items/"), collapse = "")
    itemRequest <- GET("https://esi.tech.ccp.is", path=requestPath, add_headers(Authorization = buildAuthcode))
    stop_for_status(itemRequest)
    items <- content(itemRequest, "parsed", "application/json")
    
    return(items)
  }
  
  
    
  
  netfunction <- function(itemType) {
    tbl <- characterObj()[["transactRich"]]
    
    subbuy <- subset(tbl, (tbl[["typeName"]] == itemType & tbl[["is_buy"]]==TRUE) )
    subsell <- subset(tbl, (tbl[["typeName"]] == itemType & tbl[["is_buy"]]==FALSE) )
    subbuy <- sum(subbuy[["unit_price"]] * subbuy[["quantity"]])*-1
    subsell <- sum(subsell[["unit_price"]] * subsell[["quantity"]])
    return(subbuy+subsell) 
  }
  
  replaceBoolWithFactors <- function(tbl){
    
    tbl$is_buy[tbl$is_buy == TRUE] <- -1
    tbl$is_buy[tbl$is_buy == FALSE] <- 1
    
    return(tbl)
  }
  
  rewriteSellBuyEntries <- function(joinedCSVtables){
    tbl <- joinedCSVtables
    tbl$is_buy[tbl$is_buy == TRUE] <- "buy"
    tbl$is_buy[tbl$is_buy == FALSE] <- "sell"
    
    return(tbl)
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

  
  downloadedData <- reactive({
    parseQueryString(session$clientData$url_search)
                     })
  
  retrieveData <- function (){
    
    if (!is.null(downloadedData()[['code']])){
      getResponse <- downloadedData()[['code']]
      
      #get token from login auth
      TokenNew <- getTokenNew(getResponse)
      authcode <- TokenNew$authcode
      refreshcode <- TokenNew$refreshcode
      buildAuthcode <- TokenNew$buildAuthcode
      
      #use token to ask login server for character ID
      CharacterID <- getCharacterID(buildAuthcode)
      #use ID to ask for character name (no token needed)
      CharacterName <- getCharacterName(CharacterID)
      #use ID to ask for character image path (no token needed)
      CharacterImage <- getCharacterImage(CharacterID)
      
      #prepare new token
      TokenNew <- getTokenFromRefresh(refreshcode)
      authcode <- TokenNew$authcode
      refreshcode <- TokenNew$refreshcode
      buildAuthcode <- TokenNew$buildAuthcode
      
      #use token to call transactions
      WalletTransactions <- getWalletTransactions(buildAuthcode, CharacterID)
      
      #prepare new token
      TokenNew <- getTokenFromRefresh(refreshcode)
      authcode <- TokenNew$authcode
      refreshcode <- TokenNew$refreshcode
      buildAuthcode <- TokenNew$buildAuthcode
      
      #use token to call contracts
      Contracts <- getContracts(buildAuthcode, CharacterID)
      
      #prepare new token
      TokenNew <- getTokenFromRefresh(refreshcode)
      authcode <- TokenNew$authcode
      refreshcode <- TokenNew$refreshcode
      buildAuthcode <- TokenNew$buildAuthcode
      
      csvtable <- rbindlist(WalletTransactions, fill=T)
      
      contractscsvtable <- rbindlist(Contracts, fill=T)
      publicContracts <- extractPublicContracts(contractscsvtable)
      
      invTable <- read.csv2(file=file.path("www", "invTypes.csv"), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
      
      joinedCSVtables <- sqldf('SELECT csvtable.date, csvtable.transaction_id, csvtable.quantity, "typeName", csvtable.type_id, csvtable.unit_price, csvtable.client_id, "client_name", csvtable.location_id, "stationName", csvtable.is_buy, csvtable.is_personal, csvtable.journal_ref_id FROM csvtable INNER JOIN invTable ON csvtable.type_id="typeID"')
      oldFormatDL <- rewriteSellBuyEntries(joinedCSVtables)
      oldFormatDL$date <- gsub("T", " ", oldFormatDL$date)
      oldFormatDL$date <- gsub("Z", "", oldFormatDL$date)
      
      if (nrow(publicContracts) != 0) {
        getItems <- resolveContractItems(TokenNew, CharacterID, publicContracts)
        publicContractsItems <- getItems[[1]]
        
        #prepare new token
        TokenNew <- getItems[[2]]
        authcode <- TokenNew$authcode
        refreshcode <- TokenNew$refreshcode
        buildAuthcode <- TokenNew$buildAuthcode
      
        joinedContractItems <- sqldf('SELECT publicContractsItems.acceptor_id, publicContractsItems.assignee_id, publicContractsItems.availability, publicContractsItems.collateral, publicContractsItems.contract_id, publicContractsItems.date_accepted, publicContractsItems.date_completed, publicContractsItems.date_expired, publicContractsItems.date_issued, publicContractsItems.days_to_complete, publicContractsItems.end_location_id, publicContractsItems.for_corporation, publicContractsItems.issuer_corporation_id, publicContractsItems.issuer_id, publicContractsItems.price, publicContractsItems.reward, publicContractsItems.start_location_id, publicContractsItems.status, publicContractsItems.title, publicContractsItems.type, publicContractsItems.volume, publicContractsItems.item_id, "typeName" FROM publicContractsItems INNER JOIN invTable ON publicContractsItems.item_id="typeID"')
        joinedContractItems$date_completed <- gsub("T", " ", joinedContractItems$date_completed)
        joinedContractItems$date_completed <- gsub("Z", "", joinedContractItems$date_completed)
        
        if (sum(joinedContractItems$item_id) > 0) {
          joinedContractItems <- joinedContractItems[(joinedContractItems$item_id!=0),]
        } else {
          joinedContractItems <- "cannot resolve only multi item contracts"
        }
        
      } else {
        joinedContractItems = "no public contracts"
      }
      
      characterObj <- list(transactRich = joinedCSVtables, transactSlim = csvtable, transactOld = oldFormatDL, contractsSlim = joinedContractItems, ID = CharacterID, name = CharacterName, portrait=CharacterImage, refreshToken = refreshcode)
      
    }else{
      joinedCSVtables <- read.csv2(file=file.path("www", "bigtestsheet.csv"), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
      CharacterID = 7777
      CharacterName = "Sample Capsuleer"
      refreshcode = "77iSk77ok77isTruth"
      silhouette = "https://image.eveonline.com/Character/1_64.jpg"
      characterObj <- list(transactRich = joinedCSVtables, transactSlim = "", transactOld = joinedCSVtables, contractsSlim = "", ID = CharacterID, name = CharacterName, portrait=silhouette, refreshToken = refreshcode)
    }
    return(characterObj)
  }
  
  characterObj <- reactive(
    {
      retrieveData()
    }
  )
  

  
  # --download buttons --
  #old format
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    
    content = function(file) {
      write.table(characterObj()[["transactOld"]], file, sep=",", col.names=input$headers, row.names=F)
    }
  )
  #new format
  output$downloadSlim <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.table(characterObj()[["transactSlim"]], file, sep=",", col.names=input$headers, row.names=F)
    }
  )
  
  #contracts
  output$downloadContracts <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.table(characterObj()[["contractsSlim"]], file, sep=",", col.names=input$headers, row.names=F)
    }
  )
  
  
  # dropdown for net calculation
  output$dropdown = renderUI({
    
    
    selectInput("dynamic", "Select item for buy/sell net calculation",
                 choices = sort(unique(characterObj()[["transactRich"]][["typeName"]])),
                 multiple = F,
                 selectize=FALSE
  )})
  
  output$slider = renderUI({
    sliderInput("topX", "Move visible transactions:",
                min = 0, max = length(unique(characterObj()[["transactRich"]][["typeName"]])), value = 0
    )
  })
  

  # character Name
  output$characterName <- renderText({
    paste(characterObj()[["name"]])
  })
  
  #character ID
  output$charID <- renderText({
    paste(characterObj()[["ID"]])
  })
  
  #character portrait
  output$portrait <- renderText({
    c('<img src="',characterObj()[["portrait"]],'">')
  })
  
  # current refresh token
  output$refreshToken <- renderText({
    paste(characterObj()[["refreshToken"]])
  })
  
  output$distPlot <- renderPlot({
    tbl <- replaceBoolWithFactors(characterObj()[["transactRich"]])
    price <- tbl[["unit_price"]] * tbl[["is_buy"]] * tbl[["quantity"]]
    itemNames <- tbl[["typeName"]]
    
    pred <- data.frame(price,itemNames)
    data <- aggregate(price ~., data = pred, sum)
    
    data$colour <- ifelse(data$price < 0, "negative","positive")
    
    ggplot(data)+aes(x= reorder(itemNames,-price), price, label=format(price, big.mark=","))+
      geom_bar(stat="identity", position="dodge",aes(fill = colour))+
      scale_fill_manual(values=c(positive="#2A9FD6",negative="#DF691A"))+
      scale_y_continuous(labels = function(x) format(x, big.mark=","))+
      geom_label()+
      coord_cartesian(xlim=c(input$topX, input$topX+5))+
      labs(x="Item", y="ISK")+ 
      ggtitle("ISK value per item type")
    
    
  })
  
  output$timeline <- renderPlot({
    tbl <- characterObj()[["transactRich"]]
    reformatDates <- strptime(characterObj()[["transactRich"]][["date"]], format="%Y-%m-%dT%H:%M:%SZ")
    tbl$hour <- reformatDates$hour
    
    ggplot(tbl)+aes(as.Date(date, format="%Y-%m-%dT%H:%M:%SZ"), unit_price, color=hour)+
      geom_point(stat="identity", size=5)+
      scale_y_continuous(labels = function(x) format(x, big.mark=","))+
      scale_color_gradient2(low="#2A9FD6", high="#2A9FD6", mid="#DF691A", midpoint=19, guide = "legend", space = "Lab")+
      labs(x="Date", y="ISK")+ 
      ggtitle("Trade value by date and time")
      
  })
  
  output$skilltradingPlot <- renderPlot({
    tbl <- characterObj()[["transactRich"]]
    subtbl <- subset(tbl, tbl[["typeName"]] == "Skill Extractor" | tbl[["typeName"]] == "Large Skill Injector" | tbl[["typeName"]] == "Small Skill Injector" | tbl[["typeName"]] == "PLEX" | tbl[["typeName"]] == "Multiple Pilot Training Certificate")
    subtbl <- replaceBoolWithFactors(subtbl)
    subtbl$weighted <- subtbl[["unit_price"]] * subtbl[["is_buy"]] * subtbl[["quantity"]]
    subtbl$factored <- subtbl[["unit_price"]] * subtbl[["is_buy"]]
    
    
    ggplot(subtbl, aes(as.Date(date, format="%Y-%m-%dT%H:%M:%SZ"), factored))+
      geom_line(aes(colour = typeName))+
      geom_point(aes(colour = typeName))+
      scale_y_continuous(labels = function(x) format(x, big.mark=","))+
      scale_colour_manual(values=c("#2A9FD6","#DF691A","#07f251","#f207ee"))+
      labs(x="Date", y="ISK")
  })
  
  output$skillTradingNet <- renderPlot({
    tbl <- characterObj()[["transactRich"]]
    subtblHarvest <- subset(tbl, tbl[["typeName"]] == "Skill Extractor" | tbl[["typeName"]] == "Large Skill Injector" | tbl[["typeName"]] == "Small Skill Injector")
    subtblSowing <- subset(tbl, tbl[["typeName"]] == "PLEX" | tbl[["typeName"]] == "Multiple Pilot Training Certificate")
    subtblHarvest <- replaceBoolWithFactors(subtblHarvest)
    subtblSowing <- replaceBoolWithFactors(subtblSowing)
    
    netHarvest <- sum(subtblHarvest[["unit_price"]] * subtblHarvest[["quantity"]])
    netSowing <- sum(subtblSowing[["unit_price"]] * subtblSowing[["quantity"]])
    legend <- c("ISK sink", "ISK harvest")
    netValues <- c(netSowing, netHarvest)
    
    netdf <- data.frame(legend, netValues)
    
    ggplot(netdf, aes(x= legend, y=netValues, label=format(netValues, big.mark=",")))+
      geom_bar(stat="identity", aes(fill=legend))+
      geom_label()+
      scale_y_continuous(labels = function(x) format(x, big.mark=","))+
      scale_fill_manual(values=c("#2A9FD6","#DF691A"))+
      labs(x="", y="ISK")
  })
  
  output$arbitrage <- renderPlot({
    tbl <- characterObj()[["transactRich"]]
    focussedItem <- subset(tbl, tbl[["typeName"]] == input$dynamic)
    focussedItemSell <- subset(focussedItem, focussedItem[["is_buy"]] == FALSE)
    focussedItemBuy <- subset(focussedItem, focussedItem[["is_buy"]] == TRUE)
    focussedItemSell$unit_price <- focussedItemSell$unit_price * focussedItemSell$quantity
    focussedItemBuy$unit_price <- focussedItemBuy$unit_price * focussedItemBuy$quantity * -1
    
    ggplot(focussedItemSell, aes(as.Date(date, format="%Y-%m-%dT%H:%M:%SZ"), unit_price))+
      geom_point(colour="green")+
      geom_point(data=focussedItemBuy, aes(as.Date(date, format="%Y-%m-%dT%H:%M:%SZ"), unit_price), colour="red")+
      geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)+
      scale_y_continuous(labels = function(x) format(x, big.mark=","))+
      labs(x="Date", y="ISK")
      
  })
  
  output$table <- renderTable({
    characterObj()[["transactOld"]]
    })
  
  output$tradenet <- renderText({
    paste("Selected item (", input$dynamic ,") has a sell/buy net of ", format(netfunction(input$dynamic), big.mark=","), " ISK")
    
  })

}
