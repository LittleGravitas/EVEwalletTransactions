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