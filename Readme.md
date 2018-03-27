##Download your EVE wallet transaction data and export as CSV. Look at some cute graphs while your at it.
##For an example how it looks, visit https://srsbsns.shinyapps.io/wallet_transactions_dl/ 
##This can also be run locally. You will need to have RStudio installed: https://www.rstudio.com
##You will also need to register an app with CCP on their developer site: https://developers.eveonline.com/ 
##For this app, you will need to set the scope to "esi-wallet.read_character_wallet.v1"
##Once you have it registered you can put your app ID ("client id"), its secret and its callback URL into the config.R file

##To start the app, 
1) double click "wallet_transactions_dl.Rproj", or open RStudio and open the project if you don't have the file association activated
2) enter these 2 commands in the RStudio Console (at the bottom of the window):

library(shiny)
runApp(appDir = getwd(), port = 4355, launch.browser = T, host = getOption("shiny.host", "127.0.0.1"), display.mode = "normal")

3) If your browser does not open automatically, type in this address: http://127.0.0.1:4355

NB: If you run the app with the "Run App" button, the port will be randomly assigned and the CCP auth server cannot send your credentials back to right address (my example uses http://localhost:4355, but you can use a different port).


##Reference documents:
Reference for the ESI API if you want to call other things than just the wallet transactions: https://esi.tech.ccp.is/latest/
Example of how the authentication calls work if you want to understand what is going on: https://developers.eveonline.com/blog/article/sso-to-authenticated-calls
Shiny reference: https://shiny.rstudio.com/reference/shiny/
R reference: https://www.r-project.org/other-docs.html
GGplot2 reference for awesome graphs: http://ggplot2.tidyverse.org/reference/