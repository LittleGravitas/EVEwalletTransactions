#Download your EVE wallet transaction data and export as CSV. 
######Look at some cute graphs while your at it.
_If you want to see it working online visit: https://srsbsns.shinyapps.io/wallet_transactions_dl/ _

##How to set up your local app
1. You will need to have *RStudio* installed: https://www.rstudio.com
2. Download this project to your computer
3. You will need to *register an app* with CCP on their developer site: https://developers.eveonline.com/ 
4. *Log in* with your EVE account, click *Manage Applications*
5. Click *create new application*
6. Set a name, a description, set connection type to *Authentication & API Access*
7. You will need to set the permission from the list of available *scopes* to "esi-wallet.read_character_wallet.v1"
7. Set the *callback URL* to http://127.0.0.1:4355 (any free port suffices, but you need to specify a port)
8. Finish by *Create Application*
4. Once you have it registered, copy your *app ID (aka "client id")*, its *secret* and its *callback URL* into the *config.R* file that is part of the project you downloaded in step 2

##To start the app, 
1. double click "wallet_transactions_dl.Rproj", or open RStudio and open the project if you don't have the file association activated
2. enter these 2 commands in the RStudio Console (at the bottom of the window):
3. Install the *packages* by copying this line into the command line and hitting enter:
... install.packages(c("shiny", "httr", "rjson", "ggplot2", "data.table", "sqldf", "openssl"))
4. To start the shiny server and run the app, copy these two lines into the command line:
...library(shiny)
...runApp(appDir = getwd(), port = 4355, launch.browser = T, host = getOption("shiny.host", "127.0.0.1"), display.mode = "normal")
3. If your browser does not open automatically, type in this address: http://127.0.0.1:4355

_NB: If you run the app with the "Run App" button, the port will be randomly assigned and the CCP auth server cannot send your credentials back to right address (my example uses http://localhost:4355, but you can use a different port; just make sure to update config.R and the callback URL on https://developers.eveonline.com/)._


##Reference documents:
Reference for the ESI API if you want to call other things than just the wallet transactions: https://esi.tech.ccp.is/latest/
Example of how the authentication calls work if you want to understand what is going on: https://developers.eveonline.com/blog/article/sso-to-authenticated-calls
Shiny reference: https://shiny.rstudio.com/reference/shiny/
R reference: https://www.r-project.org/other-docs.html
GGplot2 reference for awesome graphs: http://ggplot2.tidyverse.org/reference/