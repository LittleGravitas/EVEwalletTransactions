##########Download your EVE wallet transaction data and export as CSV. Look at some cute graphs while your at it.

##To start the app, 
1) double click "wallet_transactions_dl.Rproj"
2) enter these 2 commands in the RStudio Console at the bottom of the window:

library(shiny)
runApp(appDir = getwd(), port = 4355, launch.browser = T, host = getOption("shiny.host", "127.0.0.1"), display.mode = "normal")

3) If your browser does not open automatically, type in this address: http://127.0.0.1:4355

If you run the app with the "Run App" button, the port will be randomly assigned and the CCP auth server cannot send your credentials back to right address (it is fixed to http://127.0.0.1:4355 within the CCP developer config things).

##NB! Feel free to experiment with the code, but please don't spam the thing with bull shit requests that lead to it getting shut down.

##Reference documents:
Reference for the ESI API: https://esi.tech.ccp.is/latest/
Example of how the authentication calls work: https://developers.eveonline.com/blog/article/sso-to-authenticated-calls
Shiny reference: https://shiny.rstudio.com/reference/shiny/
R reference: https://www.r-project.org/other-docs.html
GGplot2 reference for awesome graphs: http://ggplot2.tidyverse.org/reference/