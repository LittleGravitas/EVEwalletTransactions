library(shiny)
source("config.R")

# UI
fluidPage(theme = "bootstrap.css",
  tags$head(tags$style("
                  #container * {  
                       display: inline;
                       }")),

  # Application title
  titlePanel("Download Wallet Transactions!"),

  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      img(src="EVE_SSO_Login_Buttons_Small_White.png", align = "left", onclick=paste(c("location.href='https://login.eveonline.com/oauth/authorize?response_type=code&redirect_uri=", cbURL, "&client_id=", appClient_ID,"&scope=esi-wallet.read_character_wallet.v1'"), collapse="")),
      tags$br(),
      tags$br(),
      downloadButton("downloadData", "Export to CSV (legacy)"),
      downloadButton("downloadSlim", "Export to CSV (new)"),
      checkboxInput("headers", "Include headers", FALSE),
      
      
      tags$hr(),
      htmlOutput("portrait"),
      div(id="container", tags$b("Name: "), textOutput("characterName")),
      div(id="container", tags$b("ID: "), textOutput("charID")),
      div(id="container", tags$b("Refresh token: "), textOutput("refreshToken"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("General Plots", plotOutput("distPlot"), uiOutput("slider"), plotOutput("timeline")),
        tabPanel("Table", tableOutput("table")),
        tabPanel("Skill trading", plotOutput("skilltradingPlot"), plotOutput("skillTradingNet")),
        tabPanel("Arbitrage", plotOutput("arbitrage"), uiOutput("dropdown"), textOutput("tradenet")),
        tabPanel("About", includeHTML("about.html"))
      )
      
    )
  )
)
