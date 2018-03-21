library(shiny)

# UI
fluidPage(
  tags$head(tags$style("
                  #container * {  
                       display: inline;
                       }")),

  # Application title
  titlePanel("Download Wallet Transactions!"),

  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      img(src='EVE_SSO_Login_Buttons_Small_White.png', align = "left", onclick="location.href='https://login.eveonline.com/oauth/authorize?response_type=code&redirect_uri=http://localhost:4355&client_id=6bc0906a89d449a8b7022ade05770c66&scope=esi-wallet.read_character_wallet.v1'"),
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
        tabPanel("General Plots", plotOutput("distPlot"),  sliderInput("topX", "Move visible transactions:",
           min = 0, max = 100, value = 0
        ), plotOutput("timeline")),
        tabPanel("Table", tableOutput("table")),
        tabPanel("Skill trading", plotOutput("skilltradingPlot"), plotOutput("skillTradingNet")),
        tabPanel("Arbitrage", plotOutput("arbitrage"), uiOutput("dropdown"), textOutput("tradenet"))
      )
      
    )
  )
)
