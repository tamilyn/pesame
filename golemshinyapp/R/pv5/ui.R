
hdr <- dashboardHeader(title = "PESAME")

side <- dashboardSidebar(
    sidebarMenu(
        #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Load", icon = icon("th"), tabName = "load"),
        menuItem("Describe Factors", icon = icon("th"), tabName = "factor"),
        menuItem("Analyze", icon = icon("th"), tabName = "analyze"),
        menuItem("Citation", icon = icon("th"), tabName = "citation")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard", h2("Dashboard tab content")),
        tabItem(tabName = "load", uiOutput("loadUI")),
        tabItem(tabName = "factor", uiOutput("factorUI")),
        tabItem(tabName = "analyze",  uiOutput("analyzeUI")),
        tabItem(tabName = "citation", uiOutput("citationUI"))))

dashboardPage(skin = "black", hdr, side, body)
