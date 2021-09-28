## Load libraries
library(shinydashboard)
library(readr)
library(dygraphs)
library(lubridate)
library(xts)

dashboardPage(skin = "yellow", title = "Badger Boost Analysis",
	dashboardHeader(title = tags$div(align="left", tags$img(src='badger.png'), "Badger Analysis")),

    dashboardSidebar(
		sidebarMenu(
			id = "tabs",
			# h4("Introduction", align="center"),
			# tags$div(
			#     "Badger Boost is a staking incentive", tags$br(),
			#     "program that scales the APY users get", tags$br(),
			#     "from depositing into one of the many", tags$br(),
			#     "Sett vaults hosted by the protocol.", tags$br(),
			#     "This bounty submission visualizes the", tags$br(), 
			#     "change in user staking ratios, native", tags$br(), 
			#     "asset and non-native asset holdings", tags$br(),
			#     "over time. For each graph, on the", tags$br(),
			#     "x-axis we have weekly dates ", tags$br(),
			#     "tracking the distribution of user", tags$br(),
			#     "deposits. On the various y-axes, the", tags$br(),
			#     "staking ratio, native assets and non-native assets value ranges are listed. Overall, from the beginning of tracking until the end, the Stake Ratio has increased, particularly around August 5th when the boosting program went live. At the beginning of the time period the Stake Ratio was ____ , ended at a value of ____. On average, during this period, the Stake Ratio was about ____. The ratio increased because the denominator, the “Native Asset Balance”, was higher during this period as Badger asset token holders increased their platform balances. At the start of this period the average native balance was ____. A few week after, the trend began to increase simultaneously pulling up the Stake Ratio. These simple graphs show that the boosting program was indeed a success and other similar incentive initiatives could possibly be met with even better results."
			# ),
			h4("Metrics Pre and Post Boost",align = "center"),
			menuItem("Assets Under Management", tabName = "aum", icon = icon("chart-line")),
			menuItem("Badger Token", tabName = "badger_tok", icon = icon("otter")),
			menuItem("DIGG Token", tabName = "digg_tok", icon = icon("digg")),
			hr(),
			menuItem("Micro Sett Statistics", tabName = "sett_view", icon = icon("piggy-bank")),
			uiOutput("sett_ui"),
			hr(),
			menuItem("Macro Sett Statistics", tabName = "stat_view", icon = icon("search-plus")),
			uiOutput("stat_ui"),
			hr()
		)
	),

	dashboardBody(
		tabItems(
			tabItem(tabName = "aum",
				column(width = 12,
					dygraphOutput("aum_comparison"),
					hr(),
					uiOutput("aum_writeup")
				)
			),
			tabItem(tabName = "badger_tok",
				column(width = 12,
					dygraphOutput("b_tok_comparison"),
					hr(),
					uiOutput("b_tok_writeup")
				)
			),
			tabItem(tabName = "digg_tok",
				column(width = 12,
					dygraphOutput("d_tok_comparison"),
					hr(),
					uiOutput("d_tok_writeup")
				)
			),
			tabItem(tabName = "stat_view",
				column(width = 12,
					plotOutput("dot_plot")
				)
			),
			tabItem(tabName = "sett_view",
				column(width = 9,
					dygraphOutput("sett_aum",height = "240px"),
					hr(),
					dygraphOutput("sett_bal_sup",height = "240px")
				),
				column(width = 3,
					valueBoxOutput("sett_val_b",width = 12),
					valueBoxOutput("sett_apr_b",width = 12),
					valueBoxOutput("sett_deprecated",width = 12),
					valueBoxOutput("sett_boostable",width = 12)
				)
			)
		)
	)
)

# shiny::runApp("~/Work/Repos/OAG_Github/badger_boost")