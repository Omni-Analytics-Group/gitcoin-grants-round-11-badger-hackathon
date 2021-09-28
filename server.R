## Load libraries
library(shinydashboard)
library(readr)
library(dygraphs)
library(lubridate)
library(xts)
library(httr)
library(dplyr)
library(ggplot2)

################################################################################
## Helper Functions
################################################################################
## Price Match
price_match_sett <- function(date_m,sett_hist,var_n = "value")
{
	match_idx <- match(date_m,as_date(sett_hist$Time))
	if(is.na(match_idx)) return(0)
	as.numeric(sett_hist[match_idx[1],var_n])
}
## Get Price
get_price <- function(c_id)
{
	start_t <- as.numeric(as_datetime("2021-01-01"))
	end_t <- as.numeric(now())

	query_call <- paste0(
							"https://api.coingecko.com/api/v3/coins/",
							c_id,
							"/market_chart/range?vs_currency=usd&from=",
							start_t,
							"&to=",
							end_t
						)
	data_p <- content(GET(query_call))$prices
	data_p_df <- data.frame(
							 Date = as_date(as_datetime(sapply(data_p,"[[",1)/1000)),
							 Price = sapply(data_p,"[[",2)
						)
	names(data_p_df)[2] <- c_id
	return(data_p_df)
}
################################################################################
################################################################################

################################################################################
## Read Data
################################################################################
## Read in ETH data
eth_setts <- read_csv("api_data/eth_setts.csv",col_types = cols())
eth_setts$id <- sub('.', '', eth_setts$Vault_Token)
eth_prices <- read_csv("api_data/eth_prices.csv",col_types = cols())
eth_setts_hist <- readRDS("api_data/eth_setts_hist.RDS")

## Read in BSC data
bsc_setts <- read_csv("api_data/bsc_setts.csv",col_types = cols())
bsc_prices <- read_csv("api_data/bsc_prices.csv",col_types = cols())
bsc_setts_hist <- readRDS("api_data/bsc_setts_hist.RDS")

## Read in MATIC data
matic_setts <- read_csv("api_data/matic_setts.csv",col_types = cols())
matic_prices <- read_csv("api_data/matic_prices.csv",col_types = cols())
matic_setts_hist <- readRDS("api_data/matic_setts_hist.RDS")

## Read in ARBITRUM data
arbitrum_setts <- read_csv("api_data/arbitrum_setts.csv",col_types = cols())
arbitrum_prices <- read_csv("api_data/arbitrum_prices.csv",col_types = cols())
arbitrum_setts_hist <- readRDS("api_data/arbitrum_setts_hist.RDS")
################################################################################
################################################################################

################################################################################
## Vault Volume around 5 Aug
################################################################################
time_start <- as_date("2021-01-01")
time_end <- as_date(today())
val_time_df <- data.frame(Date = seq(time_start, time_end, by = "days"))

## ETH
val_time_df <- cbind(val_time_df,do.call(cbind,lapply(eth_setts_hist,function(x,y,z) sapply(y,price_match_sett,sett_hist=x,var_n=z),y=val_time_df$Date,z="value")))
names(val_time_df)[-1] <- eth_setts$id
val_time_df$All_Vaults <- apply(val_time_df[,2:22],1,sum,na.rm=TRUE)/10^6
val_time_df$Boosted_Vaults <- apply(val_time_df[,2:22][,eth_setts$If_Boostable],1,sum,na.rm=TRUE)/10^6
val_time_df$Non_Boosted_Vaults <- apply(val_time_df[,2:22][,!eth_setts$If_Boostable],1,sum,na.rm=TRUE)/10^6
val_time_df <- val_time_df[val_time_df$All_Vaults > 0,]
val_time_xts <- xts(val_time_df[,-1],order.by=val_time_df$Date)
################################################################################
################################################################################

################################################################################
## Vault Ratio around 5 Aug
################################################################################
rat_time_df <- data.frame(Date = val_time_df$Date)
## ETH
rat_time_df <- cbind(rat_time_df,do.call(cbind,lapply(eth_setts_hist,function(x,y,z) sapply(y,price_match_sett,sett_hist=x,var_n=z),y=rat_time_df$Date,z="ratio")))
names(rat_time_df)[-1] <- eth_setts$id
rat_time_xts <- xts(rat_time_df[,-1],order.by=rat_time_df$Date)
################################################################################
################################################################################

################################################################################
## Vault Balance around 5 Aug
################################################################################
bal_time_df <- data.frame(Date = val_time_df$Date)
## ETH
bal_time_df <- cbind(bal_time_df,do.call(cbind,lapply(eth_setts_hist,function(x,y,z) sapply(y,price_match_sett,sett_hist=x,var_n=z),y=rat_time_df$Date,z="balance")))
names(bal_time_df)[-1] <- eth_setts$id
bal_time_xts <- xts(bal_time_df[,-1],order.by=bal_time_df$Date)
################################################################################
################################################################################

################################################################################
## Vault Supply around 5 Aug
################################################################################
sup_time_df <- data.frame(Date = val_time_df$Date)
## ETH
sup_time_df <- cbind(sup_time_df,do.call(cbind,lapply(eth_setts_hist,function(x,y,z) sapply(y,price_match_sett,sett_hist=x,var_n=z),y=rat_time_df$Date,z="supply")))
names(sup_time_df)[-1] <- eth_setts$id
sup_time_xts <- xts(sup_time_df[,-1],order.by=sup_time_df$Date)
################################################################################
################################################################################

################################################################################
## Token Prices around 5 Aug
################################################################################
## Badger & Digg Token
btc_price <- get_price("bitcoin")
badger_price <- get_price("badger-dao")
digg_price <- get_price("digg")
all_prices <- merge(merge(btc_price,badger_price),digg_price)
all_prices_xts <- xts(all_prices[,-1],order.by=all_prices$Date)
################################################################################
################################################################################

function(input, output, session)
{
	################################################################################
	## Pre Post Boost
	################################################################################
	## AUM
	output$aum_writeup <- renderUI({h3("In this graph we see the change in Assets Under Management for Boosted setts slightly increased in the days after the implementation. This fact highlights a slight diverging pattern where the non-Boosted Setts saw a decrease in their Total Locked Value. By the end of the month, the AUM for Boosted Setts were at their highest, while the non-Boosted Setts continued to struggle to regain its former TVL apex.")})
	output$aum_comparison <- renderDygraph({ 
												dygraph(val_time_xts[,c("Boosted_Vaults","Non_Boosted_Vaults")], main = "Boosted vs Non Boosted Vaults AUM (Mainnet)",ylab = "Vault Value (in Millions USD)")%>%
												dyAxis("y", logscale=TRUE) %>%
			  									dyEvent("2021-8-05", "Boost Implementation", labelLoc = "bottom") %>%
			  									dyLegend(width = 400) %>%
			  									dyRangeSelector(dateWindow = c("2021-07-05", "2021-09-05")) %>%
			  									dyShading(from = "2021-8-02", to = "2021-8-8", color = "#CCEBD6")
										})

	## Badger Token
	output$b_tok_writeup <- renderUI({h3("The graph above shows a pronounced and sustained increase in the price of Badger post-Boost implementation. Though other market influences are likely to have played a role, the beginning of the trend is quite noticeable as the price appreciated 2x quickly after the August 5th program implementation.")})
	output$b_tok_comparison <- renderDygraph({ 
												dygraph(all_prices_xts[,c("badger-dao")], main = "Badger Token Price Around Boost Event",ylab = "Badger Token Price in USD")%>%
			  									# dySeries("bitcoin", axis = 'y2') %>%
			  									# dyAxis("y2", label = "BTC Price for Benchmark", independentTicks = TRUE) %>%
			  									dyEvent("2021-8-05", "Boost Implementation", labelLoc = "bottom") %>%
			  									dyLegend(width = 400) %>%
			  									dyRangeSelector(dateWindow = c("2021-07-05", "2021-09-05")) %>%
			  									dyShading(from = "2021-8-01", to = "2021-8-8", color = "#CCEBD6")
										})

	## Digg Token
	output$d_tok_writeup <- renderUI({h3("Prior to the August 5th Boost incentive program, the $DIGG token had spent a little over a week off of its peg as the price of $BTC remained consistently higher. Immediately after the announcement there was an abrupt and sudden positive rebase that placed the price of $DIGG firmly above $BTC until there were enough negative rebases to place the token back on peg. This graph shows that the effects of increased demand for native Badger assets can be significant enough to create noticeable price movements.")})
	output$d_tok_comparison <- renderDygraph({ 
												dygraph(all_prices_xts[,c("bitcoin","digg")], main = "Digg Token Price Around Boost Event",ylab = "Price in USD")%>%
			  									dyEvent("2021-8-05", "Boost Implementation", labelLoc = "bottom") %>%
			  									dyLegend(width = 400) %>%
			  									dyRangeSelector(dateWindow = c("2021-07-05", "2021-09-05")) %>%
			  									dyShading(from = "2021-8-01", to = "2021-8-8", color = "#CCEBD6")
										})
	################################################################################
	################################################################################

	################################################################################
	## Micro Sett Statistics
	################################################################################
	output$stat_ui <- renderUI({
								if(input$tabs!="stat_view") return(NULL)
								selectizeInput('stat_sel', "Select Metric", choices = c("Value ($)","APR (%)","Token Ratio"), multiple = FALSE)
							})
	output$dot_plot <- renderPlot({
									if(is.null(input$stat_sel)) return(NULL)
									if(input$stat_sel == "Value ($)")
									{
										plot_data <- eth_setts %>% arrange(Value) %>% mutate(Sett = factor(id, levels = id))
										pt <- ggplot(data = plot_data, aes(x = Value, y = Sett)) +
												geom_point() +
												geom_segment(aes(yend = Sett, x = 0, xend = Value)) +
												scale_x_log10(labels = scales::dollar,
												              breaks = 10^(1:10)) +
												labs(title = "USD Value for the Different Setts", x = "Value ($)")+
												theme(plot.background = element_blank())
										return(pt)
									}
									if(input$stat_sel == "APR (%)")
									{
										plot_data <- eth_setts %>% arrange(APR) %>% mutate(Sett = factor(id, levels = id)) %>%
										    mutate(Label = scales::percent(APR / 100, accuracy = 1))
										pt <- ggplot(data = plot_data, aes(x = APR, y = Sett)) +
												geom_point() +
												geom_segment(aes(yend = Sett, x = 0, xend = APR)) +
										        geom_text(aes(label = Label), hjust = -0.2) +
												scale_x_log10(labels = function(.) scales::percent(. / 100, accuracy = 1),
												              breaks = 100 * c(.02, .04, .08, .16, .32, .64, 1.28)) +
												labs(title = "APR for the Different Setts", x = "APR (%)")+
												theme(plot.background = element_blank())
										return(pt)
									}
									if(input$stat_sel == "Token Ratio")
									{
										plot_data <- eth_setts %>% arrange(Token_Per_Share) %>% mutate(Sett = factor(id, levels = id))
										pt <- ggplot(data = plot_data, aes(x = Token_Per_Share, y = Sett)) +
												geom_point() +
												geom_segment(aes(yend = Sett, x = 0, xend = Token_Per_Share)) +
												scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
												labs(title = "Token Ratio for the Different Setts")+
												xlab("Token Ratio")+
												theme(plot.background = element_blank())
										return(pt)
									}
						}, bg="transparent")

	################################################################################
	################################################################################
	
	################################################################################
	## Micro Sett Statistics
	################################################################################
	output$sett_ui <- renderUI({
								if(input$tabs!="sett_view") return(NULL)
								selectizeInput('sett_sel', "Select Sett", choices = names(val_time_xts)[1:21], multiple = FALSE)
							})
	output$sett_aum <- renderDygraph({ 
												if(is.null(input$sett_sel)) return(NULL)
												dygraph(val_time_xts[,input$sett_sel]/10^6, main = NULL,ylab = "Value in Million USD",group = "micro_sett")%>%
			  									dyEvent("2021-8-05", "Boost Implementation", labelLoc = "bottom") %>%
			  									dyLegend(width = 400) %>%
			  									dyRangeSelector(dateWindow = c("2021-07-05", "2021-09-05")) %>%
			  									dyShading(from = "2021-8-01", to = "2021-8-8", color = "#CCEBD6")
										})
	output$sett_bal_sup <- renderDygraph({ 
												if(is.null(input$sett_sel)) return(NULL)
												Balance <- bal_time_xts[,input$sett_sel]
												Supply <- sup_time_xts[,input$sett_sel]
												Ratio <- rat_time_xts[,input$sett_sel]
												plot_xts <- cbind(Balance,Supply,Ratio)
												names(plot_xts) <- c("Balance","Supply","Ratio")
												dygraph(plot_xts, main = NULL,ylab = "Token Count",group = "micro_sett")%>%
												dySeries("Ratio", axis = 'y2') %>%
												dyAxis("y2", label = "Token Ratio", independentTicks = TRUE) %>%
			  									dyEvent("2021-8-05", "Boost Implementation", labelLoc = "bottom") %>%
			  									dyLegend(width = 400) %>%
			  									dyShading(from = "2021-8-01", to = "2021-8-8", color = "#CCEBD6")
										})
	output$sett_boostable <- renderInfoBox({
												if(is.null(input$sett_sel)) return(valueBox("", ""))
												response <- ifelse(eth_setts$If_Boostable[match(input$sett_sel,eth_setts$id)],"Yes","No")
												box_c <- ifelse(response=="Yes","green","red")
												valueBox(response, "Boosted?",color=box_c)
								})
	output$sett_deprecated <- renderInfoBox({
												if(is.null(input$sett_sel)) return(valueBox("", ""))
												response <- ifelse(eth_setts$If_Deprecated[match(input$sett_sel,eth_setts$id)],"Yes","No")
												box_c <- ifelse(response=="Yes","red","green")
												valueBox(response, "Deprecated?",color=box_c)
								})	
	output$sett_val_b <- renderInfoBox({
												if(is.null(input$sett_sel)) return(valueBox("", ""))
												response <- round(as.numeric(eth_setts$Value[match(input$sett_sel,eth_setts$id)])/10^6,2)
												valueBox(response, "Sett Value (Million $)")
								})
	output$sett_apr_b <- renderInfoBox({
												if(is.null(input$sett_sel)) return(valueBox("", ""))
												response <- round(as.numeric(eth_setts$APR[match(input$sett_sel,eth_setts$id)]),2)
												valueBox(response, "Sett APR %")
								})
	################################################################################
	################################################################################

}







