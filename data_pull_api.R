## Loading required libraries
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

###############################################################
## Helper functions
###############################################################
parse_sett <- function(sett)
{
	data.frame(
				Name = sett$name,
				Asset = sett$asset,
				Asset_Address = sett$underlyingToken,
				Vault_Token = sett$vaultAsset,
				Vault_Token_Address = sett$vaultToken,
				State = sett$state,
				Value = sett$value,
				Balance = sett$balance,
				Token_Per_Share = sett$ppfs,
				APR = sett$apr,
				Bouncer = sett$bouncer,
				If_Boostable = sett$boostable,
				If_Experimental = sett$experimental,
				If_Deprecated = sett$deprecated
			)
}
hist_sett <- function(sett_add,sett_net)
{
	end_date <- today()
	start_date <- end_date - days(15)
	hist_data <- data.frame()
	while(TRUE)
	{
		hist_data_t <- content(GET(paste0(
											"https://api.badger.finance/v2/charts?id=",
											sett_add,
											"&granularity=day&chain=",
											sett_net,
											"&start=",
											start_date,
											"T15%3A19%3A16.772Z&end=",
											end_date,
											"T15%3A19%3A16.772Z"
										)))
		hist_data_t <- do.call(rbind,lapply(hist_data_t,function(x) as.data.frame(t(unlist(x)))))
		hist_data <- rbind(hist_data,hist_data_t)
		if(is.null(nrow(hist_data_t))) break()
		if(nrow(hist_data_t)<15) break()
		end_date <- start_date
		start_date <- end_date - days(15)
	}
	hist_data <- unique(hist_data)
	hist_data$Time <- as_datetime(as.numeric(hist_data$timestamp)/1000)
	return(hist_data)
}
get_price_df <- function(net_id)
{
	t_data_raw <- content(GET(paste0("https://api.badger.finance/v2/tokens?chain=",net_id)),"parsed")
	t_data <- bind_rows(lapply(t_data_raw,function(x) as.data.frame(t(unlist(x)))))
	p_data_raw <- content(GET(paste0("https://api.badger.finance/v2/prices?currency=usd&chain=",net_id)),"parsed")
	p_data <- data.frame(address=names(p_data_raw),price = unlist(p_data_raw))
	row.names(p_data) <- NULL
	merge(x = t_data, y = p_data, by = "address", all = TRUE)
}
###############################################################
###############################################################

## ETH Mainnet
eth_setts_raw <- content(GET("https://api.badger.finance/v2/setts?chain=eth&currency=usd"),"parsed")
eth_setts <- do.call(rbind,lapply(eth_setts_raw,parse_sett))
# eth_setts_hist <- list()
# for(idx in 1:nrow(eth_setts))
# {
# 	eth_setts_hist[[idx]] <- hist_sett(eth_setts$Vault_Token_Address[idx],sett_net="eth")
# 	message(idx)
# }
eth_setts_hist <- mapply(hist_sett,eth_setts$Vault_Token_Address,sett_net="eth",SIMPLIFY=FALSE)
eth_prices <- get_price_df("eth")
write.csv(eth_setts,"~/Desktop/badger_boost/api_data/eth_setts.csv",row.names=FALSE)
write.csv(eth_prices,"~/Desktop/badger_boost/api_data/eth_prices.csv",row.names=FALSE)
saveRDS(eth_setts_hist,"~/Desktop/badger_boost/api_data/eth_setts_hist.RDS")


## BSC
bsc_setts_raw <- content(GET("https://api.badger.finance/v2/setts?chain=bsc&currency=usd"),"parsed")
bsc_setts <- do.call(rbind,lapply(bsc_setts_raw,parse_sett))
bsc_setts_hist <- mapply(hist_sett,bsc_setts$Vault_Token_Address,sett_net="bsc",SIMPLIFY=FALSE)
bsc_prices <- get_price_df("bsc")
write.csv(bsc_setts,"~/Desktop/badger_boost/api_data/bsc_setts.csv",row.names=FALSE)
write.csv(bsc_prices,"~/Desktop/badger_boost/api_data/bsc_prices.csv",row.names=FALSE)
saveRDS(bsc_setts_hist,"~/Desktop/badger_boost/api_data/bsc_setts_hist.RDS")

## Matic/Polygon
matic_setts_raw <- content(GET("https://api.badger.finance/v2/setts?chain=matic&currency=usd"),"parsed")
matic_setts <- do.call(rbind,lapply(matic_setts_raw,parse_sett))
matic_setts_hist <- mapply(hist_sett,matic_setts$Vault_Token_Address,sett_net="matic",SIMPLIFY=FALSE)
matic_prices <- get_price_df("matic")
write.csv(matic_setts,"~/Desktop/badger_boost/api_data/matic_setts.csv",row.names=FALSE)
write.csv(matic_prices,"~/Desktop/badger_boost/api_data/matic_prices.csv",row.names=FALSE)
saveRDS(matic_setts_hist,"~/Desktop/badger_boost/api_data/matic_setts_hist.RDS")

## Arbitrum
arbitrum_setts_raw <- content(GET("https://api.badger.finance/v2/setts?chain=arbitrum&currency=usd"),"parsed")
arbitrum_setts <- do.call(rbind,lapply(arbitrum_setts_raw,parse_sett))
arbitrum_setts_hist <- mapply(hist_sett,arbitrum_setts$Vault_Token_Address,sett_net="arbitrum",SIMPLIFY=FALSE)
arbitrum_prices <- get_price_df("arbitrum")
write.csv(arbitrum_setts,"~/Desktop/badger_boost/api_data/arbitrum_setts.csv",row.names=FALSE)
write.csv(arbitrum_prices,"~/Desktop/badger_boost/api_data/arbitrum_prices.csv",row.names=FALSE)
saveRDS(arbitrum_setts_hist,"~/Desktop/badger_boost/api_data/arbitrum_setts_hist.RDS")

## Boost Leaderboard
user_boost_leaderboard <- bind_rows(lapply(content(GET("https://api.badger.finance/v2/leaderboards/complete")),function(x) as.data.frame(t(unlist(x)))))
write.csv(user_boost_leaderboard,"~/Desktop/badger_boost/api_data/user_boost_leaderboard.csv",row.names=FALSE)




