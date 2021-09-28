library(ghql)
library(jsonlite)
library(dplyr)
library(readr)


con <- GraphqlClient$new("https://api.thegraph.com/subgraphs/name/antonyip/badger_community_subgraph6")
con_p <- GraphqlClient$new("https://api.thegraph.com/subgraphs/name/antonyip/badger_community_subgraph5")
qry <- Query$new()
    
##################################################################
## Stats of Setts Overall
##################################################################
qry$query(
    'setts_stats',
    'query setts_stats
    {
        setts
        {
            id
            name
            symbol
            decimals
            status
            token
            {
            	id
            	name
            	symbol
            	decimals
            }
            totalSupply
            balance
            pricePerFullShare
            netDeposit
            netShareDeposit
            grossDeposit
            grossShareDeposit
            grossWithdraw
            grossShareWithdraw
        }
    }'
)
##################################################################
##################################################################

##################################################################
## Stats of Setts Historical
##################################################################
qry$query(
    'setts_stats_hist',
    'query setts_stats_hist($idlast: String!)
    {
        settSnapshots(orderBy: id, orderDirection: asc,first:1000,where:{id_gt:$idlast})
        {
            id
            timestamp
            name
            symbol
            decimals
            token
            {
            	id
            	name
            	symbol
            	decimals
            }
            totalSupply
            balance
            pricePerFullShare
            netDeposit
            netShareDeposit
            grossDeposit
            grossShareDeposit
            grossWithdraw
            grossShareWithdraw
        }
    }'
)
get_snapshot_all <- function(con=con)
{
    ## Loop historical
    id_last = ""
    snapshot_data <- data.frame()
    while(TRUE)
    {
        snapshot_data_t <- fromJSON(con$exec(qry$queries$setts_stats_hist,list(idlast=id_last)))$data$settSnapshots
        if(length(snapshot_data_t)==0) break()
        snapshot_data <- bind_rows(snapshot_data,snapshot_data_t)
        id_last <- tail(snapshot_data$id,1)
        message(paste0("Fetched ",nrow(snapshot_data)," Entries"))
    }
    return(snapshot_data)
}
get_sett_snapshots <- function(sett_add = "0x19d97d8fa813ee2f51ad4b4e04ea08baf4dffc28",snapshot_data=snapshot_data)
{
    ret_df <- snapshot_data[grepl(sett_add,snapshot_data$id),]
    ret_df$id <- sett_add
    
    return(ret_df)
}
##################################################################
##################################################################

## Get all Setts Metrics
setts <- fromJSON(con$exec(qry$queries$setts_stats))$data$setts
setts_polygon <- fromJSON(con_p$exec(qry$queries$setts_stats))$data$setts

## Get historical snapshots of all setts
mainnet_snapshots <- get_snapshot_all(con=con)
badger_sett_hist <- do.call(rbind,lapply(setts$id,get_sett_snapshots,snapshot_data=mainnet_snapshots))
polygon_snapshots <- get_snapshot_all(con=con_p)
badger_sett_hist_polygon <- do.call(rbind,lapply(setts_polygon$id,get_sett_snapshots,snapshot_data=polygon_snapshots))

# ## Get historical snapshots of a particular sett
# badger_sett_hist <- get_sett_snapshots("0x19d97d8fa813ee2f51ad4b4e04ea08baf4dffc28",snapshot_data=mainnet_snapshots)
# badger_sett_hist_poly <- get_sett_snapshots("0x6b2d4c4bb50274c5d4986ff678cc971c0260e967",snapshot_data=polygon_snapshots)


## Write out
write.csv(setts,"~/Desktop/badger_boost/Setts_mainnet.csv",row.names=FALSE)
write.csv(setts_polygon,"~/Desktop/badger_boost/Setts_polygon.csv",row.names=FALSE)
write.csv(badger_sett_hist,"~/Desktop/badger_boost/Setts_mainnet_hist.csv",row.names=FALSE)
write.csv(badger_sett_hist_poly,"~/Desktop/badger_boost/Setts_polygon_hist.csv",row.names=FALSE)

