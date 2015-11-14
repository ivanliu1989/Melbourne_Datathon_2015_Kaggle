names(total)
# [1] "ACCOUNT_ID"                       "EVENT_ID"                         "TRANSACTION_COUNT_INPLAY"        
# [4] "TRANSACTION_COUNT_OUTPLAY"        "TRANSACTION_COUNT_ALL"            "AVG_BET_SIZE_INPLAY"             
# [7] "AVG_BET_SIZE_OUTPLAY"             "AVG_BET_SIZE_ALL"                 "MAX_BET_SIZE_INPLAY"             
# [10] "MAX_BET_SIZE_OUTPLAY"             "MIN_BET_SIZE_INPLAY"              "MIN_BET_SIZE_OUTPLAY"            
# [13] "STDEV_BET_SIZE_INPLAY"            "STDEV_BET_SIZE_OUTPLAY"           "STDEV_BET_SIZE_ALL"              
# [16] "AVG_PLACED_TAKEN_TIME_INPLAY"     "AVG_PLACED_TAKEN_TIME_OUTPLAY"    "AVG_PLACED_TAKEN_TIME_ALL"       
# [19] "MEDIAN_PLACED_TAKEN_TIME_INPLAY"  "MEDIAN_PLACED_TAKEN_TIME_OUTPLAY" "MEDIAN_PLACED_TAKEN_TIME_ALL"    
# [22] "STDEV_PLACED_TAKEN_TIME_INPLAY"   "STDEV_PLACED_TAKEN_TIME_OUTPLAY"  "STDEV_PLACED_TAKEN_TIME_ALL"     
# [25] "STDEV_TAKEN_HOUR_INPLAY"          "STDEV_TAKEN_HOUR_OUTPLAY"         "PREV_WIN_RATE_INPLAY"            
# [28] "PREV_WIN_RATE_OUTPLAY"            "PREV_WIN_RATE_ALL"                "NET_PROFIT_INPLAY"               
# [31] "NET_PROFIT_OUTPLAY"               "NET_PROFIT_ALL"                   "MARGIN_INPLAY"                   
# [34] "MARGIN_OUTPLAY"                   "MARGIN_ALL"                       "SD_BET_TAKEN_INPLAY"             
# [37] "SD_BET_TAKEN_OUTPLAY"             "SD_BET_TAKEN_ALL"                 "AVG_BET_TAKEN_INPLAY"            
# [40] "AVG_BET_TAKEN_OUTPLAY"            "AVG_BET_TAKEN_ALL"                "MEDIAN_BET_TAKEN_INPLAY"         
# [43] "MEDIAN_BET_TAKEN_OUTPLAY"         "MEDIAN_BET_TAKEN_ALL"             "INPLAY_RATIO"                    
# [46] "BL_RATIO_INPLAY"                  "BL_RATIO_OUTPLAY"                 "BL_RATIO"                        
# [49] "BL_DIFF_TRANSACTION_COUNT_IN"     "BL_DIFF_AVG_BET_SIZE_IN"          "BL_DIFF_MAX_BET_SIZE_IN"         
# [52] "BL_DIFF_MIN_BET_SIZE_IN"          "BL_DIFF_STDEV_BET_SIZE_IN"        "BL_DIFF_TRANSACTION_COUNT_OUT"   
# [55] "BL_DIFF_AVG_BET_SIZE_OUT"         "BL_DIFF_MAX_BET_SIZE_OUT"         "BL_DIFF_MIN_BET_SIZE_OUT"        
# [58] "BL_DIFF_STDEV_BET_SIZE_OUT"       "EVENT_COUNT"                      "PREV_FREQ"                       
# [61] "PREV_FREQ_SKEW"                   "PREV_WIN"                         "PREV_WIN_RATE"                   
# [64] "PREV_WIN_SKEW"                    "MARGIN_SKEW"                      "MARGIN_TOTAL"                    
# [67] "WIN_HIST_LAG1"                    "WIN_HIST_LAG2"                    "WIN_HIST_LAG3"                   
# [70] "NET_PROFIT_IN_PER_EVENT"          "NET_PROFIT_OUT_PER_EVENT"         "NET_PROFIT_ALL_PER_EVENT"        
# [73] "INVEST"                           "tsne_3d_1"                        "tsne_3d_2"                       
# [76] "tsne_3d_3"                        "flag_regr"                        "flag_class"                


par(mfrow=c(2,1))
feat <- 'INVEST'
hist(log10(total[total$flag_class == 'N',feat]))
hist(log10(total[total$flag_class == 'Y',feat]))

hist(total[total$flag_class == 'N',feat])
hist(total[total$flag_class == 'Y',feat])



AVG_BET_SIZE_OUTPLAY
AVG_BET_SIZE_ALL
INVEST