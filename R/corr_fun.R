
## A function to look at the correlation between two groups. 

## Cannot use dataframe$variable in a function - have to use [[]] 
# 
# corr_fun <- function(d1, d2, spec1, spec2, method1){
#   a <- c(cor.test(d1[[spec1]], d2[[spec2]], method = method1),
#   cor.test(d1[[spec1]][1:71], d2[[spec2]][2:72], method = method1),
#   cor.test(d1[[spec1]][1:70], d2[[spec2]][3:72], method = method1),
#   cor.test(d1[[spec1]][1:69], d2[[spec2]][4:72], method = method1),
#   cor.test(d1[[spec1]][1:68], d2[[spec2]][5:72], method = method1),
#   cor.test(d1[[spec1]][1:67], d2[[spec2]][6:72], method = method1),
#   cor.test(d1[[spec1]][1:66], d2[[spec2]][7:72], method = method1),
#   cor.test(d1[[spec1]][1:65], d2[[spec2]][8:72], method = method1),
#   cor.test(d1[[spec1]][1:64], d2[[spec2]][9:72], method = method1),
#   cor.test(d1[[spec1]][1:63], d2[[spec2]][10:72], method = method1),
#   cor.test(d1[[spec1]][1:62], d2[[spec2]][11:72], method = method1),
#   cor.test(d1[[spec1]][1:61], d2[[spec2]][12:72], method = method1))
#   cor_coeffs <- (a[c(4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103)])
#   cor_pvals <- (a[c(3,12,21,30,39,48,57,66,75,84,93,102)])
#   corr_and_pvals <- as.data.frame(cbind(cor_coeffs, cor_pvals))
#   #return(as.data.frame(cor_coeffs))
#   #return(cor_pvals)
#   return(corr_and_pvals)
# }



# corr_fun <- function(d1, d2, spec1, spec2, method1){
#   a <- c(cor.test(d1[[spec1]], d2[[spec2]], method = method1),
#          cor.test(d1[[spec1]][1:102], d2[[spec2]][2:103], method = method1),
#          cor.test(d1[[spec1]][1:101], d2[[spec2]][3:103], method = method1),
#          cor.test(d1[[spec1]][1:100], d2[[spec2]][4:103], method = method1),
#          cor.test(d1[[spec1]][1:99], d2[[spec2]][5:103], method = method1),
#          cor.test(d1[[spec1]][1:98], d2[[spec2]][6:103], method = method1),
#          cor.test(d1[[spec1]][1:97], d2[[spec2]][7:103], method = method1),
#          cor.test(d1[[spec1]][1:96], d2[[spec2]][8:103], method = method1),
#          cor.test(d1[[spec1]][1:95], d2[[spec2]][9:103], method = method1),
#          cor.test(d1[[spec1]][1:94], d2[[spec2]][10:103], method = method1),
#          cor.test(d1[[spec1]][1:93], d2[[spec2]][11:103], method = method1),
#          cor.test(d1[[spec1]][1:92], d2[[spec2]][12:103], method = method1))
#   cor_coeffs <- (a[c(4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103)])
#   cor_pvals <- (a[c(3,12,21,30,39,48,57,66,75,84,93,102)])
#   corr_and_pvals <- as.data.frame(cbind(cor_coeffs, cor_pvals))
#   #return(as.data.frame(cor_coeffs))
#   #return(cor_pvals)
#   return(corr_and_pvals)
# }

##Newest version with easier to use output

corr_fun <- function(d1, d2, spec1, spec2, method1){
  a <- c(cor.test(d1[[spec1]], d2[[spec2]], method = method1),
         cor.test(d1[[spec1]][1:102], d2[[spec2]][2:103], method = method1),
         cor.test(d1[[spec1]][1:101], d2[[spec2]][3:103], method = method1),
         cor.test(d1[[spec1]][1:100], d2[[spec2]][4:103], method = method1),
         cor.test(d1[[spec1]][1:99], d2[[spec2]][5:103], method = method1),
         cor.test(d1[[spec1]][1:98], d2[[spec2]][6:103], method = method1),
         cor.test(d1[[spec1]][1:97], d2[[spec2]][7:103], method = method1),
         cor.test(d1[[spec1]][1:96], d2[[spec2]][8:103], method = method1),
         cor.test(d1[[spec1]][1:95], d2[[spec2]][9:103], method = method1),
         cor.test(d1[[spec1]][1:94], d2[[spec2]][10:103], method = method1),
         cor.test(d1[[spec1]][1:93], d2[[spec2]][11:103], method = method1),
         cor.test(d1[[spec1]][1:92], d2[[spec2]][12:103], method = method1))
  cor_coeffs <- c(a[4]["estimate"][[1]][1], a[13]["estimate"][[1]][1],
                 a[22]["estimate"][[1]][1], a[31]["estimate"][[1]][1], 
                 a[40]["estimate"][[1]][1], a[49]["estimate"][[1]][1], 
                 a[58]["estimate"][[1]][1], a[67]["estimate"][[1]][1], 
                 a[76]["estimate"][[1]][1], a[85]["estimate"][[1]][1], 
                 a[94]["estimate"][[1]][1], a[103]["estimate"][[1]][1])
  cor_pvals <- c(a[3][[1]], a[12][[1]], a[21][[1]], a[30][[1]],a[39][[1]],a[48][[1]],
                 a[57][[1]],a[66][[1]],a[75][[1]],a[84][[1]],a[93][[1]],a[102][[1]])
  corr_and_pvals <- as.data.frame(cbind(cor_coeffs, cor_pvals))
  return(corr_and_pvals)
}

#Try2 <- corr_fun(Monthly_Totals, Monthly_Totals, "Dog_Totals", "Jackal_Totals", "pearson") 

#corr_fun(all_counts, all_counts, "Dog_Totals", "Jackal_Totals", "pearson") 
#Try1[5]
