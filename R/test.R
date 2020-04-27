# library(tidyverse)
# 
# f <- function(a, b, c, d, add_i) {
# a <- a + add_i
# b <- b + add_i
# c <- c + add_i
# d <- d + add_i
# o <- konfound::tkonfound(a, b, c, d)
# transfer <- o %>% pluck(2) %>% str_split(" cases") %>% pluck(1, 1) %>% str_split("inference, ") %>% pluck(1, 2) %>% as.integer()
# val <- (transfer/d) / sum(a, b, c, d)
# print(str_c("Processed with adding ", add_i, " to all cells")
# return(val)
# }
# 
# tkonfound(35, 17, 17, 38, thr_p = 0.01, switch_trm = F, plt = T)
# 
# res <- 1:100 %>% 
#     map(f, a = 36, b = 17, c = 17, d = 38)
# 
# tibble(a = 1:100, b = unlist(res)) %>%  
#     ggplot(aes(x = a, y = b)) +
#     geom_point() +
#     geom_line() +
#     ylab("")
