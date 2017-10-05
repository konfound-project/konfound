# library(tidyverse)
# 
# x <- pkonfound(2, .4, 100, 3, to_return="plot")
# x <- x %>% mutate(key = ifelse(key == "percent_bias", "To Invalidate", "Effect"))
# x$key <- factor(x$key, levels = c("To Invalidate", "Effect"))
# ggplot(x, aes(x = inference, y = val, fill = key)) +
#     geom_col() +
#     scale_fill_manual("Inference", values = c("green4", "darkgray")) +
#     ggplot2::ylab("%") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# x <- pkonfound(.4, 2, 100, 3, to_return="plot")
# x <- x %>% mutate(key = ifelse(key == "percent_bias", "To Sustain", "Effect"))
# x$key <- factor(x$key, levels = c("To Sustain", "Effect"))
# ggplot(x, aes(x = inference, y = val, fill = key)) +
#     geom_col() +
#     scale_fill_manual("Inference", values = c("red4", "darkgray")) +
#     ggplot2::ylab("%") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
