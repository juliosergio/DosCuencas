p9 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(0.2, 0.1),
                  aes(colour = "Group 1")) +
    stat_function(fun = dnorm, args = list(0.7, 0.05),
                  aes(colour = "Group 2")) +
    scale_x_continuous(name = "Probability",
                       breaks = seq(0, 1, 0.2),
                       limits=c(0, 1)) +
    scale_y_continuous(name = "Frequency") +
    ggtitle("Normal function curves of probabilities") +
    scale_colour_brewer(palette="Accent") +
    labs(colour = "Groups")
p9


aa <- list(list(0.2, 0.1), list(0.7, 0.05), list(0.45, 0.2))
p9 <- ggplot(data.frame(x = c(0, 1)), aes(x = x))
for (i in 1:3) {
    p9 <- p9 + stat_function(fun = dnorm, args = aa[[i]],
                             aes_(colour = paste("Group", i)))
}
p9 <- p9 + 
    scale_x_continuous(name = "Probability",
                       breaks = seq(0, 1, 0.2),
                       limits=c(0, 1)) +
    scale_y_continuous(name = "Frequency") +
    ggtitle("Normal function curves of probabilities") +
    scale_colour_brewer(palette="Accent") +
    labs(colour = "Groups")
p9

aa <- list(list(0.2, 0.1), list(0.7, 0.05), list(0.45, 0.2))
p9 <- Reduce(function(x, y){
    x + stat_function(fun = dnorm, args = y, 
                      aes_(colour = paste("Group", length(x$layers)+1)))}, 
    aa, 
    init = ggplot(data.frame(x = c(0, 1)), aes(x = x)))
p9 <- p9 + 
    scale_x_continuous(name = "Probability",
                       breaks = seq(0, 1, 0.2),
                       limits=c(0, 1)) +
    scale_y_continuous(name = "Frequency") +
    ggtitle("Normal function curves of probabilities") +
    scale_colour_brewer(palette="Accent") +
    labs(colour = "Groups")
p9


