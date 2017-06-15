MyData <-
    structure(
        list(Date = 
                 structure(
                     c(1492979809.99827, 1492602845.68722, 
                       1493093428.90318, 1492605578.0691, 1492961342.65056, 1492771976.83545, 
                       1493020588.88485, 1493057018.85104, 1492852011.23873, 1492855996.55059
                     ), class = c("POSIXct", "POSIXt")), 
             Value = c(4.52885504579172, 
                       6.0024610790424, 8.96430060034618, 7.06435370026156, 5.08460514713079, 
                       3.47828012891114, 6.29844291834161, 0.898315710946918, 1.44857675535604, 
                       5.74641009094194)
        ), 
        .Names = c("Date", "Value"), 
        row.names = c(NA,           
                      -10L), 
        class = "data.frame"
    )

tt <- read.table("~/algo.txt", header = T)

for (i in 1:nrow(dict_data_raw)) {
    dict_data_raw$is_key[i] <- 
        if(dict_data_raw$columnfriendlyname[i]==dict_data_raw$table_name[i]) {
            "primary_key" 
        } else 
            if (dict_data_raw$columnfriendlyname[i] %in% u) {
                "foreign_key" 
            } else  "non_key"
}

apply(table,  2,  function(x)  ifelse(x=="Under",paste0("\\cellcolor{", x, "}"),paste0("\\cellcolor333{", x, "}")))


set.seed(8675309)
name <- c('Bil', 'Sal', 'Kai', 'Kat', 'Jim', 'Xiu')
age <- c(7, 8, 8, 9, 8, 7)
HAVE <- data.frame(name, age, matrix(sample(100, 6*6, TRUE), ncol=6))

WANT <- subset(HAVE, select = c(name, age, X2, X4, X6))

HAVE[, c(1:2, sort(order(HAVE[HAVE$name == "Bil",-(1:2)], decreasing = TRUE)[1:3]+2))]    



df1 <- data.frame(ID=c(23425, 84733, 49822, 39940), X=c(312,354,765,432))
df2 <- data.frame(ID=c(23425, 49822), Y=c(111,222))


####################

library(janeaustenr)
library(dplyr)
library(stringr)


original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                   ignore_case = TRUE)))) %>%
    ungroup()

original_books


library(tidytext)
tidy_books <- original_books %>%
    unnest_tokens(word, text)

tidy_books



words <- c("USA","UK","Germany","Australia","Italy","in","to")
sentences <- c("I lived in Germany 2 years",
            "I moved from Italy to USA",
            "people in USA, UK and Australia speak English")

#############################################
# Plotting two or more families of curves in a faceted
# way

gpar <- data.frame(shape=c(9,5,3), scale=c(0.5,1,2))
npar <- data.frame(mean=c(4,5,6), sd=c(0.5,1,1.5))
funcs <- list(dgamma, dnorm)

# Distribuciones gamma
g1 <- ggplot(data.frame(x=c(0,10)), aes(x)) + 
    Map(function(params, name){stat_function(mapping=aes_(color=name),
                                             fun = dgamma, args = params)},
        params = apply(gpar,1,as.list),
        name = paste('Group', 1:nrow(gpar)))

# Distribuciones normales
n1 <- ggplot(data.frame(x=c(0,10)), aes(x)) + 
    Map(function(params, name){stat_function(mapping=aes_(color=name),
                                             fun = dnorm, args = params)},
        params = apply(npar,1,as.list),
        name = paste('Group', 1:nrow(npar)))


# Ambas distribuciones
# en forma facetada
ng <- nrow(gpar)
nn <- nrow(npar)
a0 <- c(0,10)
cc <- c(rep("Gamma",ng), rep("Normal",nn))
gn1 <- ggplot(data.frame(x=a0), aes(x)) + 
    Map(function(ff, params, name, ccs){stat_function(data=data.frame(x=a0, category=ccs), 
                                                      mapping=aes_(color=name),
                                             fun = ff, args = params)},
        ff = c(rep(list(dgamma),ng),  rep(list(dnorm),nn)),
        params = c(apply(gpar,1,as.list), apply(npar,1,as.list)),
        name = c(paste('Group', 1:ng), paste('Group', 1:nn)),
        ccs = cc
    )
gn1 + facet_wrap("category", nrow = 2)


