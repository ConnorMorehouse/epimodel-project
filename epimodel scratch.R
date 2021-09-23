# setup #
#####
require(MASS)
require(parallel)
require(doParallel)
require(foreach)
require(readr)
require(readxl)
require(tidyverse)
require(tibble)
require(network)
require(tergmLite)
require(EpiModel)
require(xtable)

setwd('C:/Users/Connor/Documents/school/R/projects/DTS internship proj')

id_res <- read_xlsx('ids and res.xlsx')
stuacad_df <- read_xls('STUACAD.20FA.xls')

stuacad_df <- stuacad_df %>% 
  rename(id = 'Student ID')

id_res <- id_res %>% 
  rename(id = 'Id')

joined <- left_join(id_res, stuacad_df)

joined <- joined %>% 
  filter(!is.na(id)) %>% 
  rename(sport = 'Wjc Active Sports One Line',
         bld = 'Wjc Current Bldg',
         major1 = 'Active Major1 Desc',
         major2 = 'Active Major2 Desc',
         major3 = 'Active Major3 Desc',
         major4 = 'Active Major4 Desc',
         minor1 = 'Active Minor1 Desc',
         minor2 = 'Active Minor2 Desc',
         minor3 = 'Wjc Active Minor3 Desc',
         class = 'Class') %>% 
  dplyr::select(id:class,major1:minor3)

nodes <- dplyr::select(joined,id)

ish <- c('test','tester','data2','gapminder','p')

rm(list = ish, inherits = F)

#####
# functions # 
#####

`%notin%` <- Negate(`%in%`)

netdense <- function(links,nodes) {
  sqrt(nrow(links)/nrow(nodes)^2)
}

meandeg <- function(links,nodes) {
  nrow(links)/nrow(nodes)
}

sim.peak.qnt <- function(sim) {
  x <- c(0, .25, .5, .75, 1)
  lapply(x, get.qnt.data)
}

get.peak.qnt. <- function(x) {
  sim.dat.x <- as.data.frame(sim, out = 'qnt', qval = x)
  peak.x <- max(sim.dat.x$i.num)
}

#####
# also time tests #
#####


rm(testlist)
testlist = list() 
times[[1]] <- system.time({
for (y in 1:10) {
  testlist[[y]] <- tibble(ish = double(),
                          ish2 = double(),
                          key = character())
  for (i in 1:20) {
    for (x in 1:20) {
      if (joined$id[i] == joined$id[x]) {next}
      else if (joined[i,5] %in% joined[x,5:8]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% testlist[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = c(.1,.9))) {
            new1 <- joined$id[i];
            testlist[[y]][nrow(testlist[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            testlist[[y]][nrow(testlist[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            testlist[[y]][nrow(testlist[[y]]), 3] <- new3}
        }
      }
    }
  }
}
})
times

#####
# small loop test #
#####


it_link <- function(joined) {
  if (joined$id[i] != joined$id[x]) {
    if (joined[i,5] %in% joined[x,5:8]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% testlist[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = c(.1,.9))) {
          new1 <- joined$id[i];
          testlist[[y]][nrow(testlist[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          testlist[[y]][nrow(testlist[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          testlist[[y]][nrow(testlist[[y]]), 3] <- new3}
      }
    }
  }
}

#####
# time tests #
#####

rm(testlist)
testlist = list()
times[[2]] <- system.time({
foreach(y = 1:10) %dopar% {
  testlist[[y]] <- dplyr::tibble(ish = double(),
                          ish2 = double(),
                          key = character());
  print(y)
  for (i in 1:20) {
    for (x in 1:20) {
      if (joined$id[i] != joined$id[x]) {
       if (joined[i,5] %in% joined[x,5:8]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% testlist[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = c(.1,.9))) {
            new1 <- joined$id[i];
            return(testlist[[y]][nrow(testlist[[y]]) + 1, 1] <- new1);
            new2 <- joined$id[x];
            return(testlist[[y]][nrow(testlist[[y]]), 2] <- new2);
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            return(testlist[[y]][nrow(testlist[[y]]), 3] <- new3)}
          }
        }
      }
    }
  }
}
})
times
View(testlist)

rm(testlist)
testlist = list()
x = 1
i = 1
times[[3]] <- system.time({
  for (y in 1:10) {
    testlist[[y]] <- dplyr::tibble(ish = double(),
                                   ish2 = double(),
                                   key = character());
    foreach(1:20) %:% 
      foreach(1:20) %dopar% {
        if (joined$id[i] != joined$id[x]) {
          if (joined[i,5] %in% joined[x,5:8]) {
            if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% testlist[[y]][,3]) {
              if (sample(c(T,F), size = 1, replace = T, prob = c(.1,.9))) {
                new1 <- joined$id[i];
                testlist[[y]][nrow(testlist[[y]]) + 1, 1] <- new1;
                new2 <- joined$id[x];
                testlist[[y]][nrow(testlist[[y]]), 2] <- new2;
                new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
                testlist[[y]][nrow(testlist[[y]]), 3] <- new3}
              }
            }
          }
        x = x + 1
        }
      }
    
})

rm(testlist)
testlist = list()
x = 1
times[[4]] <- system.time({
  for (y in 1:10) {
    testlist[[y]] <- dplyr::tibble(ish = double(),
                                   ish2 = double(),
                                   key = character());
    for (i in 1:20) {
      foreach(1:20) %dopar% {
        if (joined$id[i] != joined$id[x]) {
          if (joined[i,5] %in% joined[x,5:8]) {
            if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% testlist[[y]][,3]) {
              if (sample(c(T,F), size = 1, replace = T, prob = c(.1,.9))) {
                new1 <- joined$id[i];
                testlist[[y]][nrow(testlist[[y]]) + 1, 1] <- new1;
                new2 <- joined$id[x];
                testlist[[y]][nrow(testlist[[y]]), 2] <- new2;
                new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
                testlist[[y]][nrow(testlist[[y]]), 3] <- new3}
            }
          }
        }
        x = x + 1
      } 
    }
  }
})
times
View(testlist)


#####
# also part of loop (prolly 3rd loop tests) #
#####


rm(testlist)
testlist = list()
system.time({
for (y in 1:10) {
  testlist[[y]] <- dplyr::tibble(ish = double(),
                                 ish2 = double(),
                                 key = character())
  for (i in 1:20) {
    for (x in 1:20) {
      if (joined$id[i] != joined$id[x]) {
        if (joined[i,5] %in% joined[x,5:8]) {
          if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% testlist[[y]][,3]) {
            if (sample(c(T,F), size = 1, replace = T, prob = c(.1,.9))) {
              new1 <- joined$id[i];
              testlist[[y]][nrow(testlist[[y]]) + 1, 1] <- new1;
              new2 <- joined$id[x];
              testlist[[y]][nrow(testlist[[y]]), 2] <- new2;
              new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
              testlist[[y]][nrow(testlist[[y]]), 3] <- new3}
          }
        }
      }
    }
  }
}
})
View(testlist)



# part of loop #
# more loop pieces #
#####

rm(testlist)
testlist = list()
system.time({
  for (y in 1:10) {
    testlist[[y]] <- dplyr::tibble(ish = double(),
                                   ish2 = double(),
                                   key = character());
    foreach(i = 1:20) %:%
      foreach(x = 1:20) %dopar% {
        if (joined$id[i] != joined$id[x]) {
          print(paste(i,x))
          # if (joined[i,5] %in% joined[x,5:8]) {
          #   if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% testlist[[y]][,3]) {
          #     if (sample(c(T,F), size = 1, replace = T, prob = c(.1,.9))) {
          #       new1 <- joined$id[i];
          #       testlist[[y]][nrow(testlist[[y]]) + 1, 1] <- new1;
          #       new2 <- joined$id[x];
          #       testlist[[y]][nrow(testlist[[y]]), 2] <- new2;
          #       new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          #       testlist[[y]][nrow(testlist[[y]]), 3] <- new3}
          #   }
          # }
        }
      }
    }
  })
View(testlist)


# big old loop #
# full double loop #
#####

housing_link_prob <- c(.6,.4)
same_class_link_prob <- c(.5,.5)
maybe_same_class_link_prob <- c(.25,.75)
team_link_prob <- c(.4,.6)


rm(samplepops)
samplepops = list() 
y = 1
system.time({
  samplepops[[y]] <- dplyr::tibble(ish = double(),
                                   ish2 = double(),
                                   key = character())
for (i in 1:nrow(joined)) {
  for (x in 1:nrow(joined)) {
    if (joined$id[i] != joined$id[x]) {  
      if (joined$bld[i] == joined$bld[x]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = housing_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
    } else if (joined$major1[i] %in% joined[x,5:8]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
    } else if (joined$major2[i] %in% joined[x,5:8]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
    } else if (joined$major3[i] %in% joined[x,5:8]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
    } else if (joined$major4[i] %in% joined[x,5:8]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
    } else if (joined$minor1[i] %in% joined[x,9:11]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
    } else if (joined$minor2[i] %in% joined[x,9:11]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
    } else if (joined$minor2[i] %in% joined[x,9:11]) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}} 
    } else if ((!is.na(joined$sport[i]) & !is.na(joined$sport[x])) & (joined$sport[i] %in% joined$sport[x])) {
      if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
        if (sample(c(T,F), size = 1, replace = T, prob = team_link_prob)) {
          new1 <- joined$id[i];
          samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
          new2 <- joined$id[x];
          samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
          new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
          samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}} 
    }
  }
}
}
})

#####
# short #
#####

y = 1
rm(samplepops)
samplepops = list()
samplepops[[y]] <- dplyr::tibble(ish = double(),
                                 ish2 = double(),
                                 key = character())
system.time({
for (y in 1:1) {
  samplepops[[y]] <- dplyr::tibble(ish = double(),
                                   ish2 = double(),
                                   key = character())
for (i in 1:10) {
  for (x in 1:10) {
    if (joined$id[i] != joined$id[x]) {  
      if (joined$bld[i] == joined$bld[x]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = housing_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
      } else if (joined$major1[i] %in% joined[x,5:8]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
      } else if (joined$major2[i] %in% joined[x,5:8]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
      } else if (joined$major3[i] %in% joined[x,5:8]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
      } else if (joined$major4[i] %in% joined[x,5:8]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
      } else if (joined$minor1[i] %in% joined[x,9:11]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
      } else if (joined$minor2[i] %in% joined[x,9:11]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}}
      } else if (joined$minor2[i] %in% joined[x,9:11]) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = same_class_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}} 
      } else if ((!is.na(joined$sport[i]) & !is.na(joined$sport[x])) & (joined$sport[i] %in% joined$sport[x])) {
        if (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])) %notin% samplepops[[y]][,3]) {
          if (sample(c(T,F), size = 1, replace = T, prob = team_link_prob)) {
            new1 <- joined$id[i];
            samplepops[[y]][nrow(samplepops[[y]]) + 1, 1] <- new1;
            new2 <- joined$id[x];
            samplepops[[y]][nrow(samplepops[[y]]), 2] <- new2;
            new3 <- (paste(pmin(joined$id[i],joined$id[x]),pmax(joined$id[i],joined$id[x])));
            samplepops[[y]][nrow(samplepops[[y]]), 3] <- new3;}} 
      }
    }
  }
}
}
})
lapply(samplepops, allunique)
lapply(samplepops, View)

#####
# shit #
##### 
end = 10

rm(testlist)
testlist = list()
for (y in 1:end) {
  testlist[[y]] <- dplyr::tibble(a = numeric(),
                            b = numeric(),
                            c = character())
  
  for (i in 1:nrow(joined)) {
    for (x in 1:nrow(joined)) {
      if (sample(c(T,F), size = 1, replace = T, prob = c(.0125,.9875))) {
        
        print(round((x + ((i - 1)*nrow(joined)) + ((y - 1)*nrow(joined)*nrow(joined)))/(nrow(joined)*nrow(joined)*end),2));
        new1 = joined[i,1];
        testlist[[y]][nrow(testlist[[y]]) + 1, 1] <- new1;
        new2 = joined[x,1];
        testlist[[y]][nrow(testlist[[y]]), 2] <- new2;
        new3 = paste(pmin(joined$id[i], joined$id[x]), pmax(joined$id[i], joined$id[x]));
        testlist[[y]][nrow(testlist[[y]]), 3] <- new3
      }
    }
  }
}
# dup removal
no_dup <- lapply(testlist, distinct, c, .keep_all = T)

# tests dup removal
n_occur <- data.frame(table(testlist[[1]][,3]))
n_occur2 <- data.frame(table(testlist[[2]][,3]))
n_occur3 <- data.frame(table(testlist[[3]][,3]))
n_occur4 <- data.frame(table(testlist[[4]][,3]))
n_occur5 <- data.frame(table(testlist[[5]][,3]))
n_occur6 <- data.frame(table(testlist[[6]][,3]))
n_occur7 <- data.frame(table(testlist[[7]][,3]))
n_occur8 <- data.frame(table(testlist[[8]][,3]))
n_occur9 <- data.frame(table(testlist[[9]][,3]))
n_occur10 <- data.frame(table(testlist[[10]][,3]))
oc <- list(n_occur,n_occur2,n_occur3,n_occur4,n_occur5,n_occur6,n_occur7,n_occur8,n_occur9,n_occur10)

oc_dup <- list(n_occur[n_occur$Freq > 1,],n_occur2[n_occur2$Freq > 1,],n_occur3[n_occur3$Freq > 1,],n_occur4[n_occur4$Freq > 1,])
lapply(oc_dup,nrow)

for (i in 1:4) {
  print(nrow(testlist[[i]]) - nrow(no_dup[[i]]))
}

# verify network density 
lapply(no_dup, netdense, joined)

#####
# you fucking idiot #
#####
#####
# example 1 #
#####
testnw <- network.initialize(n = 1000, directed = F)
testnw <- set.vertex.attribute(testnw, 'race', rep(0:1, each = 500))

formation <- ~edges + nodefactor('race') + nodematch('race') + concurrent

target.stats  <- c(250, 375, 225, 100)

coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 25)
coef.diss

est1 <- netest(testnw, formation, target.stats, coef.diss, edapprox = T)

dx <- netdx(est1, nsims = 5, nsteps = 500, nwstats.formula = ~edges + nodefactor('race', base = 0) +
              nodematch('race') + concurrent)
dx

par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(dx)

par(mfrow = c(1,2))
plot(dx, type = 'duration')
plot(dx, type = 'dissolution')

param <- param.net(inf.prob = .1, act.rate = 5, rec.rate = .02)

status.vector <- c(rbinom(500, 1, .1), rep(0, 500))
status.vector <- ifelse(status.vector == 1, 'i', 's')
init <- init.net(status.vector = status.vector)

control <- control.net(type = 'SIS', nsteps = 500, nsims = 10, epi.by = 'race')

sim1 <- netsim(est1, param, init, control)

head(as.data.frame(sim1), 10)
head(as.data.frame(sim1, out = 'mean'), 10)

this <- get_network(sim1, sim = 1)
this

par(mfrow = c(1,1), mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim1)

plot(sim1, mean.line = F, qnts = F, sim.lines = T)

plot(sim1, y = c('si.flow','is.flow'), qnts = 1, legend = T)

plot(sim1, y = c('i.num.race0','i.num.race1'), legend = T)

#####
# example 2 #
#####

num.m1 <- 500
num.m2 <- 500
nw <- network.initialize(num.m1 + num.m2, bipartite = num.m1, directed = F)
nw <- set_vertex_attribute(nw, 'group', rep(1:2, c(num.m1, num.m2)))

deg.dist.m1 <- c(.40, .55, .04, .01)
deg.dist.m2 <- c(.48, .41, .08, .03)

pois.dists <- c(dpois(0:2, lambda = .66), ppois(2, lambda = .66, lower.tail = F))

par(mar = c(3,3,2,1), mfrow = c(1,1))
cols <- brewer_ramp(4, 'Set2')
barplot(cbind(deg.dist.m1, deg.dist.m2, pois.dists), beside = F, ylim = c(0,1), col = cols)
legend('topright', legend = paste0('deg', 3:0), pch = 15, col = rev(cols), bg = 'white')

check_degdist_bal(num.m1, num.m2, deg.dist.m1, deg.dist.m2)

formation <- ~edges + nodematch('group') + b1degree(0:1) + b2degree(0:1)
target.stats <- c(330, 0, 200, 275, 240, 205)

coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 25, d.rate = .005)
coef.diss

est2 <- netest(nw, formation, target.stats, coef.diss)

dx <- netdx(est2, nsims = 5, nsteps = 500)
dx

plot(dx, stats = 'edges')

plot(dx, stats = c('b1deg1', 'b2deg1'), sim.lines = T, sim.lwd = .4, qnts = F, mean.lwd = 4, mean.smooth = F)

plot(dx, stats = c('b1deg1', 'b2deg1', 'b1deg0','b2deg0'))


param <- param.net(inf.prob = .3, inf.prob.g2 = .1,
                   a.rate = .005, a.rate.g2 = NA, 
                   ds.rate = .005, ds.rate.g2 = .005,
                   di.rate = .005, di.rate.g2 = .005)

init <- init.net(i.num = 50, i.num.g2 = 50)
control <- control.net(type = 'SI', nsims = 5, nsteps = 500, nwstats.formula = ~edges +meandeg, delete.nodes = T)
sim2 <- netsim(est2, param, init, control)
sim2

plot(sim2, type = 'formation', plots.joined = F)

plot(sim2, popfrac = F)


#####
# my try #
#####
#####
# notes #
#####
# we want serosorting (maybe), covid disease param, find formation formula, maybe bipartite on athelete,
# no mortality or birth (might use as analagous to quarentine and outside infections), 

# scaled target network density: .112
# test degreedist with log-log plot 

# network_initialize: n = 750 ---
# set_vertix_attribute: network, 'attr', rep(values, times)
# param: inf.prob = .42, act.rate = 3(?), rec.rate = 1/14, a.rate = (per person per unit time), test.acc = , test.frac = 0 
# formation: ~edges + nodematch('athlete') + 
# target.stats: 
# coef.diss: dissolution = , duration = 88 
# init: i.num = 19, r.num = 86 (from jewell covid page) ---
# control: type = 'SIR', nsims = 5, nsteps = 117 (semester length in days), ncores = cores(8), epi.by = (?),
# save.nwstats = T(?), nwstats.formula = , attr.rules = list(status = 'i')

#####
# code #
#####



thesim <- function(tr) {
  mynw <- network_initialize(n = 750, bipartite = F)
  mynw <- set_vertex_attribute(mynw, 'race', rep(0:1, 750)) ### finish this 
  
  formation <- ~edges + nodematch('race') + concurrent ### finish this 
  
  target.stats  <- c(6800, 2000, 750) ### finish this 
  
  coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 88)
  coef.diss 
  
  est <- netest(mynw, formation, target.stats, coef.diss)
  est
  
  dx <- netdx(est, nsims = 10, nsteps = 115)
  dx
  
  param <- param.net(inf.prob = .11, act.rate = .25, rec.rate = 1/10, test.rate = tr, test.dur = 10, rec.rate.tx = 1/14,
                     inf.prob.tx = 0)
  init <- init.net(i.num = 5, r.num = 86)
  control <- control.net(NULL, nsims = 30, nsteps = 115, tnt.FUN = tnt, recovery.FUN = recov, infection.FUN = cus_infec,
                         ncores = detectCores())
  sim <- netsim(est, param, init, control)
  stopImplicitCluster()
  
  # par(mfrow = c(1,1))
  # plot(dx, stats = 'edges')
  # 
  # path <- paste0(getwd(),'/EpiPlot_',tr*700,'.png')
  # png(path)
  # plot(sim, main = paste('Testing rate of',tr*7))
  # dev.off()
  
  dat <- as.data.frame(sim, out = 'mean')
  return(dat)
  
}

record <- function(tr, dat) {
  out <- data.frame(test_rate = tr,
                    at = dat[dat$i.num == max(dat$i.num),]$time,
                    peak = dat[dat$i.num == max(dat$i.num),]$i.num)
  return(out)
}

all <- function(rates) {
  
  out <- data.frame(test_rate = numeric(),
                        at = numeric(), 
                        peak = numeric())
  
  for (i in 1:length(rates)) {
    dat <- thesim(rates[i]/7)
    out[i,]  <- record(rates[i],dat)
  }
  
  return(out)
  
}

rates1 <- c(.05,.1,.15,.2,.25,.3,.35,.4,.45,.5)
rates2 <- c(.55,.6,.65,.7,.75,.8,.85,.9,.95)
rates <- c(rates1,rates2)

results <- all(rates)

ggplot(results, aes(x = test_rate, y = peak)) +
  geom_line() +
  theme_minimal() + 
  theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
  geom_hline(yintercept = 150, linetype = 'dashed') +
  ggtitle('Peak infections vs testing rate')

#####
# network sample plot #
#####

net.sample <- network_initialize(n = 100)

formation.sample <- ~edges 

target.stats.sample <- (121)

dissolution.coef.sample <- dissolution_coefs(dissolution = ~offset(edges), duration = 50)

est.sample <- netest(net.sample, formation.sample, target.stats.sample, dissolution.coef.sample)

dx.sample <- netdx(est.sample, nsims = 1, nsteps = 100)


param.sample <- param.net(inf.prob = .9, act.rate = .25, rec.rate = 1/10, test.rate = .1, test.dur = 10, rec.rate.tx = 1/14,
                   inf.prob.tx = 0)

init.sample <- init.net(i.num = 5, r.num = 86)

control.sample <- control.net(NULL, nsims = 10, nsteps = 115, tnt.FUN = tnt, recovery.FUN = recov, infection.FUN = cus_infec,
                       ncores = detectCores())

sim.sample <- netsim(est.sample, param.sample, init.sample, control.sample)
stopImplicitCluster()

plot(sim.sample, type = 'network', legend = T, col.status = T)

plot(sim.sample, type = 'network', legend = T, col.status = T, at = 30)

plot(sim.sample)

plot(dx.sample, type = 'formation')

rm(net.sample, formation.sample, target.stats.sample, dissolution.coef.sample, est.sample, dx.sample, 
   param.sample, init.sample, control.sample, sim.sample)

#####
# function to test multiple testing rates #
#####

test.rate.sims <- function(testrate) {
  for (i in 1:length(testrate)) {
    mynw <- network_initialize(n = 750, bipartite = F)
    mynw <- set_vertex_attribute(mynw, 'race', rep(0:1, 750)) ### finish this 
    
    formation <- ~edges + nodematch('race') + concurrent ### finish this 
    
    target.stats  <- c(6800, 2000, 750)
    
    coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 88)
    coef.diss 
    
    est <- netest(mynw, formation, target.stats, coef.diss)
    est
    
    dx <- netdx(est, nsims = 10, nsteps = 115)
    dx
    
    plot(dx, stats = 'edges')
    
    param <- param.net(inf.prob = .11, act.rate = .25, rec.rate = 1/10, test.rate = testrate[i], test.dur = 10, rec.rate.tx = 1/14,
                       inf.prob.tx = 0)
    init <- init.net(i.num = 19, r.num = 86)
    control <- control.net(NULL, nsims = 10, nsteps = 115, tnt.FUN = tnt, recovery.FUN = recov, infection.FUN = cus_infec,
                           ncores = detectCores())
    sim <- netsim(est, param, init, control)
    stopImplicitCluster()
    
    pdx.edge <- recordPlot(dx, 'edges')
    pdx.duration <- recordPlot(dx, 'duration')
    pdx.dissolution  <- recordPlot(dx, 'dissolution')
    psim <- recordPlot(sim)
    
    out <- list()
    out$plots[i] <- c(pdx.edge, pdx.dissolution, pdx.duration, psim)
    out$data[i] <- sim.dat.qnt(sim)
    
    return(out)
    
  }
}

results <- list()
testrates <- c(0, .1, .2, .25, .5, .75, .9)
results <- lapply(testrates, runSim)

#####
# extractin data #
#####

sim.dat.mean <- as.data.frame(sim, out = 'mean')
mean.peak <- max(sim.dat.mean$i.num)
mean.peak

sim.dat.1 <- as.data.frame(sim, out = 'qnt', qval = 1)
peak.1 <- max(sim.dat.1$i.num)
peak.1

par(mfrow = c(1,1)) 
plot(sim)


datqnt25 <- as.data.frame(sim, out = 'qnt', qval = .25)
datqnt50 <- as.data.frame(sim, out = 'qnt', qval = .5)
datqnt75 <- as.data.frame(sim, out = 'qnt', qval = .75)

datqnt25$qnt <- 25
datqnt50$qnt <- 50
datqnt75$qnt <- 75

datqnt <- rbind(datqnt25, datqnt50, datqnt75) %>% 
  group_by(qnt)

ggplot(datqnt, aes(x = time, y = s.num, fill = qnt)) +
  geom_area()

ggplot(datqnt25, aes(x = time, y = s.num, col = 'blue')) +
  geom_line() +
  geom_line(aes(y = i.num, col = 'red')) +
  geom_line(aes(y = r.num, col = 'green')) +
  theme_bw()

ggplot(datqnt25, aes(x = time, y = s.num, position = 'stack')) +
  geom_area(aes(fill = 'red')) +
  geom_area(data = datqnt50, aes(fill = 'blue')) +
  geom_area(data = datqnt75, aes(fill = 'green')) +
  xlim(0, 20)

#####
# new module # 
#####

# what I want: its an intervention module. test x percent of pop and removed dectected positives 
# randomly select x percent of pop ---
# if they are infected, remove from the population (maybe incorporate a testing accuracey)
#   set infectivitey to 0 until moved to recovered
#   or, just remove from pop or just move straight to recovered 
#   testing acc var 

intervention <- function(dat, at, test.frac = 1, test.acc = 1) {
  
  #vars 
  # idsElig <- which(dat$attr$active == 1)
  # nElig <- length(idsElig)
  # tested <- dplyr::sample_n(idsElig, test.frac*nElig) 
  # test.frac <- dat$param$test.frac
  # test.acc <- dat$param$test.acc
  
  #process 
  # if (nElig > 0) {
  #   tested.pos <- which(tested$attr$status == 'i') 
  #   if (length(tested.pos) > 0) {
  #     dat$attr$status[tested.pos] <- 'r'
  #   }
  # }
  
  #summary stats
  # if (at == 2) {
  #   dat$epi$test.pos <- c(0, length(tested.pos))
  # } else {
  #   dat$epi$test.pos <- length(tested.pos)
  # }
  
}

#####
# testing module #
#####

tnt <- function(dat, at) {
  
  ## Uncomment this to function environment interactively
  # browser()
  
  ## Attributes ##
  active <- get_attr(dat, "active")
  
  # Initialize new Dx attr at sim start
  if (at == 2) {
    dat <- set_attr(dat, "diag.status", rep(0, length(active)))
    dat <- set_attr(dat, "diag.time", rep(NA, length(active)))
  }
  diag.status <- get_attr(dat, "diag.status")
  diag.time <- get_attr(dat, "diag.time")
  
  ## Parameters ##
  test.rate <- get_param(dat, "test.rate")
  test.dur <- get_param(dat,"test.dur")
  
  ## Determine eligible to test ##
  idsElig <- which(active == 1 & diag.status == 0)
  nElig <- length(idsElig)
  
  ## Vector of recovered IDs after stochastic process ## 
  vecTest <- which(rbinom(nElig, 1, test.rate) == 1)
  idsTest <- idsElig[vecTest]
  nTest <- length(idsTest)
  
  ## Update attribute if any tested ##
  diag.status[idsTest] <- 1
  diag.time[idsTest] <- at
  
  ## Dx lasts for test.dur weeks, then is reset ##
  ## Probably should be related to 1/rec.rate.tx ##
  idsReset <- which(at - diag.time > (test.dur - 1))
  diag.status[idsReset] <- 0
  diag.time[idsReset] <- NA
  
  ## Write out updated attributes ##
  dat <- set_attr(dat, "diag.status", diag.status)
  dat <- set_attr(dat, "diag.time", diag.time)
  
  ## Write out summary statistics ##
  dat <- set_epi(dat, "nTest", at, nTest)
  dat <- set_epi(dat, "nRest", at, length(idsReset))
  
  return(dat)
} 

#####
# new reovery module #
#####

recov <- function(dat, at) {
  
  ## Uncomment this to function environment interactively
  # browser()
  
  ## Attributes ##
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  diag.status <- get_attr(dat, "diag.status")
  diag.time <- get_attr(dat, "diag.time")
  
  ## Parameters ##
  rec.rate <- get_param(dat, "rec.rate")
  rec.rate.tx <- get_param(dat, "rec.rate.tx")

  ## Determine eligible to recover ##
  idsElig <- which(active == 1 & status == "i")
  nElig <- length(idsElig)
  
  ## Determined Dx status of eligible ##
  diag.status.elig <- diag.status[idsElig]
  
  ## Recovery rates dependent on Dx status ##
  ratesElig <- ifelse(diag.status.elig == 1, rec.rate.tx, rec.rate)

  ## Vector of recovered IDs after stochastic process
  vecRecov <- which(rbinom(nElig, 1, ratesElig) == 1)
  idsRecov <- idsElig[vecRecov]
  nRecov <- length(idsRecov)
  
  ## Update attributes if any recovered ##
  status[idsRecov] <- "r"
  diag.status[idsRecov] <- 0
  diag.time[idsRecov] <- NA
  
  ## Write out updated attributes ##
  dat <- set_attr(dat, "status", status)
  dat <- set_attr(dat, "diag.status", diag.status)
  dat <- set_attr(dat, "diag.time", diag.time)
  
  ## Write out summary statistics ##
  dat <- set_epi(dat, "ir.flow", at, nRecov)
  dat <- set_epi(dat, 'r.num', at, sum(status == 'r'))
  
  
  return(dat)
}

#####
# custom infection module #
#####

cus_infec <- function(dat, at) {
  
  ## get attributes ## 
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  infTime <- get_attr(dat, "infTime")
  diag.status <- get_attr(dat, 'diag.status')
  
  ## get paramaters ## 
  inf.prob <- get_param(dat, "inf.prob")
  act.rate <- get_param(dat, "act.rate")
  inter.eff <- get_param(dat, "inter.eff", override.null.error = TRUE)
  inter.start <- get_param(dat, "inter.start", override.null.error = TRUE)
  inf.prob.tx <- get_param(dat, 'inf.prob.tx')
  
  ## get infected and eligible ## 
  idsInf <- which(active == 1 & status == "i")
  nActive <- sum(active == 1)
  nElig <- length(idsInf)
  nInf <- 0
  
  #if (at > 1) {
      
    ## get infection prob and act rate ##
    if (nElig > 0 && nElig < nActive) {
      del <- discord_edgelist(dat, at, network = 1)
      if (!(is.null(del))) {
        del$infDur <- at - infTime[del$inf]
        del$infDur[del$infDur == 0] <- 1
        del$diag.status <- diag.status[del$inf]
        linf.prob <- length(inf.prob)
        #
        linf.prob.tx <- length(inf.prob.tx)
        del$transProb <- ifelse(del$diag.status == 0,
          ifelse(del$infDur <= linf.prob, inf.prob[del$infDur], inf.prob[linf.prob]),
          ifelse(del$infDur <= linf.prob.tx, inf.prob.tx[del$infDur], inf.prob.tx[linf.prob.tx]))
        #
        if (!is.null(inter.eff) && at >= inter.start) {
          del$transProb <- del$transProb * (1 - inter.eff)
        }
        lact.rate <- length(act.rate)
        del$actRate <- ifelse(del$infDur <= lact.rate, act.rate[del$infDur], 
                              act.rate[lact.rate])
        del$finalProb <- 1 - (1 - del$transProb)^del$actRate
        
        ## infection sim ##
        transmit <- rbinom(nrow(del), 1, del$finalProb)
        
        ## update attibutes ## 
        del <- del[which(transmit == 1), ]
        idsNewInf <- unique(del$sus)
        
        ## write out updated attributes ## 
        status <- get_attr(dat, "status")
        status[idsNewInf] <- "i"
        dat <- set_attr(dat, "status", status)
        infTime[idsNewInf] <- at
        dat <- set_attr(dat, "infTime", infTime)
        nInf <- length(idsNewInf)
      }
    }
  #}
  ## save tranmission matrix ##
  if (nInf > 0) {
    dat <- set_transmat(dat, del, at)
  }
  ## write out summary stats ##
  dat <- set_epi(dat, "si.flow", at, nInf)
  return(dat)
}

#####
# intervention testing #
#####
testnet <- network.initialize(n = 1000, directed = F) 

testform <- ~edges

test.targets <-  c(1200) 

test.coefs <- dissolution_coefs(dissolution = ~offset(edges), duration = 25)
test.coefs

test.est <- netest(testnet, testform, test.targets, test.coefs)
test.est

testdx <- netdx(test.est, nsims = 5, nsteps = 100)
testdx

par(mfrow = c(1,1))
plot(testdx, stats = 'edges')

test.param <- param.net(inf.prob = .42, act.rate = .5, rec.rate = 1/14, test.rate = .1, test.dur = 14, rec.rate.tx = 1/14,
                        inter.eff = .9)
test.init <- init.net(i.num = 300, r.num = 5)
test.control <- control.net(NULL, nsims = 2, nsteps = 200, tnt.FUN = tnt, recovery.FUN = recov,
                            infection.FUN = infection.net)
test.sim <- netsim(test.est, test.param, test.init, test.control)
stopImplicitCluster()

test.param.2 <- param.net(inf.prob = .42, act.rate = .5, rec.rate = 1/14, test.rate = .1, test.dur = 14, rec.rate.tx = 1/14,
                          inter.eff = .5)
test.control.2 <- control.net(NULL, nsims = 2, nsteps = 200, tnt.FUN = tnt, recovery.FUN = recov,
                            infection.FUN = infection.net)
test.sim.2 <- netsim(test.est, test.param.2, test.init, test.control.2)
stopImplicitCluster()

test.control.3 <- control.net(NULL, nsims = 2, nsteps = 200, tnt.FUN = tnt, recovery.FUN = recov,
                              infection.FUN = infection.net)
test.param.3 <- param.net(inf.prob = .42, act.rate = .5, rec.rate = 1/14, test.rate = .1, test.dur = 14, rec.rate.tx = 1/14)
test.sim.3 <- netsim(test.est, test.param.3, test.init, test.control)
stopImplicitCluster()

par(mfrow = c(1,3))
plot(test.sim)
plot(test.sim.2)
plot(test.sim.3)

test.sim.dat <- as.data.frame(test.sim, out = 'mean')
test.sim.2.dat <- as.data.frame(test.sim.2, out = 'mean')

#####
# fucking counting #
#####
count = 0
lens = 200
end = 2
for (y in 1:end) {
  for (i in 1:lens) {
    for (x in 1:lens) {
      print(round((x + ((i - 1)*lens) + ((y - 1)*lens*lens))/(lens*lens*end),2));
    }
  }
}
count

#####
# idek #
#####
rm(tester)
tester <- data.frame(degree = numeric(),
                     y = numeric())
for (i in 1:length(degdisttest)) tester[i,1] <- i
for (i in 1:length(degdisttest)) tester[i,2] <- degdisttest[i]

tester

ggplot(tester, aes(x = degree, y = y)) +
  geom_jitter() +
  scale_y_continuous(trans = 'log') +
  scale_x_continuous(trans = 'log')


#####
# workspace #
#####

rm(ish)
rm(dat)
dat <- 
for (at in 1:3) {
  if (at == 1) {
    dat <- initialize.net(est, param, init, control, NULL)
  } else {
    dat <- tnt(dat,at)
    dat <- cus_infec(dat,at)
    dat <- recov(dat,at)
    dat <- nwupdate.net(dat,at)
    dat <- prevalence.net(dat,at)
    dat <- verbose.net(dat,at)
  }
  ish <- dat
}
ish 

del <- discord_edgelist(maybe, 2, network = 1)

idselig <- which(maybe$attr$active == 1)

diagstat <- ifelse(rbinom(idselig, 1, .1) == 1, 1, 0)
inftime <- get_attr(maybe, 'infTime')

at <- 2
del$infdur <- at - inftime[del$inf]
del$diagstatus <- diagstat[del$inf]
infprob <- maybe$param$inf.prob
infprobtx <- maybe$param$inf.prob.tx

linfprob <- length(infprob)
linfprobtx <- length(infprobtx)

del$tranprob <- ifelse(del$diagstat == 0, 
       ifelse(infdur <= linfprob, infprob[infdur], infprob[linfprob]),
       ifelse(infdur <= linfprobtx, infprobtx[infdur], infprobtx[linfprobtx]))
test
length(test)

