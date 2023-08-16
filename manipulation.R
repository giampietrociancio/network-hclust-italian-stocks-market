# filter data according to "from" and "to" variable# arrange in alphabetical order
from <- as.Date("2016-01-01")
to <- as.Date("2021-02-01")
time.vector <- seq(from, to, by="days")

# pivot the table and arrange in alphabetical order
data <- stock.prices %>% arrange(symbol) %>% group_by(symbol)  %>% pivot_wider(names_from = symbol, values_from = adjusted) %>% filter(date %in% time.vector) %>% select(-date)

# create a function to compute standardized log-returns and filter out NAs 
logreturn <- function(x) {
  df <- tibble(.rows = (nrow(x)-1))
  for (i in 1:length(x)) {
    df[,i] <- diff(log(x[[i]])) #compute log returns
    names(df)[i] <- names(x[i])
  }
  df <- df[,colSums(is.na(df))==0] #filter out NA returns
  df <- df %>% mutate_all(~(scale(.) %>% as.vector)) #standardized values (Mantegna, Stanley 1999)
}

# apply the function
returns <- logreturn(data)