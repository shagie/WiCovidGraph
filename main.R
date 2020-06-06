library(ggplot2)
library(scales)
library(zoo)

data <- read.csv("data.csv")
data[16,] <- NA # negative tests/day ?
date <- as.Date(data$LoadDttm, format("%Y/%m/%d %H:%M:%S+00"))

ptests <- data$POS_NEW
ntests <- data$NEG_NEW
ttests <- data$TEST_NEW

tmax <- max(ttests, na.rm = TRUE) * 6

test_totals <- data.frame(
  "date" = rep(date, 2),
  "test" = c(ptests, ntests),
  "label" = c(rep("pos", length(date)), rep("neg", length(date))),
  "ratio" = rep(tmax * ptests / ttests, 2)
)

head(test_totals)

p <- ggplot(test_totals, aes(x = date))
p <- p + geom_bar(stat = "identity", aes(y = test, fill = label))
p <- p + geom_point(aes(y = ratio))
p <- p + geom_line(aes(y = rollmean(ratio, 7, fill = NA)))
p <- p + scale_y_continuous(
  sec.axis = sec_axis(trans = ~. / (tmax / 100), name = "% Positive tests")
)
p <- p + scale_x_date(
  name = "Day",
  labels = date_format("%m/%d"),
  breaks = date_breaks("week")
)

p