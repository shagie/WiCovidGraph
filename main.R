library(ggplot2)
library(scales)
library(zoo)

data <- read.csv("data.csv")
# Test data for [16] has negative values - interpolate the positive ones
data$TEST_NEW[16] <- (data$TEST_NEW[15] + data$TEST_NEW[17]) / 2
data$NEG_NEW[16] <- (data$NEG_NEW[15] + data$NEG_NEW[17]) / 2
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

p <- ggplot(test_totals, aes(x = date)) +
  geom_bar(stat = "identity", aes(y = test, fill = label)) +
  geom_point(aes(y = ratio)) +
  geom_line(aes(y = rollmean(ratio, 7, fill = NA))) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~. / (tmax), name = "Positive tests", labels = percent),
  ) +
  scale_x_date(
    name = "Day",
    labels = date_format("%m/%d"),
    breaks = date_breaks("week")
  ) +
  # Labels
  labs(
    title = "Wisconsin Covid Testing"
  ) +
  ylab("Total tests")

p