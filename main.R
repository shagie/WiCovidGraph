library(ggplot2)
library(scales)
library(zoo)

data <- read.csv("data.csv")
# Test data for [16] has negative values - interpolate the positive ones
data$TEST_NEW[16] <- (data$TEST_NEW[15] + data$TEST_NEW[17]) / 2
data$NEG_NEW[16] <- (data$TEST_NEW[16] - data$POS_NEW[16])
date <- as.Date(data$LoadDttm, format("%Y/%m/%d %H:%M:%S+00"))

ptests <- data$POS_NEW
ntests <- data$NEG_NEW
ttests <- data$TEST_NEW

# Scale for second Y axis based on 1/6th of the total
tmax <- max(ttests, na.rm = TRUE) * 6

test_totals <- data.frame(
  "date" = rep(date, 2),
  "test" = c(ptests, ntests),
  "label" = c(rep("Positive", length(date)), rep("Negative", length(date))),
  "ratio" = rep(tmax * ptests / ttests, 2)
)

combo_plot <- ggplot(test_totals, aes(x = date)) +
  geom_bar(stat = "identity", aes(y = test, fill = label)) +
  geom_point(aes(y = ratio)) +
  geom_line(aes(y = rollmean(ratio, 7, fill = NA))) +
  scale_y_continuous(
    sec.axis = sec_axis(
      trans = ~. / (tmax),
      name = "Positive tests",
      labels = scales::percent_format(accuracy = 1)
    ),
    breaks = seq(0, max(ttests, na.rm = TRUE), by = 2000),
    labels = scales::number_format(scale = 0.001, accuracy = 1),
    name = "Total tests (thousands)"
  ) +
  scale_x_date(
    name = "Day",
    labels = date_format("%m/%d"),
    breaks = date_breaks("week")
  ) +
  # Labels
  labs(
    title = "Wisconsin Covid Testing",
    fill = "Test Results"
  )

combo_plot
