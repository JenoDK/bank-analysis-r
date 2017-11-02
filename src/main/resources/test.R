# Generate some sample data, then compute mean and standard deviation
# in each group
df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)
ds <- plyr::ddply(df, "gp", plyr::summarise, mean = mean(y), sd = sd(y))

plot_this <- function(data1, data2) {
  
  pdf(paste("/Users/jeno/Downloads/", "testPlot", ".pdf", sep = ""))
  
  # The summary data frame ds is used to plot larger red points on top
  # of the raw data. Note that we don't need to supply `data` or `mapping`
  # in each layer because the defaults from ggplot() are used.
  ggplot(df, aes(gp, y)) +
    geom_point() +
    geom_point(data = ds, aes(y = mean), colour = 'red', size = 3) +
    ggtitle("First plot")
  
  # Same plot as above, declaring only the data frame in ggplot().
  # Note how the x and y aesthetics must now be declared in
  # each geom_point() layer.
  ggplot(df) +
    geom_point(aes(gp, y)) +
    geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
    ggtitle("Second plot")
  
}

plot_this(df, ds)

dev.off()

