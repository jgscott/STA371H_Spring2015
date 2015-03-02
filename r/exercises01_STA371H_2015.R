# Load the heights data set using "Import Dataset" button

# 3A: MHGT looks like a slightly stronger predictor, visually
plot(SHGT ~ FHGT, data=heights)
plot(SHGT ~ MHGT, data=heights)

# 3B
mean(heights$SHGT)
sd(heights$SHGT)
