setwd("C:/Users/djwem/OneDrive - Nanyang Technological University/Projects/PhD_Free-roaming-dogs/PhD/ch.2_mla-rf/acc-thr")
gps1 = read.csv("dog_wang_singapore-5710-annotated-gpsv2.csv")
gps2 = read.csv("dog3_wang_singapore-5710-annotated-gps2.csv"  )

acc1 = read.csv("dog_wang_singapore-5710-annotated-samplesv2.csv")
acc2 = read.csv("dog3_wang_singapore-5710-annotated-samples2.csv")


gps = rbind(gps1, gps2)
acc = rbind(acc1, acc2)

gps$sg.time = as.POSIXct(gps$timestamp)
acc$sg.time = as.POSIXct(acc$burst.start.timestamp)
acc$burst = rep(1:(nrow(acc)/26), each = 26)
acc$index = rep(1:26, length = nrow(acc))
acc.n = acc %>%
  select(burst, index, sg.time, acceleration.x, acceleration.y, acceleration.z) %>%
  rename(X = acceleration.x,
         Y = acceleration.y,
         Z = acceleration.z)

acc.n.var.col.names = acc.n %>%
  filter(index == "1") %>%
  select(burst, index, sg.time)

acc.n.var = acc.n %>%
  group_by(burst) %>%
  summarise(var.x = var(X),
            var.y = var(Y),
            var.z = var(Z))

acc.n.var = cbind(acc.n.var, acc.n.var.col.names)



gps = gps %>%
  select(sg.time, location_long, location_lat) %>%
  rename(longitude = location_long,
         latitude = location_lat)

for (i in 1:nrow(gps)) {
  gps$time.diff[i] = difftime(gps$sg.time[i+1], gps$sg.time[i], units = "secs")
}

gps$time.diff[29] = 60
acc.n.var$burst = NULL

match = read.csv('2020-07-21_acc-thr_gps-cleaned.csv') # hand-cleaned gps data
match$burst = match$X

ggplot(match, aes(x = burst)) +
  geom_line(aes(y = var.x), color = 'red') + 
  geom_line(aes(y = var.y), color = 'green') + 
  geom_line(aes(y = var.z), color = 'blue') + 
  geom_point(aes(y = time.diff3*50000), color = 'black')















