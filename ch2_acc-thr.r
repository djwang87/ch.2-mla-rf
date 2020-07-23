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

<<<<<<< HEAD
=======


>>>>>>> 22-07-2020
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



```{r identifing the sequential threshold value}
move.seq = df.all %>%
  filter(behaviour %in% c("forage", "walk", "run"))

still.seq = df.all %>%
  filter(behaviour %in% c("lie", "sit", "stand"))


runs = rle(still.seq$var.z > 10000) 
# rle() identifies ($values) and computes the number of time ($length) that value exceeds 10,000
# in the "still" behaviour dataset
# This arbitary value plucked from the table above. Axis Z is likely representing the surge axis. This is to prevent the postural changes of the dogs from activating the acc-thr of the collar in non-movement based behaviours such as barking, or lying.

myruns = which(runs$values == TRUE & runs$lengths > 5) 
# a dataframe that indicates the arguments set above; $value >10000 and $length > 2 (meaning happens more than twice in a row). 

any(myruns)

runs.lengths.cumsum = cumsum(runs$lengths)
ends = runs.lengths.cumsum[myruns] 
# To find the 'end' of each sequence as defined by myruns's arguments
# which indices more than 10,000 and the duration of that sequence.

newindex = ifelse(myruns>1, myruns-1,0)
starts = runs.lengths.cumsum[newindex] + 1

diff = starts - ends


move.runs = rle(move.seq$var.z > 10000) 

move.myruns = which(move.runs$values == TRUE & move.runs$lengths > 2) 
# a dataframe that indicates the arguments set above; $value >10000 and $length > 2 (meaning happens more than twice in a row). 

any(move.myruns)

move.runs.lengths.cumsum = cumsum(move.runs$lengths)
move.ends = move.runs.lengths.cumsum[move.myruns] 
# To find the 'end' of each sequence as defined by myruns's arguments
# which indices more than 10,000 and the duration of that sequence.

move.newindex = ifelse(move.myruns>1, move.myruns-1,0)
move.starts = move.runs.lengths.cumsum[move.newindex] + 1

diff = starts - ends






still.thr = list()
still.thr.runs = list()
still.length.cumsum = list()
still.ends = list()
still.starts = list()
still.index = list()
for (i in 1:5) {
  still.thr[[i]] = rle(still.seq$var.z > i*5000)
  still.thr.runs[[i]] = which(still.thr[[i]]$values == TRUE & still.thr[[i]]$lengths >= 5)
  still.length.cumsum[[i]] = cumsum(still.thr[[i]]$length)
  still.index[[i]] = ifelse(still.thr.runs[[i]]>1, still.thr.runs[[i]]-1,0) + 1
  still.ends[[i]] = cumsum(still.thr[[i]]$length)[still.thr.runs[[i]]]
  still.starts[[i]] = still.length.cumsum[[i]][still.index[[i]]] + 1
}


print(still.seq$var.z[starts[1]:ends[1]])

```












