library(data.table)
library(ggplot2)

dta <- fread("C:/Users/ambuehll/Desktop/odd_data/data/delay_data/fahrzeitensollist2015092020150926.csv")


line80 <- dta[linie==80 &  halt_kurz_von1 == "GLAU" & richtung == 2 & datum_von=="21.09.15"]


setorder(line80, soll_an_von)
line80[, diff:= ist_an_von-shift(ist_an_von)]

tt <- line80[,c("fahrzeug", "soll_an_von", "ist_an_von","diff")][soll_an_von>18000&diff>0&diff<60]


hist(tt$diff)



#--- general approach


line80 <- dta[linie==80 & datum_von=="21.09.15" & richtung==2]

line80 <- dta[linie==80 & richtung==2]


setorder(line80, soll_an_von)
line80[, diff:= ist_an_von-shift(ist_an_von), by=.(halt_kurz_von1, datum_von)]

tt <- line80[,c("fahrzeug", "soll_an_von", "ist_an_von","diff","halt_kurz_von1", "seq_von","kurs","fahrt_id","datum_von")][soll_an_von>18000&diff>0]

#tt[,fahrt_id:=paste0(datum_von, fahrt_id)]

setorder(tt, seq_von)
tt[seq_von==28]$fahrt_id
full <- data.table(unique(cbind(tt[fahrt_id== 435552]$halt_kurz_von1,tt[fahrt_id== 435552]$seq_von)))
fullf <- factor(full$V1,levels = full$V1, ordered = T)
tt$fullf <- factor(tt$halt_kurz_von1,levels = full$V1, ordered = T)


tt[,fahrt_iddate:= paste(fahrt_id, datum_von)]

filter <- unique(tt[diff<180]$fahrt_iddate)
subtt <- tt[fahrt_iddate %in% filter]




# stopsdv<- tt[diff<180,.(sddiff= sd(diff,na.rm = T)), by= seq_von]


ggplot(data= subtt, aes(fullf, diff, color=factor(fahrt_id)))+
geom_path(size=2)

oneday <- subtt[datum_von=="21.09.15"]
# 
# ggplot(data = subtt, aes(x = fullf, y = diff, color = factor(fahrt_id), group = factor(fahrt_id))) +
#   geom_line(size=1.1)
# 
# ggplot(data = subtt, aes(x = fullf, y = diff, group = factor(fahrt_id))) +
#   geom_line(size=1.1)

ggplot(data = subtt, aes(x = fullf, y = diff, group = factor(paste(fahrt_id, datum_von)))) +
  geom_line(size=1.1)

ggplot(data = oneday, aes(x = fullf, y = diff, group = factor(fahrt_id))) +
  geom_line(size=1.1)


###---- 


hist(subtt$ist_an_von)

ggplot(data = subtt[ist_an_von >50000], aes(x = fullf, y = diff, group = factor(paste(fahrt_id, datum_von)))) +
  geom_line(size=1.1)

ggplot(data = subtt[ist_an_von <50000], aes(x = fullf, y = diff, group = factor(paste(fahrt_id, datum_von)))) +
  geom_line(size=1.1)

