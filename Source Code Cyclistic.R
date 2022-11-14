library(tidyverse)     # dasar library untuk data analisis
library(readxl)        # untuk membaca file excel
library(lubridate)     # untuk format data waktu
library(ggplot2)       # untuk ploting data
library(here)          # untuk cleaning data
library(janitor)       # dasar library untuk data analisis
library(skimr)         # dasar library untuk data analisis
library(gghighlight)   # untuk highlight plot
library(reshape2)      # for manipulasi data
library(geosphere)     # untuk hitung geo distance
library(RColorBrewer)  # untuk gradasi warna

#upload data
Trip_202108 <- read_excel("D:/Coursera/Dataset/new_trip_data/202108-tripdata.xlsx")
Trip_202109 <- read_excel("D:/Coursera/Dataset/new_trip_data/202109-tripdata.xlsx")
Trip_202110 <- read_excel("D:/Coursera/Dataset/new_trip_data/202110-tripdata.xlsx")
Trip_202111 <- read_excel("D:/Coursera/Dataset/new_trip_data/202111-tripdata.xlsx")
Trip_202112 <- read_excel("D:/Coursera/Dataset/new_trip_data/202112-tripdata.xlsx")
Trip_202201 <- read_excel("D:/Coursera/Dataset/new_trip_data/202201-tripdata.xlsx")
Trip_202202 <- read_excel("D:/Coursera/Dataset/new_trip_data/202202-tripdata.xlsx")
Trip_202203 <- read_excel("D:/Coursera/Dataset/new_trip_data/202203-tripdata.xlsx")
Trip_202204 <- read_excel("D:/Coursera/Dataset/new_trip_data/202204-tripdata.xlsx")
Trip_202205 <- read_excel("D:/Coursera/Dataset/new_trip_data/202205-tripdata.xlsx")
Trip_202206 <- read_excel("D:/Coursera/Dataset/new_trip_data/202206-tripdata.xlsx")
Trip_202207 <- read_excel("D:/Coursera/Dataset/new_trip_data/202207-tripdata.xlsx")

#gabung data
data_trip <- rbind(Trip_202108,Trip_202109,Trip_202110,Trip_202111,Trip_202112,Trip_202201,Trip_202202,Trip_202203,Trip_202204,Trip_202205,Trip_202206,Trip_202207)

#hapus data satuan
rm(Trip_202108,Trip_202109,Trip_202110,Trip_202111,Trip_202112,Trip_202201,Trip_202202,Trip_202203,Trip_202204,Trip_202205,Trip_202206,Trip_202207)

summary(data_trip)

data_trip_clean <- drop_na(data_trip)
summary(data_trip_clean)

skim_without_charts(data_trip_clean)     # deskripsi data

data_durasi <- data_trip_clean %>%
  select(duration, customer_type)

# summarize data_durasi
data_durasi %>% 
  group_by(customer_type) %>% 
  summarize(mean_duration = mean(duration),
            variance = var(duration),
            min_duration = min(duration),
            q1_duration = quantile(duration,0.25),
            median_duration = median(duration),
            q3_duration = quantile(duration,0.75),
            max_duration = max(duration),
            total_duration = sum(duration),
            total_data = n())

# boxplot durasi
ggplot(data = data_durasi,
       mapping = aes(x=duration, y=customer_type, fill=customer_type)) +
  geom_boxplot() +
  labs(title = "Distribusi Durasi",
       subtitle = "Per Menit Berdasarkan Tipe Pengguna",
       x = "Durasi (Menit)",
       y = "Tipe Pengguna") +
  scale_fill_manual(values=c("#34A853","#FBBC05")) +
  theme(plot.title = element_text(hjust=.5),
        plot.subtitle = element_text(hjust=.5),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(0,1440,60),
                     labels=seq(0,1440,60),
                     limits=c(0,360))

#histogram durasi
ggplot(data=data_durasi,
       mapping=aes(x=duration, fill=factor(customer_type, levels=c("casual","member")))) + 
  geom_histogram(breaks=seq(0,360,1),
                 position="identity",
                 alpha=.5) +
  scale_x_continuous(breaks=seq(0,360,30),
                     labels=seq(0,360,30)) +
  labs(title="Distribusi Durasi",
       subtitle="Berdasarkan Tipe Pengguna",
       x="Durasi (Menit)",
       y="Total",
       fill="Tipe Pengguna") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_fill_manual(values=c("#34A853","#FBBC05"))

#filter durasi
data_durasi_clean <- data_durasi %>% 
  filter(duration <= 120)

data_durasi_banding <- data.frame(data=c("Data Durasi <=120 Menit","Data Durasi >120 Menit"),
                                  data_count=c(length(data_durasi_clean$duration),
                                               length(data_durasi$duration)-length(data_durasi_clean$duration)))
data_durasi_banding$fraction <- data_durasi_banding$data_count / sum(data_durasi_banding$data_count)
data_durasi_banding$ymax <- cumsum(data_durasi_banding$fraction)
data_durasi_banding$ymin <- c(0, head(data_durasi_banding$ymax, n=-1))
data_durasi_banding$labelPosition <- (data_durasi_banding$ymax + data_durasi_banding$ymin)/2
data_durasi_banding$label <- paste0(data_durasi_banding$data, "\n Persentase: ", round(data_durasi_banding$fraction*100,2),"%")
ggplot(data=data_durasi_banding, mapping=aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=data)) +
  geom_rect() +
  geom_text(x=2, aes(y=labelPosition, label=label, color=data), size=3.5) +
  labs(title="Perbandingan Data Durasi") +
  scale_fill_manual(values=c("#4285F4","#EA4335")) +
  scale_color_manual(values=c("#4285F4","#EA4335")) +
  coord_polar(theta="y") +
  xlim(c(0,4)) +
  theme_void()

data_jarak <- data_trip_clean %>%
  select(start_lat, start_lng, end_lat, end_lng, customer_type)

# hitung jarak
data_jarak <- data_jarak %>% 
  mutate(distance = round(distHaversine(cbind(data_jarak$start_lng,data_jarak$start_lat),
                                        cbind(data_jarak$end_lng, data_jarak$end_lat)),digits=5))

# summarize jarak
data_jarak %>% 
  group_by(customer_type) %>% 
  summarize(mean_distance = mean(distance),
            variance = var(distance),
            min_distance = min(distance),
            q1_distance = quantile(distance,0.25),
            median_distance = median(distance),
            q3_distance = quantile(distance,0.75),
            max_distance = max(distance),
            total_distance = sum(distance),
            total_data = n())

# boxplot jarak
ggplot(data = data_jarak,
       mapping = aes(x=distance, y=customer_type, fill=customer_type)) +
  geom_boxplot() +
  labs(title = "Distribusi Jarak",
       subtitle = "Berdasarkan Tipe Pengguna",
       x = "Jarak (Meter)",
       y = "Tipe Pengguna") +
  theme(plot.title = element_text(hjust=.5),
        plot.subtitle = element_text(hjust=.5),
        axis.text.x=element_text(angle=90),
        legend.position = "none") +
  scale_fill_manual(values=c("#34A853","#FBBC05")) +
  scale_x_continuous(breaks=seq(0,13000,1000),
                     labels=seq(0,13000,1000),
                     limits=c(0,13000))

#histogram jarak
ggplot(data = data_jarak,
       mapping = aes(x=distance, fill=factor(customer_type, levels=c("casual","member")))) +
  geom_histogram(breaks=seq(0,13000,1000),
                 position = "identity",
                 alpha = .5) +
  scale_x_continuous(breaks=seq(0,13000,1000),
                     labels=seq(0,13000,1000)) +
  labs(title = "Distribusi Jarak",
       subtitle = "Berdasarkan Tipe Pengguna",
       x = "Jarak (Meter)",
       y="Total",
       fill="Tipe Pengguna") +
  theme(plot.title = element_text(hjust=.5, size=20),
        plot.subtitle = element_text(hjust=.5),
        axis.text.x=element_text(angle=90)) +
  scale_fill_manual(values=c("#34A853","#FBBC05"))

data_jarak_clean <- data_jarak %>%
  filter(distance <= 12000)

data_jarak_banding <- data.frame(data=c("Data Jarak <=12000 M","Data Jarak >12000 M"),
                                 data_count=c(length(data_jarak_clean$distance),
                                              length(data_jarak$distance)-length(data_jarak_clean$distance)))
data_jarak_banding$fraction <- data_jarak_banding$data_count / sum(data_jarak_banding$data_count)
data_jarak_banding$ymax <- cumsum(data_jarak_banding$fraction)
data_jarak_banding$ymin <- c(0, head(data_jarak_banding$ymax, n=-1))
data_jarak_banding$labelPosition <- (data_jarak_banding$ymax + data_jarak_banding$ymin)/2
data_jarak_banding$label <- paste0(data_jarak_banding$data, "\n Persentase: ", round(data_jarak_banding$fraction*100,2),"%")
ggplot(data=data_jarak_banding, mapping=aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=data)) +
  geom_rect() +
  geom_text(x=2, aes(y=labelPosition, label=label, color=data), size=3.5) +
  labs(title="Perbandingan Data Jarak") +
  scale_fill_manual(values=c("#4285F4","#EA4335")) +
  scale_color_manual(values=c("#4285F4","#EA4335")) +
  coord_polar(theta="y") +
  xlim(c(0,4)) +
  theme_void()

# buat kolom jarak
data_trip_clean <- data_trip_clean %>% 
  mutate(distance = round(distHaversine(cbind(data_trip_clean$start_lng,data_trip_clean$start_lat),
                                        cbind(data_trip_clean$end_lng, data_trip_clean$end_lat)),digits=5))

#filter data
data_trip_clean <- data_trip_clean %>%
  filter(distance <= 12000,
         duration <= 120)

#summary data
summary(data_trip_clean)

ggplot(data=data_trip_clean,
       mapping=aes(x=duration, fill=customer_type)) + 
  geom_histogram(breaks=seq(0,120,1),
                 position="identity",
                 alpha=.5) +
  scale_x_continuous(breaks=seq(0,120,10),
                     labels=seq(0,120,10)) +
  labs(title="Distribusi Durasi",
       subtitle="Berdasarkan Tipe Pengguna",
       x="Durasi (Menit)",
       y="Total",
       fill="Tipe Pengguna") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_fill_manual(values=c("#34A853","#FBBC05")) +
  geom_vline(xintercept=16, linetype="dashed", color="#EA4335", size=1) +
  annotate("text", x=50, y=150000, label="Rata - rata Durasi 16 Menit", color="black", size=4)

# buat objek plot
data_durasi_1 <- ggplot(data=data_trip_clean, mapping=aes(x=duration, fill=customer_type)) +
  geom_histogram(breaks=seq(0,120,16), position="identity")
data_durasi_2 <- ggplot(data=data_trip_clean, mapping=aes(x=duration, fill=customer_type)) +
  geom_histogram(breaks=seq(16,120,104), position="identity")
# ambil objek plot menggunakan ggplot_build
hitung_data_durasi_1 <- ggplot_build(data_durasi_1)$data[[1]]$count
hitung_data_durasi_2 <- ggplot_build(data_durasi_2)$data[[1]]$count
# hasil hitung untuk pengguna member dan casual
casual_1 <- hitung_data_durasi_1[1]
casual_2 <- hitung_data_durasi_2[1]
member_1 <- hitung_data_durasi_1[8]
member_2 <- hitung_data_durasi_2[2]
# summary durasi
durasi_summary = data.frame(time_frame=c("0-16 Menit","0-16 Menit","16-120 Menit","16-120 Menit"),
                            customer_type=c("casual","member","casual","member"),
                            total=c(casual_1,member_1,casual_2,member_2),
                            percentage=c(round((100*casual_1/(casual_1+casual_2)),1),
                                         round((100*member_1/(member_1+member_2)),1),
                                         round((100*casual_2/(casual_1+casual_2)),1),
                                         round((100*member_2/(member_1+member_2)),1)))
# clear data yg tidak terpakai
rm(data_durasi_1,data_durasi_2, hitung_data_durasi_1,hitung_data_durasi_2,
   casual_1,casual_2,member_1,member_2)
#barplot durasi
ggplot(data=durasi_summary, mapping=aes(x=time_frame, y=total, fill=customer_type)) +
  geom_bar(stat="identity") +
  facet_wrap(~customer_type) +
  geom_text(aes(label=total), vjust=-0.5, fontface="bold", size=6) +
  geom_text(aes(label=paste(percentage,"%")), vjust=1.5, color="white", fontface="bold", size=6) +
  labs(title="Jumlah Pengguna dan Durasi",
       subtitle="Berdasarkan Tipe Pengguna",
       x="Durasi",
       y="Jumlah Pengguna",
       fill="Tipe Pengguna") +
  scale_y_continuous(limits=c(0,3250000)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values=c("#34A853","#FBBC05"))

ggplot(data=data_trip_clean,
       mapping=aes(x=distance, fill=customer_type)) + 
  geom_histogram(breaks=seq(0,12000,1000),
                 position="identity",
                 alpha=.5) +
  scale_x_continuous(breaks=seq(0,12000,1000),
                     labels=seq(0,12000,1000)) +
  labs(title="Distribusi Jarak",
       subtitle="Berdasarkan Tipe Pengguna",
       x="Jarak (Meter)",
       y="Total",
       fill="Tipe Pengguna") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x=element_text(angle=90)) +
  scale_fill_manual(values=c("#34A853","#FBBC05")) +
  geom_vline(xintercept=2000, linetype="dashed", color="#EA4335", size=1) +
  annotate("text", x=5000, y=750000, label="Rata - rata Jarak 2.000 Meter", color="black", size=4)

# buat objek plot
data_jarak_1 <- ggplot(data=data_trip_clean, mapping=aes(x=distance, fill=customer_type)) +
  geom_histogram(breaks=seq(0,12000,2000), position="identity")
data_jarak_2 <- ggplot(data=data_trip_clean, mapping=aes(x=distance, fill=customer_type)) +
  geom_histogram(breaks=seq(2000,12000,10000), position="identity")
# ambil objek plot menggunakan ggplot_build
hitung_data_jarak_1 <- ggplot_build(data_jarak_1)$data[[1]]$count
hitung_data_jarak_2 <- ggplot_build(data_jarak_2)$data[[1]]$count
# hasil hitung untuk pengguna member dan casual
casual_1 <- hitung_data_jarak_1[1]
casual_2 <- hitung_data_jarak_2[1]
member_1 <- hitung_data_jarak_1[7]
member_2 <- hitung_data_jarak_2[2]
# summary durasi
jarak_summary = data.frame(time_frame=c("0-2000 Meter","0-2000 Meter","2000-12000 Meter","2000-12000 Meter"),
                           customer_type=c("casual","member","casual","member"),
                           total=c(casual_1,member_1,casual_2,member_2),
                           percentage=c(round((100*casual_1/(casual_1+casual_2)),1),
                                        round((100*member_1/(member_1+member_2)),1),
                                        round((100*casual_2/(casual_1+casual_2)),1),
                                        round((100*member_2/(member_1+member_2)),1)))
# clear data yg tidak terpakai
rm(data_jarak_1,data_jarak_2, hitung_data_jarak_1,hitung_data_jarak_2,
   casual_1,casual_2,member_1,member_2)

ggplot(data=jarak_summary, mapping=aes(x=time_frame, y=total, fill=customer_type)) +
  geom_bar(stat="identity") +
  facet_wrap(~customer_type) +
  geom_text(aes(label=total), vjust=-0.5, fontface="bold", size=6) +
  geom_text(aes(label=paste(percentage,"%")), vjust=1.5, color="white", fontface="bold", size=6) +
  labs(title="Jumlah Pengguna dan Jarak",
       subtitle="Berdasarkan Tipe Pengguna",
       x="Jarak",
       y="Jumlah Pengguna",
       fill="Tipe Pengguna") +
  scale_y_continuous(limits=c(0,3250000)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values=c("#34A853","#FBBC05"))

data_hari <- data_trip_clean %>%
  group_by(customer_type, day_of_week) %>% 
  summarise(value=n()/52)

#fungsi konversi nama hari
day_number <- function(day_name){
  num = c()
  for (day_of_week in day_name){
    if (day_of_week=="Senin"){
      num <- append(num,1)
    } else if (day_of_week=="Selasa"){
      num <- append(num,2)
    } else if (day_of_week=="Rabu"){
      num <- append(num,3)
    } else if (day_of_week=="Kamis"){
      num <- append(num,4)
    } else if (day_of_week=="Jumat"){
      num <- append(num,5)
    } else if (day_of_week=="Sabtu"){
      num <- append(num,6)
    } else if (day_of_week=="Minggu"){
      num <- append(num,7)
    }
  }
  return(num)
}

#buat kolom id untuk nama hari
data_hari$id <- day_number(data_hari$day_of_week)

ggplot(data=data_hari, mapping=aes(x=id, y=value, color=customer_type))+
  geom_line(size=1) +
  geom_point(size=3) +
  theme_bw() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7),
                     labels=c("Senin", "Selasa", "Rabu", "Kamis", "Jumat", "Sabtu", "Minggu")) +
  scale_y_continuous(breaks=seq(0,15000,3000),
                     limits=c(0,15000)) +
  scale_color_manual(values=c("#34A853","#FBBC05")) +
  labs(title="Rata - rata Penggunaan Per Hari",
       subtitle="Berdasarkan Tipe Pengguna",
       color="Tipe Pengguna",
       x="Hari",
       y="Rata - rata Pengguna") +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor=element_blank()) +
  geom_vline(xintercept=1, linetype="dashed", color="black", size=0.5) +
  geom_vline(xintercept=5, linetype="dashed", color="black", size=0.5) +
  geom_vline(xintercept=7, linetype="dashed", color="black", size=0.5) +
  annotate("text", x=3, y=12000, label="Hari Biasa", color="black", size=5) +
  annotate("text", x=6, y=12000, label="Hari Libur", color="black", size=5)

data_bulan <- data_trip_clean %>%
  group_by(customer_type, month_year) %>% 
  summarise(value=n()/12)

#fungsi konversi nama bulan
month_year_number <- function(month_year_name){
  num = c()
  for (month_year in month_year_name){
    if (month_year=="Aug-21"){
      num <- append(num,1)
    } else if (month_year=="Sep-21"){
      num <- append(num,2)
    } else if (month_year=="Oct-21"){
      num <- append(num,3)
    } else if (month_year=="Nov-21"){
      num <- append(num,4)
    } else if (month_year=="Dec-21"){
      num <- append(num,5)
    } else if (month_year=="Jan-22"){
      num <- append(num,6)
    } else if (month_year=="Feb-22"){
      num <- append(num,7)
    } else if (month_year=="Mar-22"){
      num <- append(num,8)
    } else if (month_year=="Apr-22"){
      num <- append(num,9)
    } else if (month_year=="May-22"){
      num <- append(num,10)
    } else if (month_year=="Jun-22"){
      num <- append(num,11)
    } else if (month_year=="Jul-22"){
      num <- append(num,12)
    }
  }
  return(num)
}

#buat kolom id untuk bulan
data_bulan$id <- month_year_number(data_bulan$month_year)

ggplot(data=data_bulan, mapping=aes(x=id, y=value, color=customer_type))+
  geom_line(size=1) +
  geom_point(size=3) +
  theme_bw() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("Agus-21", "Sep-21", "Okt-21", "Nov-21", "Des-21", "Jan-22", "Feb-22", "Mar-22", "Apr-22", "Mei-22", "Jun-22", "Jul-22")) +
  scale_y_continuous(breaks=seq(0,35000,5000),
                     limits=c(0,35000)) +
  scale_color_manual(values=c("#34A853","#FBBC05")) +
  labs(title="Rata - rata Penggunaan Per Bulan",
       subtitle="Berdasarkan Tipe Pengguna",
       color="Tipe Pengguna",
       x="Bulan",
       y="Rata - rata Pengguna") +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x=element_text(angle=30),
        panel.grid.minor=element_blank()) +
  geom_vline(xintercept=2, linetype="dashed", color="black", size=0.5) +
  geom_vline(xintercept=5, linetype="dashed", color="black", size=0.5) +
  geom_vline(xintercept=8, linetype="dashed", color="black", size=0.5) +
  geom_vline(xintercept=11, linetype="dashed", color="black", size=0.5) +
  annotate("text", x=1.3, y=33000, label="Musim\nPanas", color="black", size=3.5) +
  annotate("text", x=3.5, y=33000, label="Musim\nGugur", color="black", size=3.5) +
  annotate("text", x=6.5, y=33000, label="Musim\nDingin", color="black", size=3.5) +
  annotate("text", x=9.5, y=33000, label="Musim\nSemi", color="black", size=3.5) +
  annotate("text", x=11.8, y=33000, label="Musim\nPanas", color="black", size=3.5)

data_waktu <- data_trip_clean %>%
  select(customer_type,started_at)
#buat kolom jam
data_waktu <- data_waktu %>%
  mutate(started_hour = hour(data_waktu$started_at))
#summary
waktu_summary <- data_waktu %>%
  group_by(customer_type, started_hour) %>% 
  summarise(count=n()/365)

ggplot(data=waktu_summary, mapping=aes(x=started_hour, y=count, color=customer_type))+
  geom_point(size=3)+
  geom_line(size=1)+
  theme_bw() +
  scale_x_continuous(breaks=seq(0,23,1))+
  scale_y_continuous(breaks=seq(0,1000,100),
                     limits=c(0,1000)) + 
  scale_color_manual(values=c("#34A853","#FBBC05")) +
  labs(title="Rata - rata Pengguna Per Waktu",
       subtitle="Berdasarkan Tipe Pengguna",
       color="Tipe Pengguna",
       x="Jam",
       y="Rata - rata Pengguna") +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor=element_blank()) +
  geom_vline(xintercept=8, linetype="dashed", color="black", size=0.5) +
  geom_vline(xintercept=17, linetype="dashed", color="black", size=0.5) +
  geom_label(x=8, y=650, label="Jam Sibuk", color="black", size=5) +
  geom_label(x=17, y=950, label="Jam Sibuk", color="black", size=5) + 
  annotate("rect", xmin=7, xmax=9, ymin=-Inf, ymax=Inf, alpha=.1, fill="#34A853") +
  annotate("rect", xmin=16, xmax=18, ymin=-Inf, ymax=Inf, alpha=.1, fill="#34A853")

data_stasiun_mulai <- data_trip_clean%>%
  group_by(start_station_name) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:20)

ggplot(data = data_stasiun_mulai, mapping = aes(x = reorder(start_station_name, total), y = total)) + 
  geom_bar(stat = "identity", fill = "#34A853")+ 
  scale_y_continuous(breaks=seq(0,80000,20000),
                     limits=c(0,80000)) +
  labs(title = "Top 20 Stasiun Awal Penyewaan",
       subtitle="Berdasarkan Total Pengguna",
       x = "Stasiun",
       y = "Total Pengguna")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor=element_blank()) + 
  geom_text(aes(label = formatC(total, big.mark = ",", decimal.mark = ".", format = "d")), size = 3.5, hjust = -0.1) +
  coord_flip()

data_stasiun_akhir <- data_trip_clean%>%
  group_by(end_station_name) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:20)

ggplot(data = data_stasiun_akhir, mapping = aes(x = reorder(end_station_name, total), y = total)) + 
  geom_bar(stat = "identity", fill = "#FBBC05")+ 
  scale_y_continuous(breaks=seq(0,80000,20000),
                     limits=c(0,80000)) +
  labs(title = "Top 20 Stasiun Akhir Penyewaan",
       subtitle="Berdasarkan Total Pengguna",
       x = "Stasiun",
       y = "Total Pengguna")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor=element_blank()) + 
  geom_text(aes(label = formatC(total, big.mark = ",", decimal.mark = ".", format = "d")), size = 3.5, hjust = -0.1) +
  coord_flip()

join_station <- data_stasiun_mulai %>% 
  inner_join(data_stasiun_akhir, by=c("start_station_name"="end_station_name")) %>% 
  mutate(avg_start = round(total.x/365,0), 
         avg_end = round(total.y/365,0),
         total = (total.x + total.y)) %>%
  arrange(desc(total))

ggplot(data = join_station, mapping = aes(x = reorder(start_station_name, total), y = total)) + 
  geom_bar(stat = "identity", fill = "#4285F4")+ 
  scale_y_continuous(breaks=seq(0,180000,60000),
                     limits=c(0,180000)) +
  labs(title = "Top 20 Stasiun",
       subtitle="Berdasarkan Total Pengguna",
       x = "Stasiun",
       y = "Total Pengguna")+
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor=element_blank()) + 
  geom_text(aes(label = formatC(total, big.mark = ",", decimal.mark = ".", format = "d")), size = 3.5, hjust = -0.1) +
  coord_flip()