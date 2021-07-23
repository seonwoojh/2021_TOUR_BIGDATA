library(data.table)
library(tidyverse)
library(bit64)
library(reshape)
library(factoextra)
library(GGally)
library(cluster)

# 2020년도 국민 여행조사
setwd("C:/Users/Stat1317_01/Desktop/2021 문화관광 공모전/데이터")
list.files()

tour_survey20 <- fread("(원자료) 2020년 국민여행조사_국내여행.csv")


# 필요변수 선택
tour_survey20 <- tour_survey20 %>% select(ID, SA1_1, SA1_2, SA1_4,
                                          D_TRA1_1_SPOT,
                                          D_TRA1_2_SPOT,
                                          D_TRA1_3_SPOT,
                                          D_TRA1_4_SPOT,
                                          starts_with("A5_"))
tour_survey20 <- tour_survey20[,-c(26:29)]

# 실제 관광객 추출
tour_survey20_tourist <- tour_survey20 %>% 
  filter(SA1_1 != 0 | SA1_2 != 0 | SA1_4 != 0) %>%
  select(-starts_with("SA1"))

# ID를 기준으로 여행방문지역 melt
tour_survey20_melt.dat <- melt(data=tour_survey20_tourist,
                               id.vars = "ID",
                               measure.vars = c("D_TRA1_1_SPOT",
                                                "D_TRA1_2_SPOT",
                                                "D_TRA1_3_SPOT",
                                                "D_TRA1_4_SPOT"))

# 활동변수 추출 및 전처리
tour_survey20_activity <- 
  tour_survey20_tourist %>% 
  select(starts_with("A5"))

tour_survey20_activity <- 
  data.frame(ifelse(is.na(tour_survey20_activity),0,1))
tour_survey20_activity <- cbind(tour_survey20_tourist$ID,
                                tour_survey20_activity
)
names(tour_survey20_activity)[1] <- "ID"


# tour_survey20_melt.dat과 tour_survey20_activity ID  기준으로 결합
tour_survey20_join.dat <- left_join(tour_survey20_melt.dat[,-2],
                                    tour_survey20_activity,
                                    by = "ID")


# 같은 ID의 중복 방문지역 제거
tour_survey20_join.dat <- unique(tour_survey20_join.dat) 
tour_survey20_join.dat <- 
  tour_survey20_join.dat %>% 
  filter(tour_survey20_join.dat$value !=
           is.na(tour_survey20_join.dat$value))


# 지역코드를 기준으로 aggreagte
# 각 방문 지역에서의 활동에 대한 빈도수 데이터 생성
tour20_area_activity <- aggregate(. ~ value,
                                  data = tour_survey20_join.dat[,-1],
                                  sum)

names(tour20_area_activity)[1] <- "SPOT"

# 2019년도 국민 여행조사
tour_survey19 <- fread("2019_국민여행조사_국내여행.csv")

tour_survey19 <- tour_survey19 %>% select(ID, MON_EXP_1, MON_EXP_2,
                                          MON_EXP_4, MON_EXP_6,
                                          D_TRA1_1_SPOT1,
                                          D_TRA1_1_SPOT2,
                                          D_TRA1_1_SPOT3,
                                          D_TRA1_1_SPOT4,
                                          starts_with("A3_"))

tour_survey19 <- tour_survey19[,-c(27,28,29,30)]


# 실제관광객 추출
tour_survey19_tourist <- 
  tour_survey19 %>% 
  filter(MON_EXP_1 != 2 | MON_EXP_2 != 2 | 
           MON_EXP_4 != 2 | MON_EXP_6 != 2) %>% 
  select(-starts_with("MON"))

# ID를 기준으로 여행방문지 melt
tour_survey19_melt.dat <- melt(data = tour_survey19_tourist,
                               id.vars = "ID",
                               measure.vars = c("D_TRA1_1_SPOT1",
                                                "D_TRA1_1_SPOT2",
                                                "D_TRA1_1_SPOT3",
                                                "D_TRA1_1_SPOT4"))

# 황동변수 추출 및 전처리
tour_survey19_activity <- 
  tour_survey19_tourist %>% 
  select(starts_with("A3"))

tour_survey19_activity <- 
  data.frame(ifelse(is.na(tour_survey19_activity),0,1))
tour_survey19_activity <- cbind(tour_survey19_tourist$ID,
                                tour_survey19_activity
)
names(tour_survey19_activity)[1] <- "ID"

# melt한 데이터와 bind할 데이터 생성
tour_survey19_join.dat <- left_join(tour_survey19_melt.dat[,-2],
                                    tour_survey19_activity,
                                    by = "ID")

# 같은 ID의 중복 방문지역 제거
tour_survey19_join.dat <- unique(tour_survey19_join.dat) 
tour_survey19_join.dat <- 
  tour_survey19_join.dat %>% 
  filter(tour_survey19_join.dat$value !=
           is.na(tour_survey19_join.dat$value))



# 지역코드를 기준으로 aggreagte
tour19_area_activity <- aggregate(. ~ value,
                                  data = tour_survey19_join.dat[,-1],
                                  sum)

# tour20_fin의 변수명과 동일하게 변경
names(tour19_area_activity) <- names(tour20_area_activity)

# 코드북에 존재하지 않는 지역코드 제거
tour19_area_activity <- tour19_area_activity %>% filter(SPOT != 25000)

# 2018 국민 여행조사
tour_survey18 <- fread("2018 국민여행조사_국내여행.csv")

tour_survey18 <- tour_survey18 %>% select(ID, MON_EXP_DOM_TOUR, D_TRA1_1_TOWN1,
                                          D_TRA1_1_TOWN2, D_TRA1_1_TOWN3,
                                          D_TRA1_2_TOWN1,
                                          starts_with("A3_"))
tour_survey18 <- tour_survey18[,-c(23,24,25,26)]

# 실제관광객 추출
tour_survey18_tourist <- 
  tour_survey18 %>% 
  filter(MON_EXP_DOM_TOUR == 1) %>% 
  select(-MON_EXP_DOM_TOUR) %>%
  filter(D_TRA1_1_TOWN1 != 99999)

# ID를 기준으로 여행방문지 melt
tour_survey18_melt.dat <- melt(data=tour_survey18_tourist,
                               id.vars = "ID",
                               measure.vars = c("D_TRA1_1_TOWN1",
                                                "D_TRA1_1_TOWN2",
                                                "D_TRA1_1_TOWN3",
                                                "D_TRA1_2_TOWN1"))

# 황동변수 추출 및 전처리
tour_survey18_activity <- 
  tour_survey18_tourist %>% 
  select(starts_with("A3_"))

tour_survey18_activity <- 
  data.frame(ifelse(is.na(tour_survey18_activity),0,1))
tour_survey18_activity <- cbind(tour_survey18_tourist$ID,
                                tour_survey18_activity)

names(tour_survey18_activity)[1] <- "ID"

# tour18_melt와 act19 열을 기준으로 결합
tour_survey18_join.dat <- left_join(tour_survey18_melt.dat[,-2],
                                    tour_survey18_activity,
                                    by = "ID")


### 같은 ID의 중복 방문지역 제거
# 각 ID별 방문지역에서의 활동 표시데이터 생성
tour_survey18_join.dat <- unique(tour_survey18_join.dat) 
tour_survey18_join.dat <- 
  tour_survey18_join.dat %>% 
  filter(tour_survey18_join.dat$value !=
           is.na(tour_survey18_join.dat$value))




### 지역코드를 기준으로 aggreagte
# 각 방문 지역에서의 활동에 대한 도수분포표 생성
tour18_area_activity <- aggregate(. ~ value,
                                  data = tour_survey18_join.dat[,-1],
                                  sum)
names(tour18_area_activity) <- names(tour20_area_activity)

# 지역코드(SPOT)을 기준으로 merge
area_code <- data.frame(tour20_area_activity[,1])
names(area_code) <- "SPOT"
tour18_area_activity <- merge(area_code,
                              tour18_area_activity,
                              all = T,    
                              by = "SPOT")


# 결측값(NA)에 0값 부여
tour18_area_activity[is.na(tour18_area_activity)] <- 0

### 지역이름
NAME <-c("서울특별시 종로구",
         "서울특별시 중구",
         "서울특별시 용산구",
         "서울특별시 성동구",
         "서울특별시 광진구",
         "서울특별시 동대문구",
         "서울특별시 중랑구",
         "서울특별시 성북구",
         "서울특별시 강북구",
         "서울특별시 도봉구",
         "서울특별시 노원구",
         "서울특별시 은평구",
         "서울특별시 서대문구",
         "서울특별시 마포구",
         "서울특별시 양천구",
         "서울특별시 강서구",
         "서울특별시 구로구",
         "서울특별시 금천구",
         "서울특별시 영등포구",
         "서울특별시 동작구",
         "서울특별시 관악구",
         "서울특별시 서초구",
         "서울특별시 강남구",
         "서울특별시 송파구",
         "서울특별시 강동구",
         "부산광역시 중구",
         "부산광역시 서구",
         "부산광역시 동구",
         "부산광역시 영도구",
         "부산광역시 부산진구",
         "부산광역시 동래구",
         "부산광역시 남구",
         "부산광역시 북구",
         "부산광역시 해운대구",
         "부산광역시 사하구",
         "부산광역시 금정구",
         "부산광역시 강서구",
         "부산광역시 연제구",
         "부산광역시 수영구",
         "부산광역시 사상구",
         "부산광역시 기장군",
         "대구광역시 중구",
         "대구광역시 동구",
         "대구광역시 서구",
         "대구광역시 남구",
         "대구광역시 북구",
         "대구광역시 수성구",
         "대구광역시 달서구",
         "대구광역시 달성군",
         "인천광역시 중구",
         "인천광역시 동구",
         "인천광역시 미추홀구",
         "인천광역시 연수구",
         "인천광역시 남동구",
         "인천광역시 부평구",
         "인천광역시 계양구",
         "인천광역시 서구",
         "인천광역시 강화군",
         "인천광역시 옹진군",
         "광주광역시 동구",
         "광주광역시 서구",
         "광주광역시 남구",
         "광주광역시 북구",
         "광주광역시 광산구",
         "대전광역시 동구",
         "대전광역시 중구",
         "대전광역시 서구",
         "대전광역시 유성구",
         "대전광역시 대덕구",
         "울산광역시 중구",
         "울산광역시 남구",
         "울산광역시 동구",
         "울산광역시 북구",
         "울산광역시 울주군",
         "세종특별자치시 세종시",
         "경기도 수원시",
         "경기도 성남시",
         "경기도 의정부시",
         "경기도 안양시",
         "경기도 부천시",
         "경기도 광명시",
         "경기도 평택시",
         "경기도 동두천시",
         "경기도 안산시",
         "경기도 고양시",
         "경기도 과천시",
         "경기도 구리시",
         "경기도 남양주시",
         "경기도 오산시",
         "경기도 시흥시",
         "경기도 군포시",
         "경기도 의왕시",
         "경기도 하남시",
         "경기도 용인시",
         "경기도 파주시",
         "경기도 이천시",
         "경기도 안성시",
         "경기도 김포시",
         "경기도 화성시",
         "경기도 광주시",
         "경기도 양주시",
         "경기도 포천시",
         "경기도 여주시",
         "경기도 연천군",
         "경기도 가평군",
         "경기도 양평군",
         "강원도 춘천시",
         "강원도 원주시",
         "강원도 강릉시",
         "강원도 동해시",
         "강원도 태백시",
         "강원도 속초시",
         "강원도 삼척시",
         "강원도 홍천군",
         "강원도 횡성군",
         "강원도 영월군",
         "강원도 평창군",
         "강원도 정선군",
         "강원도 철원군",
         "강원도 화천군",
         "강원도 양구군",
         "강원도 인제군",
         "강원도 고성군",
         "강원도 양양군",
         "충청북도 충주시",
         "충청북도 제천시",
         "충청북도 청주시",
         "충청북도 보은군",
         "충청북도 옥천군",
         "충청북도 영동군",
         "충청북도 진천군",
         "충청북도 괴산군",
         "충청북도 음성군",
         "충청북도 단양군",
         "충청북도 증평군",
         "충청남도 천안시",
         "충청남도 공주시",
         "충청남도 보령시",
         "충청남도 아산시",
         "충청남도 서산시",
         "충청남도 논산시",
         "충청남도 계룡시",
         "충청남도 당진시",
         "충청남도 금산군",
         "충청남도 부여군",
         "충청남도 서천군",
         "충청남도 청양군",
         "충청남도 홍성군",
         "충청남도 예산군",
         "충청남도 태안군",
         "전라북도 전주시",
         "전라북도 군산시",
         "전라북도 익산시",
         "전라북도 정읍시",
         "전라북도 남원시",
         "전라북도 김제시",
         "전라북도 완주군",
         "전라북도 진안군",
         "전라북도 무주군",
         "전라북도 장수군",
         "전라북도 임실군",
         "전라북도 순창군",
         "전라북도 고창군",
         "전라북도 부안군",
         "전라남도 목포시",
         "전라남도 여수시",
         "전라남도 순천시",
         "전라남도 나주시",
         "전라남도 광양시",
         "전라남도 담양군",
         "전라남도 곡성군",
         "전라남도 구례군",
         "전라남도 고흥군",
         "전라남도 보성군",
         "전라남도 화순군",
         "전라남도 장흥군",
         "전라남도 강진군",
         "전라남도 해남군",
         "전라남도 영암군",
         "전라남도 무안군",
         "전라남도 함평군",
         "전라남도 영광군",
         "전라남도 장성군",
         "전라남도 완도군",
         "전라남도 진도군",
         "전라남도 신안군",
         "경상북도 포항시",
         "경상북도 경주시",
         "경상북도 김천시",
         "경상북도 안동시",
         "경상북도 구미시",
         "경상북도 영주시",
         "경상북도 영천시",
         "경상북도 상주시",
         "경상북도 문경시",
         "경상북도 경산시",
         "경상북도 군위군",
         "경상북도 의성군",
         "경상북도 청송군",
         "경상북도 영양군",
         "경상북도 영덕군",
         "경상북도 청도군",
         "경상북도 고령군",
         "경상북도 성주군",
         "경상북도 칠곡군",
         "경상북도 예천군",
         "경상북도 봉화군",
         "경상북도 울진군",
         "경상북도 울릉군",
         "경상남도 진주시",
         "경상남도 통영시",
         "경상남도 사천시",
         "경상남도 김해시",
         "경상남도 밀양시",
         "경상남도 거제시",
         "경상남도 양산시",
         "경상남도 창원시",
         "경상남도 의령군",
         "경상남도 함안군",
         "경상남도 창녕군",
         "경상남도 고성군",
         "경상남도 남해군",
         "경상남도 하동군",
         "경상남도 산청군",
         "경상남도 함양군",
         "경상남도 거창군",
         "경상남도 합천군",
         "제주특별자치도 제주시",
         "제주특별자치도 서귀포시")
NAME <- data.frame(NAME)

### 18,19,20년도 여행지 활동 데이터 
tour_area_activity <- tour20_area_activity[,-1] + 
  tour19_area_activity[,-1] + 
  tour18_area_activity[,-1]
rownames(tour_area_activity) <- tour20_area_activity[,1]
colnames(tour_area_activity) <- c("자연/풍경감상",
                                  "음식관광",
                                  "야외위락/스포츠/레포츠활동",
                                  "역사유적지방문",
                                  "테마파크/놀이시설/동/식물원방문",
                                  "휴식/휴양",
                                  "온천/스파",
                                  "쇼핑",
                                  "지역문화예술/공연/전시시설관람",
                                  "스포츠경기관람",
                                  "지역축제/이벤트참가",
                                  "교육/체험프로그램참가",
                                  "종교/성지순례",
                                  "카지노/경마/경륜 등",
                                  "시티투어",
                                  "드라마촬영지방문",
                                  "유흥/오락")

### 각 활동별 활동자 수 총합 계산 후 오름차순으로 정렬
tour_activity_mean <- data.frame(sort(apply(tour_area_activity,2,mean)))
tour_activity_mean <- rownames_to_column(tour_activity_mean,
                                         var = "activity")
names(tour_activity_mean)[2] <- "mean"
tour_activity_mean$mean <- round(tour_activity_mean$mean, 2)

tour_activity_mean$colour <- ifelse(tour_activity_mean$mean < 10,
                                    "#63E2FA", "#F54847")

ggplot(tour_activity_mean,aes(x=reorder(activity,-mean), y=mean)) +
  geom_bar(stat = "identity", fill=tour_activity_mean$colour) + 
  geom_text(aes(label=mean), vjust=-0.5, size=4) +
  xlab("") + 
  ylab("") +
  theme(axis.text.x = element_text(angle=45, hjust=1,colour="black",
                                   face="bold"))


# 활동자 수의 총합이 작다고 판단되는 변수 제거
# 2000명 이하
tour_area_activity_select <- tour_area_activity[,-c(7,10,12,13,14,17)]

# 열이름(활동명), 지역명 지정
tour_area_activity_select <- rownames_to_column(tour_area_activity_select,
                                                var = "CODE")
tour_area_activity_select <- cbind(NAME, tour_area_activity_select[,-1])
tour_area_activity_select <-
  data.frame(column_to_rownames(as_tibble(tour_area_activity_select),
                                var = "NAME"))
names(tour_area_activity_select) <- 
  gsub("[.]", "/", names(tour_area_activity_select))

#광역시, 특별시, 특별자치시, 특별자치도 제외
test.dat <- tour_area_activity_select[-c(1:75,228,229),]
test.dat_proportion <- data.frame(prop.table(as.matrix(test.dat),1))

set.seed(1234)

test.dat_tot_withinss <- c()
for (i in 1:15){
  test.dat_km1 <- kmeans(test.dat_proportion, centers = i, iter.max = 1000)
  test.dat_tot_withinss[i] <- test.dat_km1$tot.withinss
}
test.dat_elbow <- data.frame(test.dat_tot_withinss, group = 1)
test.dat_elbow <- rownames_to_column(test.dat_elbow,
                                     var = "cluster")
names(test.dat_elbow)[2] <- "score"

ggplot(test.dat_elbow, aes(x = reorder(cluster,-score), y=score, group=1)) + 
  geom_line(col = "Blue", size = 0.5) + 
  xlab("Number of cluster") + 
  ylab("Total within mean of square") + 
  labs(title = "Optimal number of clusters")


# 군집분석
test.dat_km1 <- kmeans(test.dat_proportion, 4)

test.dat_km2 <- kmeans(test.dat_proportion, 5)

# 실루엣
test.dat_sil1 <- silhouette(test.dat_km1$cluster, dist(test.dat_proportion))
test.dat_sil2 <- silhouette(test.dat_km2$cluster, dist(test.dat_proportion))

fviz_silhouette(test.dat_sil1)
fviz_silhouette(test.dat_sil2)

##############################################################################

test.dat_activity_mean <- data.frame(sort(apply(test.dat,2,mean)))
test.dat_activity_mean <- rownames_to_column(test.dat_activity_mean,
                                             var = "activity")
names(test.dat_activity_mean)[2] <- "mean"
test.dat_activity_mean$mean <- round(test.dat_activity_mean$mean, 2)

test.dat_activity_mean$colour <- ifelse(test.dat_activity_mean$mean>=120,
                                        "#63E2FA", "#F54847")
test.dat_activity_mean$colour <- ifelse(test.dat_activity_mean$mean<=12,
                                        "#92cd52",
                                        test.dat_activity_mean$colour)

ggplot(test.dat_activity_mean, aes(x=reorder(activity,-mean), y=mean, 
                                   fill = colour)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=mean), vjust=-0.5, size=4) +
  theme(axis.text.x = element_text(angle=45, hjust=1,
                                   colour="black", face="bold"),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = "none") +
  xlab("") +
  ylab("") 


##############################################################################

tour_top <- test.dat[,c(1,2,6)]
top_prop <- data.frame(prop.table(as.matrix(tour_top), 1))

##############################################################################

tour_mid <- test.dat[,c(3,4,5,7)]
mid_prop <- data.frame(prop.table(as.matrix(tour_mid), 1))

##############################################################################

tour_bot <- test.dat[,c(8,9,10,11)]
bot_prop <- data.frame(prop.table(as.matrix(tour_bot), 1))
bot_prop <- bot_prop %>% filter(bot_prop != is.na(bot_prop))


set.seed(123456)
### top_prop 군집분석

top_tot_withinss <- c()
for (i in 1:15){
  top_km1 <- kmeans(top_prop, centers = i, iter.max = 1000)
  top_tot_withinss[i] <- top_km1$tot.withinss
}
top_elbow <- data.frame(top_tot_withinss, group = 1)
top_elbow <- rownames_to_column(top_elbow,
                                var = "cluster")
names(top_elbow)[2] <- "score"

ggplot(top_elbow, aes(x = reorder(cluster,-score), y=score, group=1)) + 
  geom_line(col = "Blue", size = 0.5) +
  xlab("Number of cluster") + 
  ylab("Total within sum of square") + 
  labs(title = "Optimal number of clusters") +
  theme_gray() + 
  geom_line(size=1.5, color = "blue")

# 군집분석
set.seed(123456)
top_km1 <- kmeans(top_prop, 2)
set.seed(123456)
top_km2 <- kmeans(top_prop, 3)
set.seed(123456)
top_km3 <- kmeans(top_prop, 4)

# 실루엣
top_sil1 <- silhouette(top_km1$cluster, dist(top_prop))
top_sil2 <- silhouette(top_km2$cluster, dist(top_prop))
top_sil3 <- silhouette(top_km3$cluster, dist(top_prop))

fviz_silhouette(top_sil1)
fviz_silhouette(top_sil2)
fviz_silhouette(top_sil3)

# 군집 시각화
fviz_cluster(top_km1, top_prop)
fviz_cluster(top_km2, top_prop)
fviz_cluster(top_km3, top_prop)

top_cluster.dat <- top_prop
top_cluster.dat$cluster <- as.character(top_km3$cluster)

top_cluster.dat_summary <- aggregate(.~cluster, data = top_cluster.dat, mean)

top_plot <- ggparcoord(data = top_cluster.dat_summary,
                       columns = c(2:4),
                       groupColumn = "cluster",
                       scale = "globalminmax",
                       line) +
  geom_line(size=2)

top_plot

##############################################################################
#elbow

set.seed(123)
mid_tot_withinss <- c()
for (i in 1:15){
  mid_km1 <- kmeans(mid_prop, centers = i, iter.max = 1000)
  mid_tot_withinss[i] <- mid_km1$tot.withinss
}
mid_elbow <- data.frame(mid_tot_withinss, group = 1)
mid_elbow <- rownames_to_column(mid_elbow,
                                var = "cluster")
names(mid_elbow)[2] <- "score"

ggplot(mid_elbow, aes(x = reorder(cluster,-score), y=score, group=1)) + 
  geom_line(col = "Blue", size = 0.5) +
  xlab("Number of cluster") + 
  ylab("Total within sum of square") + 
  labs(title = "Optimal number of clusters") +
  theme_gray()+ 
  geom_line(size=1.5, color = "blue")

# 군집분석
set.seed(123)
mid_km1 <- kmeans(mid_prop, 3)
set.seed(123)
mid_km2 <- kmeans(mid_prop, 4)
set.seed(123)
mid_km3 <- kmeans(mid_prop, 5)

# 실루엣
mid_sil1 <- silhouette(mid_km1$cluster, dist(mid_prop))
mid_sil2 <- silhouette(mid_km2$cluster, dist(mid_prop))
mid_sil3 <- silhouette(mid_km3$cluster, dist(mid_prop))

fviz_silhouette(mid_sil1)
fviz_silhouette(mid_sil2)
fviz_silhouette(mid_sil3)

# 군집 시각화
fviz_cluster(mid_km1, mid_prop)
fviz_cluster(mid_km2, mid_prop)
fviz_cluster(mid_km3, mid_prop)


mid_cluster.dat <- mid_prop
mid_cluster.dat$cluster <- mid_km2$cluster

mid_cluster.dat$cluster[mid_cluster.dat$cluster == 1] = "A"
mid_cluster.dat$cluster[mid_cluster.dat$cluster == 2] = "B"
mid_cluster.dat$cluster[mid_cluster.dat$cluster == 3] = "C"
mid_cluster.dat$cluster[mid_cluster.dat$cluster == 4] = "D"

mid_cluster.dat_summary <- aggregate(.~cluster, data = mid_cluster.dat, mean)

mid_plot <- ggparcoord(data = mid_cluster.dat_summary,
                       columns = c(2:5),
                       groupColumn = "cluster",
                       scale = "globalminmax") +
  geom_line(size=2)
mid_plot

##############################################################################

set.seed(12345)

bot_tot_withinss <- c()
for (i in 1:15){
  bot_km1 <- kmeans(bot_prop, centers = i, iter.max = 1000)
  bot_tot_withinss[i] <- bot_km1$tot.withinss
}
bot_elbow <- data.frame(bot_tot_withinss, group = 1)
bot_elbow <- rownames_to_column(bot_elbow,
                                var = "cluster")
names(bot_elbow)[2] <- "score"

ggplot(bot_elbow, aes(x = reorder(cluster,-score), y=score, group=1)) + 
  geom_line(col = "Blue", size = 0.5) + 
  xlab("Number of cluster") + 
  ylab("Total within sum of square") + 
  labs(title = "Optimal number of clusters")+
  theme_gray() + 
  geom_line(size=2, color = "blue") + 
  geom_line(size=1.5, color = "blue")

# 군집분석
set.seed(12345)
bot_km1 <- kmeans(bot_prop, 3)
set.seed(12345)
bot_km2 <- kmeans(bot_prop, 4)
set.seed(12345)
bot_km3 <- kmeans(bot_prop, 5)


# 실루엣
bot_sil1 <- silhouette(bot_km1$cluster, dist(bot_prop))
bot_sil2 <- silhouette(bot_km2$cluster, dist(bot_prop))
bot_sil3 <- silhouette(bot_km3$cluster, dist(bot_prop))

fviz_silhouette(bot_sil1) 
fviz_silhouette(bot_sil2)
fviz_silhouette(bot_sil3)

fviz_cluster(bot_km1, bot_prop)
fviz_cluster(bot_km2, bot_prop)
fviz_cluster(bot_km3, bot_prop)

bot_cluster.dat <- bot_prop
bot_cluster.dat$cluster <- bot_km2$cluster

bot_cluster.dat$cluster[bot_cluster.dat$cluster == 1] = "a"
bot_cluster.dat$cluster[bot_cluster.dat$cluster == 2] = "b"
bot_cluster.dat$cluster[bot_cluster.dat$cluster == 3] = "c"
bot_cluster.dat$cluster[bot_cluster.dat$cluster == 4] = "d"

bot_cluster.dat_summary <- aggregate(.~cluster, data = bot_cluster.dat, mean)


bot_plot <- ggparcoord(data = bot_cluster.dat_summary,
                       columns = c(2:5),
                       groupColumn = "cluster",
                       scale = "globalminmax") + 
  geom_line(size=2)
bot_plot

set.seed(123)
library("maptools")
library("rgdal")



map_code <- fread("CODE.csv")

# top : 4
# mid : 4
# bot : 4
top_map_cluster <- rownames_to_column(top_cluster.dat,
                                      var = "NAME")
mid_map_cluster <- rownames_to_column(mid_cluster.dat,
                                      var = "NAME")
bot_map_cluster <- rownames_to_column(bot_cluster.dat,
                                      var = "NAME")

map.dat <- merge(map_code,top_map_cluster, by = "NAME", all = T)
map.dat <- merge(map.dat,mid_map_cluster, by = "NAME", all = T)
map.dat <- merge(map.dat,bot_map_cluster, by = "NAME", all = T)

map.dat <- map.dat %>% select(SIG_CD, starts_with("cluster"))
map.dat[is.na(map.dat)] <- "0"

map.dat$clustersum <- paste(map.dat$cluster.x,
                            map.dat$cluster.y,
                            map.dat$cluster,
                            sep = "-")


map.dat_final <- map.dat[,c(1,5)]
names(map.dat_final)[2] <- "cluster"


korea <- readOGR("TL_SCCO_SIG.shp")
korea_map <- fortify(korea)
korea_map_info <- korea@data

#
korea_map_info[, "id"] = (1:nrow(korea_map_info))-1
korea_map_info[, "SIDO"] = as.numeric(substr(korea_map_info$SIG_CD,
                                             start = 1, stop = 2))

korea_map_info.merge <- merge(korea_map_info, map.dat_final, by = "SIG_CD")
View(korea_map_info.merge)

korea_map.dat <- merge(korea_map, korea_map_info.merge[,c(4,6)], by = "id")


ggplot() + 
  geom_polygon(data = korea_map.dat,
               aes(x = long, y = lat, 
                   group = group,
                   fill = cluster),
               color = "white") + 
  theme_bw() + 
  theme(legend.position = "left")
