# set wd
setwd("D:\\dc\\论文\\视频分析\\")

# load packages
# install.packages("tidyverse")
library(openxlsx)
library(ggplot2)
library(tidyverse)
# load filesimes, dt_wybg$持续时间)
dt_wybg_lc <- read.xlsx("fxb.xlsx", sheet = 1)
dt_wybg_xy <- read.xlsx("fxb.xlsx", sheet = 2)
dt_wybg_wk <- read.xlsx("fxb.xlsx", sheet = 3)
dt_wybg_yj <- read.xlsx("fxb.xlsx", sheet = 4)

dt_wybg <- rbind(rbind(dt_wybg_lc, dt_wybg_xy), 
                 rbind(dt_wybg_wk, dt_wybg_yj))
dt_yexx <- read.xlsx("yexx.xlsx", sheet = 1)

dt_wybg$times <- 1
for(i in 1:length(dt_wybg$婴儿代码)){
  if(dt_wybg$具体行为表现[i]==names(table(dt_wybg$具体行为表现)[4])){dt_wybg$times[i] <- 0}
}
dt_wybg_fxbg_times <- aggregate(dt_wybg$times~dt_wybg$婴儿代码+dt_wybg$时间段+dt_wybg$具体行为表现, dt_wybg, FUN = sum)
dt_wybg$持续时间 <- as.numeric(dt_wybg$持续时间)
dt_wybg_fxbg_cxsj <- aggregate(dt_wybg$持续时间~dt_wybg$婴儿代码+dt_wybg$时间段+dt_wybg$具体行为表现, dt_wybg, FUN = sum)

##分个人 ##分时间段  ## 分参次 ## 分具体行为
dt_wybg_fxbg <- aggregate(cbind(dt_wybg$times,dt_wybg$持续时间)~dt_wybg$婴儿代码+dt_wybg$时间段+dt_wybg$具体行为表现, dt_wybg, FUN = sum)                         
str(dt_wybg_fxbg)
names(dt_wybg_fxbg) <- c("yedm","sjd","action","times","cxsj")
dt_wybg_fxbg$dcsj <- 0
for(i in 1:length(dt_wybg_fxbg$yedm)){
  if(dt_wybg_fxbg$cxsj[i]!=0){dt_wybg_fxbg$dcsj[i] <- dt_wybg_fxbg$cxsj[i]/dt_wybg_fxbg$times[i]}
}

## 分个人
dt_wybg_fxbg_all <- aggregate(cbind(dt_wybg$times,dt_wybg$持续时间)~dt_wybg$婴儿代码+dt_wybg$时间段, dt_wybg, FUN = sum)                         
str(dt_wybg_fxbg_all)
names(dt_wybg_fxbg_all) <- c("yedm","sjd","times","cxsj")
dt_wybg_fxbg_all$dcsj <- 0
for(i in 1:length(dt_wybg_fxbg_all$yedm)){
  if(dt_wybg_fxbg_all$cxsj[i]!=0){dt_wybg_fxbg_all$dcsj[i] <- dt_wybg_fxbg_all$cxsj[i]/dt_wybg_fxbg_all$times[i]}
}
action_name <- names(table(dt_wybg_fxbg$action))
dt_wybg_fxbg = dt_wybg_fxbg %>% mutate(action = fct_recode(action,
                                                           "在孩子拒绝\n吃食物时仍喂食"=action_name[1],
                                                           "采用一些手段\n让孩子多吃食物"=action_name[2],
                                                           "孩子吃饱了仍继续\n迫使孩子吃光盘中的食物"=action_name[3],
                                                           "无发现"=action_name[4]),
                                       sjd = fct_recode(sjd,
                                                        "早餐"="早",
                                                        "午餐"="午",
                                                        "晚餐"="晚"))
dt_wybg_fxbg_all = dt_wybg_fxbg_all %>% mutate(sjd = fct_recode(sjd,
                                                                "早餐"="早",
                                                                "午餐"="午",
                                                                "晚餐"="晚"))
action_name_new <- c("强迫\n进食行为","在孩子拒绝\n吃食物时仍喂食","采用一些手段\n让孩子多吃食物","孩子吃饱了仍继续\n迫使孩子吃光盘中的食物")

# 数据清洗完毕开始做出要作图的表格并且作图 # 经验证是正确的
# 图1 按三餐分类的强迫进食喂养行为出现的平均发生次数
# 强迫进食总的三餐分类情况，各个行为三餐情况
dt_p_sum <- dt_wybg_fxbg_all %>% group_by(sjd) %>% summarise(mean(times)) %>% 
  mutate(sjd=factor(sjd,levels = c("早餐","午餐","晚餐")))
dt_p_sum$group <- "强迫\n进食行为"
names(dt_p_sum) <- c("sjd","value","groups")

dt_p_distinct <- dt_wybg_fxbg %>% 
  group_by(action, sjd) %>% 
  summarise(value = mean(times)) %>% 
  filter(action != "无发现") %>% 
  select(sjd, value, action)
names(dt_p_distinct)[3] <- "groups"

dt_p <- rbind(dt_p_sum, dt_p_distinct) %>% 
  mutate(groups = factor(groups, levels = action_name_new))
dt_p %>% 
  ggplot(mapping = aes(x=groups, y=value))+
  geom_bar(aes(y=value, fill=sjd), stat = "identity", position = "dodge")+
  scale_fill_grey()+
  labs(x="", y="次数", fill="")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 22),
        legend.text = element_text(size=22),
        legend.key.size = unit(35,"pt")) -> p1
ggsave(filename = "p1_new_chinese.png", plot = p1, width = 12.5, height = 7)

# 图2 按三餐分类的强迫进食总时长中位数
dt_p_sum <- dt_wybg_fxbg_all %>% group_by(sjd) %>% summarise(quantile(cxsj,probs=.5)) %>% 
  mutate(sjd=factor(sjd,levels = c("早餐","午餐","晚餐")))
dt_p_sum$group <- "强迫\n进食行为"
names(dt_p_sum) <- c("sjd","value","groups")

dt_p_distinct <- dt_wybg_fxbg %>% 
  group_by(action, sjd) %>% 
  summarise(value = quantile(cxsj,probs=.5)) %>% 
  filter(action != "无发现") %>% 
  select(sjd, value, action)
names(dt_p_distinct)[3] <- "groups"

dt_p <- rbind(dt_p_sum, dt_p_distinct) %>% 
  mutate(groups = factor(groups, levels = action_name_new))
dt_p %>% 
  ggplot(mapping = aes(x=groups, y=value))+
  geom_bar(aes(y=value, fill=sjd), stat = "identity", position = "dodge")+
  scale_fill_grey()+
  labs(x="", y="总时长中位数(秒)", fill="")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 22),
        legend.text = element_text(size=22),
        legend.key.size = unit(35,"pt")) -> p2
ggsave(filename = "p2_new_chinese.png", plot = p2, width = 12.5, height = 7)
# 图3 按三餐分类的强迫进食喂养行为的单次时长的中位数
# 图3 
dt_p_sum <- dt_wybg_fxbg_all %>% group_by(sjd) %>% summarise(quantile(dcsj,probs=.5)) %>% 
  mutate(sjd=factor(sjd,levels = c("早餐","午餐","晚餐")))
dt_p_sum$group <- "强迫\n进食行为"
names(dt_p_sum) <- c("sjd","value","groups")

dt_p_distinct <- dt_wybg_fxbg %>% 
  group_by(action, sjd) %>% 
  summarise(value = quantile(dcsj,probs=.5)) %>% 
  filter(action != "无发现") %>% 
  select(sjd, value, action)
names(dt_p_distinct)[3] <- "groups"

dt_p <- rbind(dt_p_sum, dt_p_distinct) %>% 
  mutate(groups = factor(groups, levels = action_name_new))
dt_p %>% 
  ggplot(mapping = aes(x=groups, y=value))+
  geom_bar(aes(y=value, fill=sjd), stat = "identity", position = "dodge")+
  scale_fill_grey()+
  labs(x="", y="单次时长中位数(秒)", fill="")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 22),
        legend.text = element_text(size=22),
        legend.key.size = unit(35,"pt")) -> p3
ggsave(filename = "p3_new_chinese.png", plot = p3, width = 12.5, height = 7)