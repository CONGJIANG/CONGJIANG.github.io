# 加载必要的包
library(lme4)    # 用于拟合混合模型
library(lmerTest) # 为lme4模型提供P值
library(ggplot2)  # 用于绘图
library(dplyr)    # 用于数据操作

# 设置随机种子以保证结果可重现
set.seed(123)

# 模拟参数
n_participants <- 2000 # 参与者数量 (论文中是1815)
n_sites <- 5          # 研究中心数量
days_per_person <- 7  # 每人测量7天
total_obs <- n_participants * days_per_person

# 模拟数据
sim_data <- data.frame(
  participant_id = rep(1:n_participants, each = days_per_person),
  site = rep(sample(1:n_sites, n_participants, replace = TRUE), each = days_per_person),
  day = rep(1:days_per_person, times = n_participants),
  treatment = rep(sample(c("Nonopioid", "Opioid"), n_participants, replace = TRUE), each = days_per_person)
)

# 添加随机效应：每个参与者有自己的基线疼痛
participant_intercept <- rnorm(n_participants, mean = 0, sd = 1.5)
# 添加随机效应：每个研究中心有自己的基线水平
site_intercept <- rnorm(n_sites, mean = 0, sd = 0.8)

# 生成疼痛评分 (0-10)
# 固定效应: 
# - 非阿片组基线疼痛为4，阿片组为4.5 (模拟阿片组疼痛稍高)
# - 疼痛随时间递减
# 随机效应: 加入参与者和中心的随机截距
# 残差: 个体在特定时间点的随机波动

sim_data <- sim_data %>%
  mutate(
    base_pain = ifelse(treatment == "Nonopioid", 4, 4.5),
    time_effect = -0.3 * (day - 1), # 疼痛每天下降0.3
    participant_re = participant_intercept[participant_id],
    site_re = site_intercept[site],
    error = rnorm(total_obs, mean = 0, sd = 1.2),
    pain_score = base_pain + time_effect + participant_re + site_re + error,
    # 确保疼痛分数在0-10之间
    pain_score = pmax(0, pmin(10, pain_score))
  )

# 查看模拟数据的前几行
head(sim_data)



# 拟合线性混合模型
# 因变量: pain_score
# 固定效应: treatment, day, 以及它们的交互项 (treatment:day)
# 随机效应: (1 | participant_id) 表示每个参与者有一个随机截距
#          (1 | site) 表示每个研究中心有一个随机截距

mixed_model <- lmer(pain_score ~ treatment * day + (1 | participant_id) + (1 | site),
                    data = sim_data)

# 查看模型摘要
summary(mixed_model)