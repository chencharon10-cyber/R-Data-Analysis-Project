# ==========================================================
# SCI 一区标准：Figure_CPUE (独立网具分面，去除无效总计版)
# ==========================================================

# 1. 强制关闭科学计数法
options(scipen = 999) 

# 2. 安装并加载必要的包 (包含 dplyr 和 tidyr 用于数据重构)
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(ggsci)) install.packages("ggsci")
if(!require(extrafont)) install.packages("extrafont")
if(!require(scales)) install.packages("scales")
if(!require(ggtext)) install.packages("ggtext") 
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")

library(ggplot2)
library(ggpubr)
library(ggsci)
library(extrafont)
library(scales)
library(ggtext)
library(dplyr)
library(tidyr)

# 载入 Windows 字体库
suppressWarnings(loadfonts(device = "win", quiet = TRUE))

# ==========================================================
# 3. 导入数据与高级预处理 (彻底剔除无效的总计CPUE)
# ==========================================================
# 【需手动修改 1】：确认导入详细版台账数据的路径
my_data <- read.csv("E:/zhuomian/CPUE汇总.csv", header = TRUE, stringsAsFactors = FALSE)

# 智能提取并重构数据：分别计算复合刺网、定置地笼的实际 CPUE
cleaned_data <- my_data %>%
  filter(!is.na(湖泊) & 湖泊 %in% c("嬉子湖", "白荡湖")) %>%
  mutate(
    # 依据原始表，将捕捞量(g)乘以2，统一换算为标准的日均 CPUE(g/net·d)
    Gillnet_CPUE = as.numeric(`该次捕捞刺网总重量.g.`) * 2,
    Trapnet_CPUE = as.numeric(`该次捕捞地笼总重量.g.`) * 2
  ) %>%
  filter(!is.na(Gillnet_CPUE) & !is.na(Trapnet_CPUE)) %>%
  # 将宽表拉长，仅保留复合刺网和定置地笼数据进行分面
  pivot_longer(
    cols = c(Gillnet_CPUE, Trapnet_CPUE),
    names_to = "Gear_Type",
    values_to = "CPUE"
  )

# 调整湖泊因子顺序：白荡湖在左，嬉子湖在右
cleaned_data$Lake <- factor(cleaned_data$湖泊, levels = c("白荡湖", "嬉子湖"))

# 调整分面因子的顺序，并利用富文本映射符合 SCI 标准的中文子标题
cleaned_data$Gear_Type <- factor(
  cleaned_data$Gear_Type, 
  levels = c("Gillnet_CPUE", "Trapnet_CPUE"),
  labels = c(
    "<span style='font-family:SimSun;'>复合刺网 </span><span style='font-family:\"Times New Roman\";'>CPUE</span>",
    "<span style='font-family:SimSun;'>定置地笼 </span><span style='font-family:\"Times New Roman\";'>CPUE</span>"
  )
)

# ==========================================================
# 4. 预定义混合字体的富文本标签 (宋体 + Times New Roman)
# ==========================================================
# 明确标注单位为 g/net·d，体现独立的努力量标准化
mixed_y_title <- "<span style='font-family:SimSun;'>单位努力捕捞量 </span><span style='font-family:\"Times New Roman\";'>(CPUE, g/net·d)</span>"
mixed_x_title <- "<span style='font-family:SimSun;'>管理模式</span>"

label_baidang <- "<span style='font-family:SimSun;'>白荡湖</span><br><span style='font-family:SimSun;'>(生态渔业)</span>"
label_xizi <- "<span style='font-family:SimSun;'>嬉子湖</span><br><span style='font-family:SimSun;'>(全面禁捕)</span>"

# ==========================================================
# 5. 开始绘制 1x2 顶级分面神图
# ==========================================================
p <- ggplot(cleaned_data, aes(x = Lake, y = CPUE, fill = Lake)) +
  
  geom_boxplot(alpha = 0.9, outlier.shape = NA, width = 0.6, linewidth = 0.8, color = "black") +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("白荡湖" = "#b2d59b", "嬉子湖" = "#c59e91")) +
  
  # 【核心优化】：改为横向排列的 1x2 布局
  facet_wrap(~Gear_Type, scales = "free_y", ncol = 2) +
  
  scale_y_continuous(
    labels = comma, 
    expand = expansion(mult = c(0, 0.15)) 
  ) +
  
  labs(x = mixed_x_title, y = mixed_y_title) +
  scale_x_discrete(labels = c("白荡湖" = label_baidang, "嬉子湖" = label_xizi)) +
  
  theme_bw() +
  theme(
    strip.text = element_markdown(size = 18, color = "black", face = "bold", margin = margin(b = 10, t = 10)),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 1.2),
    
    axis.title.x = element_markdown(face = "bold", size = 18, color = "black", margin = margin(t = 15)),
    axis.title.y = element_markdown(face = "bold", size = 18, color = "black", margin = margin(r = 15)),
    
    axis.text.y = element_text(size = 14, color = "black", family = "Times New Roman"),
    axis.text.x = element_markdown(size = 16, color = "black", lineheight = 1.2, margin = margin(t = 10)),
    
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    panel.border = element_rect(color = "black", linewidth = 1.2),
    axis.ticks = element_line(linewidth = 1, color = "black"),
    legend.position = "none",
    
    panel.spacing = unit(1.5, "lines") 
  ) +
  
  stat_compare_means(method = "wilcox.test", 
                     comparisons = list(c("白荡湖", "嬉子湖")),
                     label = "p.signif", 
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 10000000),
                                        symbols = c("****", "***", "**", "*", "ns")), 
                     bracket.size = 0.8,
                     tip.length = 0.02,
                     size = 10,  
                     vjust = 0.5,
                     label.y.npc = 0.92, 
                     family = "Times New Roman")

print(p)

# ==========================================================
# 6. 导出高清 TIFF 图片
# ==========================================================
# 【需手动修改 2】：确认导出保存的路径
ggsave("E:/zhuomian/Figure3_CPUE_Faceted_V2.tiff", 
       plot = p, 
       device = "tiff",
       width = 8,       # 宽度由11下调至8，适应2个分面面板的黄金比例
       height = 6,    
       dpi = 300,      
       compression = "none", 
       bg = "white")