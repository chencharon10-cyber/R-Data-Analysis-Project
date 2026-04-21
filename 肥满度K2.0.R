# ====================================================================
# SCI 一区标准：LWR体长体重关系与K值评估 (完美对齐 CPUE 清洗逻辑版)
# ====================================================================

# 1. 强制关闭科学计数法
options(scipen = 999)

# 2. 安装并加载神仙包
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, ggplot2, ggpubr, openxlsx, broom, extrafont)

# 载入 Windows 字体库，让 R 能正常使用 Times New Roman
loadfonts(device = "win", quiet = TRUE)
windowsFonts(TNR = windowsFont("Times New Roman"))

# ====================================================================
# 3. 路径设置区 (【需手动修改】)
# ====================================================================
# 导入数据与输出路径
input_file <- "E:/zhuomian/体长 - 体重关系（LWR） - 副本.xlsx"
output_dir <- "E:/zhuomian/SCI_Results_Final"

# 自动创建输出文件夹（防止报错）
if (!dir.exists(output_dir)) dir.create(output_dir)

# ====================================================================
# 4. 数据导入与预处理 (🔥 完美复刻 CPUE 代码的极简、强壮清洗逻辑)
# ====================================================================
raw_data <- read_excel(input_file)

# A. 智能识别列名：防止Excel导出时带有看不见的空格，或者有重复列名
lake_col <- grep("Lake|湖", colnames(raw_data), ignore.case = TRUE)
if(length(lake_col) > 0) colnames(raw_data)[lake_col[1]] <- "Lake_ID"

species_col <- grep("Species|种", colnames(raw_data), ignore.case = TRUE)
if(length(species_col) > 0) colnames(raw_data)[species_col[1]] <- "Species_ID"

sl_col <- grep("SL|长", colnames(raw_data), ignore.case = TRUE)
if(length(sl_col) > 0) colnames(raw_data)[sl_col[1]] <- "SL"

wt_col <- grep("WT|重", colnames(raw_data), ignore.case = TRUE)
if(length(wt_col) > 0) colnames(raw_data)[wt_col[1]] <- "WT"

# B. 极致净化：仅保留这 4 列核心数据，彻底抛弃 Excel 里多余的公式和重复列
cleaned_data <- raw_data[, c("Lake_ID", "Species_ID", "SL", "WT")]

# C. 强制数值型转换 (防错机制)
cleaned_data$Lake_ID <- as.character(cleaned_data$Lake_ID)
cleaned_data$Species_ID <- as.character(cleaned_data$Species_ID)
cleaned_data$SL <- as.numeric(as.character(cleaned_data$SL))
cleaned_data$WT <- as.numeric(as.character(cleaned_data$WT))

# D. 剔除含有 NA(空值) 的行
cleaned_data <- cleaned_data[!is.na(cleaned_data$Lake_ID) & 
                               !is.na(cleaned_data$Species_ID) & 
                               !is.na(cleaned_data$SL) & 
                               !is.na(cleaned_data$WT), ]

# E. 匹配正确的物种字典与湖泊名称
species_map <- c(
  "6"  = "Toxabramis swinhonis",          # 似鱎
  "24" = "Coilia brachygnathus",          # 短颌鲚
  "33" = "Culter dabryi",                 # 达氏鲌
  "45" = "Hypophthalmichthys molitrix",   # 鲢
  "50" = "Aristichthys nobilis"           # 鳙
)

# 仅保留字典里的5种鱼，并赋给学名
cleaned_data <- cleaned_data[cleaned_data$Species_ID %in% names(species_map), ]
cleaned_data$Scientific_Name <- species_map[cleaned_data$Species_ID]

# 映射湖泊名称：1 = Xizi, 2 = Baidang
cleaned_data$Lake <- ifelse(cleaned_data$Lake_ID == "1", "Xizi Lake", 
                            ifelse(cleaned_data$Lake_ID == "2", "Baidang Lake", NA))

cleaned_data <- cleaned_data[!is.na(cleaned_data$Lake) & 
                               cleaned_data$Lake %in% c("Xizi Lake", "Baidang Lake"), ]

# 调整因子顺序，完全对齐 CPUE
cleaned_data$Lake <- factor(cleaned_data$Lake, levels = c("Baidang Lake", "Xizi Lake"))

# F. 计算 K 值与 LWR 生物学清洗 (剔除极其离谱的畸形记录)
cleaned_data$K <- 100 * cleaned_data$WT / (cleaned_data$SL^3)
cleaned_data$logL <- log(cleaned_data$SL)
cleaned_data$logW <- log(cleaned_data$WT)

# 粗筛：正常的 K 值应该在 0.1 到 3.5 之间
cleaned_data <- cleaned_data[cleaned_data$K > 0.1 & cleaned_data$K < 3.5, ]

# 精筛：利用回归残差剔除偏离群体特征过大的绝对离群点
df_final <- cleaned_data %>%
  group_by(Scientific_Name, Lake) %>%
  mutate(std_resid = as.vector(rstandard(lm(logW ~ logL, na.action = na.exclude)))) %>%
  filter(abs(std_resid) <= 3) %>%
  ungroup()

cat(paste0(">>> 数据清洗完成！剩余有效优质样本：", nrow(df_final), " 尾\n"))

# ====================================================================
# 5. SCI 核心：自动化 LWR 模型参数提取 (帮你精确计算 a 和 b)
# ====================================================================
lwr_stats <- df_final %>%
  group_by(Scientific_Name, Lake) %>%
  do({
    model <- lm(logW ~ logL, data = .)
    tidy_m <- tidy(model)
    glance_m <- glance(model)
    data.frame(
      Sample_Size_N = nrow(.),
      Intercept_ln_a = tidy_m$estimate[1],
      a_value = exp(tidy_m$estimate[1]),
      b_value = tidy_m$estimate[2], # 这就是论文 3.3.1 节需要的异速生长指数 b
      b_SE = tidy_m$std.error[2],
      R_squared = glance_m$r.squared,
      P_value = glance_m$p.value
    )
  }) %>%
  ungroup()

# ====================================================================
# 6. 高颜值 SCI 绘图系统 (与 CPUE 的 theme_bw 风格一脉相承)
# ====================================================================
# 定制全局主题
theme_sci_final <- theme_bw(base_family = "TNR") +
  theme(
    text = element_text(size = 14, color = "black"),
    axis.title = element_text(face = "bold", size = 15, color = "black"),
    axis.text = element_text(size = 13, color = "black"),
    strip.text = element_text(face = "bold.italic", size = 14, background = element_rect(fill = "grey95")),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 13),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 1.2),
    axis.ticks = element_line(linewidth = 1, color = "black")
  )

color_palette <- c("Baidang Lake" = "#b2d59b", "Xizi Lake" = "#c59e91")

# 图1: LWR 对数散点图与拟合线
p_lwr <- ggplot(df_final, aes(x = logL, y = logW, color = Lake, fill = Lake)) +
  geom_point(alpha = 0.4, size = 1.5, shape = 21, color = "grey30") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
  facet_wrap(~Scientific_Name, scales = "free", ncol = 3) +
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  labs(x = "ln (Standard Length, cm)", y = "ln (Body Weight, g)", color = "Management Regime:", fill = "Management Regime:") +
  theme_sci_final

# 图2: K值多面板箱线图
p_k <- ggplot(df_final, aes(x = Lake, y = K, fill = Lake)) +
  geom_boxplot(width = 0.75, outlier.shape = 21, outlier.alpha = 0.5, outlier.size = 1.5, color = "black", linewidth = 0.75) +
  stat_compare_means(
    method = "wilcox.test",
    label = "p.signif",
    bracket.size = 0.8,
    size = 6,
    vjust = 0.5,
    family = "TNR"
  ) +
  facet_wrap(~Scientific_Name, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = c("Baidang Lake" = "Baidang", "Xizi Lake" = "Xizi")) +
  labs(x = "", y = "Fulton's Condition Factor (K)", fill = "Management Regime:") +
  theme_sci_final

# ====================================================================
# 7. 导出最终成果
# ====================================================================
print(p_lwr)

# 以标准的学术期刊宽页尺寸导出（确保 1区 SCI 标准排版清晰度）
ggsave(file.path(output_dir, "Figure_LWR_Scatter.tiff"), plot = p_lwr, width = 12, height = 8, dpi = 300, compression = "none", bg = "white")
ggsave(file.path(output_dir, "Figure_K_Factor_Boxplot.tiff"), plot = p_k, width = 12, height = 8, dpi = 300, compression = "none", bg = "white")

# 导出 Excel 统计表
export_list <- list(
  "1_LWR_Statistics" = lwr_stats,   
  "2_Cleaned_Raw_Data" = df_final
)
write.xlsx(export_list, file = file.path(output_dir, "LWR_and_K_Statistical_Report.xlsx"))

cat(paste0("\n>>> 大功告成！\n>>> 高清美图与统计表已保存至: ", output_dir, "\n"))