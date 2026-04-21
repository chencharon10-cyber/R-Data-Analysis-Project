# ==========================================================
# 鱼类 IRI 年际演变堆叠柱状图 (无标题 + 单行图例精简版)
# ==========================================================

# 1. 安装并加载必要的包 (包含富文本处理包 ggtext)
if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("scales")) install.packages("scales")
if (!require("ggtext")) install.packages("ggtext") 

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggtext)

# ==========================================================
# 2. 导入与数据清洗 (完美处理格式错误和NA警告)
# ==========================================================
# 【需手动修改 1】：替换为你的 Excel 文件路径
file_path <- "E:/zhuomian/IRI合并数据.xlsx"

# 静默读取原始数据，忽略表头
raw_data <- suppressMessages(suppressWarnings(read_excel(file_path, col_names = FALSE)))

headers <- as.character(raw_data[1, ])
lake_years <- headers[seq(1, length(headers), by = 2)]
data_content <- raw_data[3:nrow(raw_data), ]

df_list <- list()
for (i in seq_along(lake_years)) {
  if (is.na(lake_years[i])) next
  
  col_species <- i * 2 - 1
  col_iri <- i * 2
  
  # 防止多余的空白列导致越界报错
  if (col_iri > ncol(data_content)) next
  
  year_str <- substring(lake_years[i], 1, 4)
  lake_cn <- substring(lake_years[i], 6)
  
  # 提取并抑制 NA 警告
  temp_df <- suppressWarnings(data.frame(
    Year = as.numeric(year_str),
    Lake_CN = lake_cn,
    Species_CN = as.character(data_content[[col_species]]),
    IRI_Value = as.numeric(as.character(data_content[[col_iri]])),
    stringsAsFactors = FALSE
  ))
  
  # 剔除无效行
  temp_df <- temp_df %>% filter(!is.na(Species_CN) & Species_CN != "" & !is.na(IRI_Value))
  df_list[[i]] <- temp_df
}

data_long_all <- bind_rows(df_list)

# ==========================================================
# 3. 核心优势种过滤与面板重排 (保留 Top 8)
# ==========================================================
# 8种核心物种
target_species <- c(
  "似鱎", "短颌鲚", "鳙", "达氏鲌", "鲢", "鲤", "鲫", "似刺鳊鮈"
)

data_long <- data_long_all %>%
  filter(Species_CN %in% target_species) %>%
  mutate(Lake = case_when(
    Lake_CN == "嬉子湖" ~ "嬉子湖 (全面禁捕)",
    Lake_CN == "白荡湖" ~ "白荡湖 (生态渔业)",
    TRUE ~ Lake_CN
  ))

data_long$Lake <- factor(data_long$Lake, levels = c("白荡湖 (生态渔业)", "嬉子湖 (全面禁捕)"))
data_long$Species_CN <- factor(data_long$Species_CN, levels = target_species)

# ==========================================================
# 4. 颜色分配
# ==========================================================
species_colors <- c(
  "#ff9c9a", # 似鱎 
  "#85b3cb", # 短颌鲚 
  "#b7dbff", # 鳙 
  "#cef0c0", # 达氏鲌 
  "#ffc76b", # 鲢 
  "#d4a6c8", # 鲤 
  "#a3e4d7", # 鲫 
  "#f9e79f"  # 似刺鳊鮈 
)

# ==========================================================
# 5. 定义混合字体富文本标签 (宋体 + Times New Roman)
# ==========================================================
# 【修改点 1】：去除了 mixed_title 的定义
mixed_x_title <- "<span style='font-family:SimSun;'>相对重要性指数 (</span><span style='font-family:\"Times New Roman\";'>IRI</span><span style='font-family:SimSun;'>)</span>"
mixed_y_title <- "<span style='font-family:SimSun;'>年份</span>"

# ==========================================================
# 6. 绘制图形 (重点优化图例与字体)
# ==========================================================
plot <- ggplot(data_long, aes(y = factor(Year), x = IRI_Value, fill = Species_CN)) +
  geom_col(position = "stack", width = 0.5, color = "black", linewidth = 0.8) +
  facet_wrap(~Lake, ncol = 1, strip.position = "top") +
  
  scale_x_continuous(
    limits = c(0, 20000), 
    breaks = seq(0, 20000, by = 5000),
    labels = comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = setNames(species_colors, target_species),
    drop = FALSE,
    name = NULL
  ) +
  # 【修改点 2】：删除了 labs() 中的 title 参数
  labs(
    y = mixed_y_title,
    x = mixed_x_title
  ) +
  theme_bw() +
  theme(
    # 【修改点 3】：删除了 plot.title = element_markdown(...) 设置
    axis.title.x = element_markdown(face = "bold", margin = margin(t = 40), size = 62),
    axis.title.y = element_markdown(face = "bold", margin = margin(r = 30), size = 62),
    
    # 坐标轴数字 (纯 Times New Roman)
    axis.text.x = element_text(family = "Times New Roman", face = "bold", size = 45, margin = margin(t = 18)),
    axis.text.y = element_text(family = "Times New Roman", face = "bold", size = 45, margin = margin(r = 18)),
    
    axis.ticks.length = unit(0.3, "cm"),
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth = 3),
    
    # 分面标题 (纯宋体)
    strip.text = element_text(family = "SimSun", face = "bold", size = 56, margin = margin(b = 20, t = 20)),
    strip.background = element_rect(fill = "white", linewidth = 3),
    panel.spacing = unit(1.5, "cm"), 
    
    # ==========================================================
    # 【核心修改区：图例单行优化】
    # ==========================================================
    legend.position = "bottom",
    legend.justification = "center",
    
    # 【修改点 4】：文字字号减小，间距大幅缩减以适应单行
    legend.text = element_text(family = "SimSun", face = "bold", size = 50, margin = margin(r = 25, l = 10)), 
    
    # 【修改点 5】：色块宽度由 4cm 缩减为 2cm
    legend.key.width = unit(2, "cm"),   
    legend.key.height = unit(1.8, "cm"), 
    
    legend.box.margin = margin(t = 40, r = 0, b = 40, l = 0), 
    plot.margin = margin(t = 60, r = 80, b = 40, l = 80)
  ) +
  # 【修改点 6】：强制图例分为 1 行，并更新色块宽度
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, label.hjust = 0, keywidth = unit(2, "cm")))

print(plot)

# ==========================================================
# 7. 导出高清图片
# ==========================================================
ggsave(
  filename = "E:/zhuomian/Figure2_IRI_Top8_Final.tiff",  # 【需手动修改 2】：导出路径
  plot = plot,
  width = 34,  # 【修改点 7】：画幅宽度从 30 提升至 34，防止单行 8 个图例被截断
  height = 38, 
  dpi = 50,    # 请根据投稿要求调整 dpi（例如改定稿时改成 300）
  device = "tiff",
  bg = "white" 
)