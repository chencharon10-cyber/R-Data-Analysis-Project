# ==============================================================================
# ABC 曲线与 W 统计量分析脚本 (SCI 顶级期刊排版优化版)
# ==============================================================================

# --- 1. 环境准备 ---
required_packages <- c("readxl", "ggplot2", "dplyr", "tidyr", "patchwork", "showtext")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# --- 2. 字体与渲染设置 ---
# 加载中英文字体（请确保 Windows 目录下存在对应的 ttc/ttf 文件）
font_add("SimSun", "simsun.ttc")   # 宋体
font_add("TNR", 
         regular    = "times.ttf", 
         italic     = "timesi.ttf", 
         bold       = "timesbd.ttf", 
         bolditalic = "timesbi.ttf") # Times New Roman 全字族
showtext_auto()                    
showtext_opts(dpi = 300)

# ==========================================
# ！！！手动修改部分 ！！！
# ==========================================
# 修改点 1：原始数据文件夹路径（存放那8个Excel的地方）
base_path <- "E:/zhuomian/antigravitytest/ABC曲线数据_PRIMER专用"

# 修改点 2：导出路径
output_path <- "E:/zhuomian/ABC_Curves_Final_Optimized.png"
# ==========================================

# --- 3. ABC 计算逻辑函数 ---
calculate_abc <- function(file_path) {
  data <- read_excel(file_path)
  df <- data %>%
    select(species = 1, abundance = 2, biomass = 3) %>%
    filter(!is.na(species))
  S <- nrow(df)
  abc_abund <- df %>% arrange(desc(abundance)) %>%
    mutate(Ai = (cumsum(abundance) / sum(abundance)) * 100)
  abc_biom <- df %>% arrange(desc(biomass)) %>%
    mutate(Bi = (cumsum(biomass) / sum(biomass)) * 100)
  # 计算 W 指标
  W_val <- sum(abc_biom$Bi - abc_abund$Ai) / (50 * (S - 1))
  
  plot_data <- data.frame(
    Rank = 1:S,
    Biomass = abc_biom$Bi,
    Abundance = abc_abund$Ai
  ) %>%
    pivot_longer(cols = c(Biomass, Abundance), names_to = "Type", values_to = "Percentage")
  
  return(list(plot_df = plot_data, W = W_val, S = S))
}

# --- 4. 批量绘图处理 ---
lakes <- c("嬉子湖", "白荡湖")
years <- c("2022", "2023", "2024", "2025")
w_dynamics <- data.frame(Lake = character(), Year = numeric(), W = numeric())
labels <- list("嬉子湖" = c("A", "B", "C", "D"), "白荡湖" = c("E", "F", "G", "H"))
plots <- list()

# 定义统一的 SCI 结果页主题
sci_theme <- theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth = 1.0, color = "black"),
    plot.title    = element_text(size = 13, face = "bold", family = "SimSun"),
    axis.title.x  = element_text(size = 11, family = "SimSun", margin = margin(t = 6)),
    axis.title.y  = element_text(size = 11, family = "SimSun", margin = margin(r = 6)),
    axis.text     = element_text(size = 10, family = "TNR", color = "black"),
    plot.margin   = margin(8, 10, 8, 10)
  )

for (lake_idx in seq_along(lakes)) {
  lake_name <- lakes[lake_idx]
  for (year_idx in seq_along(years)) {
    year <- years[year_idx]
    file_pattern <- paste0(year, "_", lake_name)
    all_files <- list.files(base_path, pattern = ".xlsx", full.names = TRUE)
    target_file <- all_files[grep(file_pattern, all_files)]
    
    if (length(target_file) > 0) {
      res <- calculate_abc(target_file[1])
      label <- labels[[lake_name]][year_idx]
      w_dynamics <- rbind(w_dynamics, data.frame(Lake = lake_name, Year = as.numeric(year), W = res$W))
      
      p <- ggplot(res$plot_df, aes(x = Rank, y = Percentage, color = Type, linetype = Type)) +
        geom_line(linewidth = 1.1) +
        scale_color_manual(values = c("Biomass" = "#778899", "Abundance" = "#B06060")) +
        scale_linetype_manual(values = c("Biomass" = "solid", "Abundance" = "dashed")) +
        scale_x_log10(limits = c(1, NA)) +
        labs(title = paste0(label, ": ", year, "年 ", lake_name),
             x = "物种序列", y = "累积百分比 (%)") +
        # 修正 W 值的斜体显示与定位
        annotate("text", x = Inf, y = -Inf,
                 label = sprintf("italic(W) == '%.3f'", res$W),
                 parse = TRUE, hjust = 1.2, vjust = -1.5, size = 4.8, family = "TNR") +
        sci_theme
      
      plots[[label]] <- p
    }
  }
}

# --- 5. 生成 I 和 J 动态趋势图 ---
for (lake_name in lakes) {
  lake_data <- w_dynamics %>% filter(Lake == lake_name)
  label <- ifelse(lake_name == "嬉子湖", "I", "J")
  
  p_dyn <- ggplot(lake_data, aes(x = Year, y = W)) +
    geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.8, color = "gray40") +
    geom_line(color = "#556B2F", linewidth = 1.2) +
    geom_point(color = "#556B2F", size = 3.5) +
    scale_x_continuous(breaks = 2022:2025) +
    labs(title = bquote(.(label) * ":" ~ .(lake_name) ~ italic(W) ~ "统计量动态"),
         x = "年份", y = expression(italic(W) ~ "统计量")) +
    sci_theme
  
  plots[[label]] <- p_dyn
}

# --- 6. 整合布局与保存 ---
# 2列5行布局，并控制底部两张动态图的高度
final_layout <- (plots[["A"]] | plots[["E"]]) /
  (plots[["B"]] | plots[["F"]]) /
  (plots[["C"]] | plots[["G"]]) /
  (plots[["D"]] | plots[["H"]]) /
  (plots[["I"]] | plots[["J"]]) + 
  plot_layout(heights = c(1, 1, 1, 1, 1.2))

ggsave(output_path, final_layout, width = 8.5, height = 13.5, dpi = 300)
showtext_auto(FALSE)
message(">>> 优化完成！图片已保存至: ", output_path)