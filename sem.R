###############################################################################
# 长江十年禁渔"湖—湖"镜像研究 —— 鱼类群落特征路径分析代码
# 【适配小样本n=8】简化为Path Analysis，无潜变量，100%可运行
###############################################################################

# ====================== 1. 自动加载/安装必需包 ======================
required_packages <- c("lavaan", "semPlot", "readr", "dplyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("正在安装依赖包：", pkg, "\n")
    install.packages(pkg, dependencies = TRUE, repos = "https://mirror.tuna.tsinghua.edu.cn/CRAN/")
  }
  library(pkg, character.only = TRUE)
  cat("✅ 已加载包：", pkg, "\n")
}

# ====================== 2. 用户可修改参数区（仅需改这里！）======================
# ********************************************************************
# 1. 数据文件路径（改为你的桌面SEM_data.csv路径，Windows用/分隔）
data_path <- "E:/zhuomian/SEM_data.csv"
# 2. 结果输出目录（改为你想保存结果的文件夹路径，可和生长特征图同目录）
output_dir <- "E:/zhuomian/SEM结果"
# 3. Bootstrap重抽样次数（小样本建议2000次，减少计算量）
bootstrap_n <- 2000
# 4. 路径图尺寸（英寸，3:2比例，无需修改）
fig_width <- 16
fig_height <- 12
# ********************************************************************

# ====================== 3. 创建输出目录 ======================
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cat("✅ 新建输出目录：", output_dir, "\n")
} else {
  cat("✅ 输出目录已存在：", output_dir, "\n")
}

# ====================== 4. 导入标准化数据（从SPSS导出的CSV）======================
data <- readr::read_csv(data_path)
cat("✅ 数据导入完成，共", nrow(data), "行，", ncol(data), "列变量\n")
cat("变量名列表：\n")
print(colnames(data))

# ====================== 5. 【核心修正】定义简约路径分析模型（无潜变量，适配n=8）======================
# 【模型逻辑链】
# Management（管理模式）→ 优势种平均生物量 → 生长特征均值 → 群落稳定性（ZW）
# 注：为适配小样本，将同类指标取平均后做路径分析，避免协方差矩阵爆炸
sem_model <- '
  # 第一步：先在模型外手动计算复合指标（代码在后面）
  # 路径分析（直接回归）
  ZDominant_Mean ~ Management
  ZGrowth_Mean ~ ZDominant_Mean
  ZW ~ ZGrowth_Mean + Management
'

# ====================== 5.1 【关键预处理】手动计算复合指标（适配小样本）======================
# 把5个优势种Z分数取平均，作为「优势种结构」的复合指标
data$ZDominant_Mean <- rowMeans(data[, c("ZCoiliabrachygnathus", "ZCulterdabryi", "ZToxabramisswinhonis", "ZAristichthysnobilis", "ZHypophthalmichthysmolitrix")], na.rm = TRUE)
# 把4个生长特征Z分数取平均，作为「种群生长」的复合指标
data$ZGrowth_Mean <- rowMeans(data[, c("ZPSD", "ZRSDQ", "ZMEANB", "ZMEANK")], na.rm = TRUE)
cat("✅ 复合指标计算完成：ZDominant_Mean（优势种结构）、ZGrowth_Mean（种群生长）\n")

# ====================== 6. 拟合路径分析模型（添加check.lv.names=FALSE）======================
fit <- lavaan::sem(sem_model, 
                   data = data, 
                   se = "bootstrap",
                   bootstrap = bootstrap_n,
                   check.lv.names = FALSE,  # 关闭潜变量名检查
                   std.lv = TRUE)

# ====================== 7. 输出模型结果（保存到txt文件）======================
cat("\n=== 模型拟合结果与路径系数 ===")
sink(file.path(output_dir, "Path_Analysis_results.txt"))
print(lavaan::summary(fit, 
                      fit.measures = TRUE,
                      standardized = TRUE,
                      rsquare = TRUE))
sink()
cat("✅ 模型结果已保存至：", file.path(output_dir, "Path_Analysis_results.txt"), "\n")

# ====================== 8. 绘制论文级路径图（保存为TIFF+PNG）======================
semPlot::semPaths(fit, 
                  what = "std",
                  layout = "tree2",  # 更适合简约路径的布局
                  edge.label.cex = 1.3,
                  node.label.cex = 1.3,
                  color = list(lat = "#87CEEB", man = "#FFFFFF"),
                  filename = file.path(output_dir, "Path_Analysis_plot"),
                  filetype = "tiff",
                  width = fig_width,
                  height = fig_height,
                  dpi = 600)

semPlot::semPaths(fit, 
                  what = "std",
                  layout = "tree2",
                  edge.label.cex = 1.3,
                  node.label.cex = 1.3,
                  color = list(lat = "#87CEEB", man = "#FFFFFF"),
                  filename = file.path(output_dir, "Path_Analysis_plot"),
                  filetype = "png",
                  width = fig_width,
                  height = fig_height,
                  dpi = 300)
cat("✅ 路径图已保存至：", output_dir, "\n")

# ====================== 9. 运行完成提示 ======================
cat("\n=== 路径分析全部完成！（已适配小样本n=8）===")
cat("1. 模型结果 → Path_Analysis_results.txt\n")
cat("2. 路径图 → Path_Analysis_plot.tiff（投稿用）+ Path_Analysis_plot.png（预览用）\n")
cat("3. 【论文说明】因样本量限制，本研究采用路径分析（Path Analysis）替代完整SEM，将同类指标取平均后检验逻辑链\n")
cat("4. 解读重点：\n")
cat("   - 拟合指标：CFI>0.90、RMSEA<0.10 即模型合格\n")
cat("   - 路径系数：Estimate（标准化效应）+ P值（P<0.05为显著）\n")

# 自动打开输出目录（Windows）
try({ shell.exec(output_dir) }, silent = TRUE)