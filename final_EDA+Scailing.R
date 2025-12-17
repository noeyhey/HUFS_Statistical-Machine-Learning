# ==============================================================================
# 0. 환경 설정 및 라이브러리 로드
# ==============================================================================
# install.packages(c("tidyverse", "corrplot", "gridExtra", "e1071", "factoextra", "reshape2", "car"))
setwd("C:/Users/yehye/OneDrive/바탕 화면/기계학습_프로젝트")

library(tidyverse)  # dplyr, ggplot2 등
library(corrplot)   # 상관관계 시각화
library(gridExtra)  # 그래프 배열
library(e1071)      # 왜도 계산
library(reshape2)   # melt 함수
library(factoextra) # PCA 시각화
library(car)        # VIF 계산

# ==============================================================================
# 1. 데이터 로드 및 기초 전처리
# ==============================================================================
df <- read.csv("./data/UCI_Credit_Card.csv")

# ID 제거
if("ID" %in% names(df)) df <- df %>% select(-ID)

# [중요] 타겟 변수 범주형 변환 (0:Normal, 1:Default)
df$default.payment.next.month <- factor(df$default.payment.next.month, 
                                        levels = c(0, 1), 
                                        labels = c("No", "Yes"))

# 데이터 기본 정보 출력
cat("=== 데이터 구조 ===\n")
cat(sprintf("행: %d, 열: %d\n", nrow(df), ncol(df)))
cat("결측치 수:", sum(is.na(df)), "\n")

# ==============================================================================
# 2. [Feature Engineering] PAY 변수 이진화 (EDA 전에 수행)
# 이유: PAY 변수는 범주형 성격이 강하므로 연속형 분석(왜도/로그)에서 제외하기 위함
# ==============================================================================
cat("\n=== [전처리] PAY 변수 이진화 (Feature Engineering) ===\n")

pay_cols <- c("PAY_0", paste0("PAY_", 2:6))

for (col in pay_cols) {
  # 0보다 크면 1(연체), 아니면 0(정상)
  # 원본 값을 덮어씌웁니다.
  df[[col]] <- as.numeric(df[[col]] > 0)
  # 시각화나 모델링 편의를 위해 Factor로 변환 (0=No_Delay, 1=Delay)
  df[[col]] <- factor(df[[col]], levels = c(0, 1), labels = c("No_Delay", "Delay"))
}
cat("PAY 변수 이진화 완료 (Delay vs No_Delay)\n")

# ==============================================================================
# 3. 데이터 탐색 (EDA) - 1차 (로그 변환 전)
# ==============================================================================

# 3-1. 타겟 변수 분포 (불균형 확인)
plot_data <- df %>%
  group_by(default.payment.next.month) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  mutate(label = paste0(round(prop * 100, 1), "%"))

p_target <- ggplot(plot_data, aes(x = default.payment.next.month, y = count, fill = default.payment.next.month)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5) +
  scale_fill_manual(values = c("Midnight Blue", "Burlywood 3")) +
  labs(title = "Target = 연체 여부", x = "Status", y = "Count") +
  theme_minimal()
print(p_target)

# 3-2. 연속형 변수 히스토그램 & 왜도 확인
# (PAY는 이제 범주형이므로 제외하고, 금액 관련 변수만 확인)
cols_amount <- c("LIMIT_BAL", paste0("BILL_AMT", 1:6), paste0("PAY_AMT", 1:6))

# 히스토그램 함수
plot_histograms <- function(data, columns, title) {
  data %>%
    select(all_of(columns)) %>%
    gather(key = "Variable", value = "Value") %>%
    ggplot(aes(x = Value)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "Midnight Blue", alpha = 0.7) +
    geom_density(col = "Burlywood 3", lwd = 1) +
    facet_wrap(~ Variable, scales = "free") +
    theme_minimal() +
    labs(title = title)
}

# 변환 전 히스토그램
print(plot_histograms(df, "LIMIT_BAL", "신용 한도 분포"))
print(plot_histograms(df, paste0("PAY_AMT", 1:6), "월별 납부 금액 분포"))
print(plot_histograms(df, paste0("BILL_AMT", 1:6), "월별 청구 금액 분포"))

# 변환 전 왜도 확인
cat("\n=== [Before] 변수별 왜도(Skewness) ===\n")
skew_vals <- sapply(df[, cols_amount], skewness, na.rm = TRUE)
print(round(skew_vals, 2))

# ==============================================================================
# 4. [전처리] 로그 변환 (Log Transformation)
# ==============================================================================
# cat("\n=== [전처리] 로그 변환 적용 (왜도 해결) ===\n")

# 로그 변환 대상: 연속형 변수 전체 (LIMIT_BAL, BILL, PAY_AMT)
# log_candidates <- cols_amount 

# for(col in log_candidates) {
  # 음수 처리 로직: 최소값이 음수면 (x - min + 1)로 이동 후 로그 변환
  # min_val <- min(df[[col]], na.rm = TRUE)
  
  # if(min_val < 0) {
    # 음수가 있는 경우 (주로 BILL_AMT)
    # df[[col]] <- log1p(df[[col]] - min_val)
    # cat(sprintf("   [Modified] %s : 음수 포함 (Min: %.2f) -> Shift 후 Log1p 적용\n", col, min_val)) } else {
    # 양수만 있는 경우
    # df[[col]] <- log1p(df[[col]])
    # cat(sprintf("   [Done] %s : Log1p 변환 완료\n", col)) }}





cat("\n=== [전처리] 로그 변환 적용 (PAY_AMT 및 기타 변수만, BILL 제외) ===\n")

# 1. 로그 변환 대상 설정: 전체에서 'BILL'이 들어간 이름만 제외 (!grepl 사용)
# 변환 대상: LIMIT_BAL, PAY_AMT1 ~ 6
log_candidates <- cols_amount[!grepl("BILL", cols_amount)]

# 확인용 출력 (잘 빠졌는지 확인)
cat(">>> 로그 변환 적용 대상 변수 목록:\n")
print(log_candidates)

# 2. 루프 실행
for(col in log_candidates) {
  # 음수 처리 로직: 최소값이 음수면 (x - min + 1)로 이동 후 로그 변환
  min_val <- min(df[[col]], na.rm = TRUE)
  
  if(min_val < 0) {
    # 음수가 있는 경우
    df[[col]] <- log1p(df[[col]] - min_val)
    cat(sprintf("   [Modified] %s : 음수 포함 (Min: %.2f) -> Shift 후 Log1p 적용\n", col, min_val))
  } else {
    # 양수만 있는 경우
    df[[col]] <- log1p(df[[col]])
    cat(sprintf("   [Done] %s : Log1p 변환 완료\n", col))
  }
}





# ==============================================================================
# 5. 데이터 탐색 (EDA) - 2차 (로그 변환 후 검증)
# ==============================================================================
cat("\n=== [After] 로그 변환 후 히스토그램 및 왜도 확인 ===\n")

# 변환 후 히스토그램 확인
print(plot_histograms(df, "LIMIT_BAL", "After Log: 신용 한도 분포"))
print(plot_histograms(df, paste0("PAY_AMT", 1:6), "After Log: 월별 납부 금액"))

# 변환 후 왜도 재계산
skew_vals_after <- sapply(df[, log_candidates], skewness, na.rm = TRUE)
print(round(skew_vals_after, 2))









# ==============================================================================
# 6. 상관관계 분석 (로그 변환된 데이터 기준)
# ==============================================================================
# 상관계수 계산을 위해 수치형 변수만 선택 (Target, PAY_n 등 Factor 제외)
numeric_df <- df %>% select(all_of(cols_amount))

# 상관행렬 계산
cor_matrix <- cor(numeric_df)

# BILL_AMT 간의 다중공선성 시각화 (보고서용)
bill_cor <- cor(numeric_df %>% select(starts_with("BILL")))

par(mfrow=c(1,1))
corrplot(bill_cor, 
         method = "number", 
         type = "upper", 
         tl.col = "black", 
         title = "상관관계: 월별 청구 금액", 
         mar = c(0,0,2,0))

# ==============================================================================
# 7. PCA (주성분 분석) - 다중공선성 해결 근거
# ==============================================================================
# 스케일링 후 PCA 수행
pca_res <- prcomp(scale(numeric_df), center = TRUE, scale. = TRUE)

# Scree Plot
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 60), 
         main = "Scree Plot: 주성분 누적 설명력")

# 누적 설명력 확인
eig_val <- get_eigenvalue(pca_res)
cat("\n=== PCA 상위 10개 주성분 누적 설명력 ===\n")
print(head(eig_val, 10))

# ==============================================================================
# 8. 데이터 분할 (Train / Test)
# ==============================================================================
cat("\n=== 데이터 분할 (Train 80% : Test 20%) ===\n")
set.seed(123)
train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
trainData <- df[train_idx, ]
testData  <- df[-train_idx, ]

# ==============================================================================
# 9. [최종 전처리] Robust Scaling (Median & IQR)
# ==============================================================================
# 주의: 이미 로그 변환된 상태에서 스케일링을 한 번 더 수행하여 단위를 맞춥니다.

# 스케일링 대상: 나이(AGE) + 로그변환된 금액 변수들
continuous_cols_to_scale <- c("AGE", cols_amount)

cat("\n=== Robust Scaling 적용 (대상: AGE + 금액변수) ===\n")

# Robust Scale 함수
apply_robust <- function(x, med, iqr_val) {
  if(iqr_val == 0) iqr_val <- 1
  return((x - med) / iqr_val)
}

# Train 기준 통계량 계산
train_meds <- sapply(trainData[, continuous_cols_to_scale], median, na.rm=TRUE)
train_iqrs <- sapply(trainData[, continuous_cols_to_scale], IQR, na.rm=TRUE)

# Train 변환
for(col in continuous_cols_to_scale) {
  trainData[[col]] <- apply_robust(trainData[[col]], train_meds[col], train_iqrs[col])
}

# Test 변환 (Train 통계량 사용)
for(col in continuous_cols_to_scale) {
  testData[[col]] <- apply_robust(testData[[col]], train_meds[col], train_iqrs[col])
}

# 데이터 분리 (스케일링된 데이터프레임 저장)
trainData_scaled <- trainData
testData_scaled <- testData

cat("=== 전처리 완료! (Log -> Robust Scaling) ===\n")
cat("[확인] Train LIMIT_BAL 요약:\n")
print(summary(trainData_scaled$LIMIT_BAL))

# 히스토그램으로 최종 분포 확인 (보고서용)
p_final <- ggplot(trainData_scaled, aes(x = LIMIT_BAL)) +
  geom_histogram(bins=30, color="Midnight Blue", fill="Burlywood 3", alpha=0.7) +
  labs(title="Target 신용 한도 최종 분포(Log + Robust Scaled)") +
  theme_minimal()
print(p_final)
