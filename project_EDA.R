# install.packages(c("tidyverse", "corrplot", "gridExtra"))
setwd("C:/Users/yehye/OneDrive/바탕 화면/기계학습_프로젝트")

# 라이브러리 로드
library(tidyverse)  # 데이터 전처리(dplyr) 및 시각화(ggplot2) 포함
library(corrplot)   # 상관관계 히트맵용
library(gridExtra)  # 그래프 배열(subplot)용

# 1. 데이터 로드
df <- read.csv("./data/UCI_Credit_Card.csv")

# ID 컬럼 제거 (옵션)
if("ID" %in% names(df)) {
  df <- df %>% select(-ID)
}

# 2. 데이터 기본 정보 출력
cat("=== 데이터 구조 (Shape) ===\n")
cat(sprintf("행: %d, 열: %d\n", nrow(df), ncol(df)))

cat("\n=== 결측치 확인 ===\n")
na_count <- sum(is.na(df))
cat(na_count, "개 (깨끗한 데이터임을 확인)\n")

# 3. 타겟 변수 분포 (Class Imbalance 확인)
cat("\n=== 타겟 변수 분포 (0:정상, 1:연체) ===\n")
target_dist <- prop.table(table(df$default.payment.next.month))
print(target_dist)

# 시각화: 타겟 분포
# 3-1. 시각화를 위한 요약 데이터 생성 (빈도수 및 비율 계산)
plot_data <- df %>%
  group_by(default.payment.next.month) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  mutate(label = paste0(round(prop * 100, 1), "%")) # 그래프에 표시할 라벨 생성

# 3-2. 그래프 그리기
ggplot(plot_data, aes(x = factor(default.payment.next.month), y = count, fill = factor(default.payment.next.month))) +
  geom_bar(stat = "identity", width = 0.6) + # 막대 그래프
  geom_text(aes(label = label), vjust = -0.5, size = 5, fontface = "bold") + # 위에 % 표시
  scale_fill_manual(values = c("steelblue", "coral"), 
                    labels = c("Normal (0)", "Default (1)")) + # 색상 지정
  scale_x_discrete(labels = c("Normal (0)", "Default (1)")) + # X축 이름 변경
  labs(title = "Distribution of Target Variable",
       subtitle = paste0("Total Samples: ", sum(plot_data$count)),
       x = "Payment Status",
       y = "Count",
       fill = "Class") +
  theme_minimal() + # 깔끔한 테마 적용
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text = element_text(size = 12))


library(ggplot2)
library(tidyr)
library(dplyr)
library(e1071) # 왜도(Skewness) 계산용

# 1. 데이터 다시 로드
df <- read.csv("./data/UCI_Credit_Card.csv")

# 2. 시각화할 연속형 변수 그룹 정의
cols_amount <- c("LIMIT_BAL", paste0("BILL_AMT", 1:6), paste0("PAY_AMT", 1:6))

# 3. [핵심] 히스토그램 그리기 함수 정의
plot_histograms <- function(data, columns, title) {
  data %>%
    select(all_of(columns)) %>%
    gather(key = "Variable", value = "Value") %>% # 데이터를 길게 변환 (Plotting용)
    ggplot(aes(x = Value)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_density(col = "red", lwd = 1) + # 분포 곡선 추가
    facet_wrap(~ Variable, scales = "free") + # 변수별로 쪼개서 그리기
    theme_minimal() +
    labs(title = title, x = "Amount", y = "Density")
}

# 4. 실행: 히스토그램 확인
# (1) LIMIT_BAL (신용한도)
print(plot_histograms(df, "LIMIT_BAL", "Distribution of Credit Limit (LIMIT_BAL)"))

# (2) BILL_AMT (청구 금액)
print(plot_histograms(df, paste0("BILL_AMT", 1:6), "Distribution of Bill Amounts"))

# (3) PAY_AMT (납부 금액)
print(plot_histograms(df, paste0("PAY_AMT", 1:6), "Distribution of Pay Amounts"))

# 5. [정량적 확인] 왜도(Skewness) 계산
# 0에 가까우면 대칭, 절대값이 2 이상이면 심한 치우침
cat("\n=== 변수별 왜도(Skewness) 확인 ===\n")
skew_vals <- sapply(df[, cols_amount], skewness, na.rm = TRUE)
print(round(skew_vals, 2))


# ====================================================
# [NEW] 로그 변환 (Log Transformation) 적용
# ====================================================
cat("\n=== [전처리] 로그 변환 적용 시작 (왜도 해결) ===\n")

# 1. 로그 변환 대상 변수 정의 (왜도가 심했던 금액 변수들)
# LIMIT_BAL, BILL_AMT1~6, PAY_AMT1~6
# 주의: BILL_AMT에 음수가 있다면 그 변수는 제외하거나 별도 처리가 필요합니다.
# 여기서는 간단히 '음수가 없는 변수'만 자동으로 찾아 변환합니다.

log_candidates <- c("LIMIT_BAL", paste0("BILL_AMT", 1:6), paste0("PAY_AMT", 1:6))

for(col in log_candidates) {
  # (1) 음수 체크: 최소값이 0보다 작으면 로그 변환 시 NaN 발생하므로 건너뜀
  if(min(df[[col]], na.rm = TRUE) < 0) {
    cat(sprintf("   [Skip] %s : 음수 값 존재 (Min: %.2f) -> 로그 변환 제외\n", 
                col, min(df[[col]], na.rm = TRUE)))
  } else {
    # (2) 로그 변환 적용: log(x + 1) 사용
    df[[col]] <- log1p(df[[col]])
    cat(sprintf("   [Done] %s : Log1p 변환 완료\n", col))
  }
}

# 2. 변환 후 분포 재확인 (2차 EDA)
cat("\n=== [검증] 로그 변환 후 히스토그램 확인 ===\n")

# LIMIT_BAL 재확인
print(plot_histograms(df, "LIMIT_BAL", "Distribution of Log-Transformed LIMIT_BAL"))

# PAY_AMT 재확인 (가장 심각했던 변수)
print(plot_histograms(df, paste0("PAY_AMT", 1:6), "Distribution of Log-Transformed PAY_AMT"))

# 왜도 재계산 (얼마나 개선되었는지 수치로 확인)
skew_vals_after <- sapply(df[, log_candidates], skewness, na.rm = TRUE)
cat("\n--- 변환 후 왜도(Skewness) 수치 ---\n")
print(round(skew_vals_after, 2))


# ====================================================
# [Modified] Robust Scaling (Median & IQR)
# ====================================================

cat("=== [전처리] Robust Scaling (중앙값 기반 정규화) 적용 시작 ===\n")

# 1. [수정] 스케일링 대상 변수 명확히 정의 (여기서 정의가 누락되었었습니다)
# 금액 변수들 (LIMIT_BAL, BILL_AMT1~6, PAY_AMT1~6)
finance_cols <- c("LIMIT_BAL", paste0("BILL_AMT", 1:6), paste0("PAY_AMT", 1:6))

# 최종 스케일링 대상: 나이(AGE) + 금액 변수들
continuous_cols_to_scale <- c("AGE", finance_cols)

cat("스케일링 대상 변수 개수:", length(continuous_cols_to_scale), "개\n")

# 2. Robust Scaling 함수 정의 (수동 구현)
apply_robust_scale <- function(x, median_val, iqr_val) {
  # IQR이 0인 경우(데이터가 거의 같은 값) 나눗셈 에러 방지 -> 1로 대체
  if (iqr_val == 0) iqr_val <- 1 
  return ((x - median_val) / iqr_val)
}

# 3. Train 데이터 기준 통계량(Median, IQR) 계산
# (sapply를 사용하여 각 컬럼별로 계산)
train_medians <- sapply(trainData[, continuous_cols_to_scale], median, na.rm = TRUE)
train_iqrs    <- sapply(trainData[, continuous_cols_to_scale], IQR, na.rm = TRUE)

# 4. Train 데이터 변환
trainData_robust <- trainData # 복사본 생성
for (col in continuous_cols_to_scale) {
  trainData_robust[[col]] <- apply_robust_scale(trainData[[col]], 
                                                train_medians[col], 
                                                train_iqrs[col])
}

# 5. Test 데이터 변환 (Train 통계량 사용!)
testData_robust <- testData # 복사본 생성
for (col in continuous_cols_to_scale) {
  testData_robust[[col]] <- apply_robust_scale(testData[[col]], 
                                               train_medians[col], 
                                               train_iqrs[col])
}

# 6. 데이터 재결합 (범주형 변수 합치기)
# category_cols는 이미 정의되어 있다고 가정하지만, 혹시 모르니 다시 안전하게 정의
category_cols <- setdiff(names(df), c(continuous_cols_to_scale, "ID")) 

trainData_final <- cbind(trainData_robust[, continuous_cols_to_scale], 
                         trainData[, category_cols])
testData_final  <- cbind(testData_robust[, continuous_cols_to_scale], 
                         testData[, category_cols])

# (중요) 변수명 일치 시키기 (이후 모델링 코드와 연결)
trainData_scaled <- trainData_final
testData_scaled <- testData_final

cat("=== Robust Scaling 완료! ===\n")
cat("Train 크기:", dim(trainData_scaled), "\n")
cat("Test  크기:", dim(testData_scaled), "\n")

# 7. 분포 확인 (선택 사항)
# 이상치가 잘 제어되었는지 LIMIT_BAL로 확인
cat("\n[확인] Robust Scaling 후 요약 통계량 (LIMIT_BAL)\n")
summary(trainData_scaled$LIMIT_BAL)



## 상관관계 ##
# 라이브러리 로드
library(reshape2) # melt 함수를 쓰기 위해 필요

# 1. 상관계수 행렬 계산
# (주의: 수치형 변수만 선택해야 에러가 안 납니다)
cor_matrix <- cor(df)

# 2. 데이터 변환 (Matrix -> Data Frame)
# Var1(변수1), Var2(변수2), value(상관계수) 형태가 됩니다.
melted_cor <- melt(cor_matrix)

# 3. ggplot으로 그리기
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + # 히트맵 타일 생성
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1), # X축 글씨 회전
        axis.text.y = element_text(size = 10)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = "", y = "")

# 4. 다중공선성 시각화
# 이 그래프가 "왜 PCA나 Lasso를 써야하는지" 보여주는 가장 중요한 근거입니다.

# 필수 라이브러리 로드
library(tidyverse)
library(corrplot)

# 4-1. 분석 대상: 월별 청구 금액 (BILL_AMT1 ~ BILL_AMT6) 추출
# BILL_AMT1: 9월 청구액 ~ BILL_AMT6: 4월 청구액
bill_cols <- c('BILL_AMT1', 'BILL_AMT2', 'BILL_AMT3', 
               'BILL_AMT4', 'BILL_AMT5', 'BILL_AMT6')

bill_data <- df %>% select(all_of(bill_cols))

# 4-2. 상관계수 행렬 계산 (소수점 4자리까지)
cor_matrix <- cor(bill_data)
print(round(cor_matrix, 4))

# 4-3. 시각화: 상관계수 히트맵 (숫자 직접 표시)
# 이 그래프를 캡처해서 보고서 '데이터 탐색' 파트에 넣으세요.
par(mfrow=c(1,1))
corrplot(cor_matrix, 
         method = "number",       # 숫자로 표시 (가장 확실한 증거)
         type = "upper",          # 대각선 윗부분만 표시
         tl.col = "black",        # 라벨 색상
         tl.srt = 45,             # 라벨 각도
         number.cex = 1.2,        # 글자 크기 키움
         col = colorRampPalette(c("white", "red"))(10), # 붉은색 강조
         title = "Correlation betwen Bill Amounts (High Multicollinearity)",
         mar = c(0,0,2,0))


pay_cols <- c('PAY_0', 'PAY_2', 'PAY_3', 
               'PAY_4', 'PAY_5', 'PAY_6')

pay_data <- df %>% select(all_of(pay_cols))

# 4-2. 상관계수 행렬 계산 (소수점 4자리까지)
cor_matrix_pay <- cor(pay_data)
print(round(cor_matrix_pay, 4))

# 4-3. 시각화: 상관계수 히트맵 (숫자 직접 표시)
# 이 그래프를 캡처해서 보고서 '데이터 탐색' 파트에 넣으세요.
par(mfrow=c(1,1))
corrplot(cor_matrix_pay, 
         method = "number",       # 숫자로 표시 (가장 확실한 증거)
         type = "upper",          # 대각선 윗부분만 표시
         tl.col = "black",        # 라벨 색상
         tl.srt = 45,             # 라벨 각도
         number.cex = 1.2,        # 글자 크기 키움
         col = colorRampPalette(c("white", "red"))(10), # 붉은색 강조
         title = "Correlation betwen PAY (High Multicollinearity)",
         mar = c(0,0,2,0))

# 4-4. 분산팽창지수(VIF) 확인
# VIF가 10을 넘으면 "다중공선성이 심각하다"고 판단합니다.

library(car)

# 가상의 선형 모델을 만들어 VIF 계산
# (예: PAY1을 나머지 변수들로 설명할 수 있는지 확인)
lm_model_pay <- lm(PAY_0 ~ PAY_2 + PAY_3 + PAY_4
                   + PAY_5 + PAY_6, data = df)
vif_values_pay <- vif(lm_model_pay)

cat("\n=== VIF(분산팽창지수) 결과 ===\n")
print(vif_values_pay)




# 5. 주요 범주형 변수별 연체율 확인 (성별, 교육, 결혼)
# R에서는 factor로 변환 후 평균을 구하거나 stat_summary를 사용합니다.

# 그래프 1: 성별
p1 <- ggplot(df, aes(x = factor(SEX), y = default.payment.next.month)) +
  stat_summary(fun = "mean", geom = "bar", fill = "#BBDEFB") +
  labs(title = "Default Rate by Sex", x = "Sex (1:Male, 2:Female)", y = "Default Rate") +
  theme_minimal()

# 그래프 2: 교육
p2 <- ggplot(df, aes(x = factor(EDUCATION), y = default.payment.next.month)) +
  stat_summary(fun = "mean", geom = "bar", fill = "#64B5F6") +
  labs(title = "Default Rate by Education", x = "Education", y = "Default Rate") +
  theme_minimal()

# 그래프 3: 결혼
p3 <- ggplot(df, aes(x = factor(MARRIAGE), y = default.payment.next.month)) +
  stat_summary(fun = "mean", geom = "bar", fill = "#2196F3") +
  labs(title = "Default Rate by Marriage", x = "Marriage", y = "Default Rate") +
  theme_minimal()

# 3개의 그래프를 나란히 배치 (Python의 plt.subplots(1,3) 효과)
grid.arrange(p1, p2, p3, ncol = 3)

# 요약 결론 출력
cat("\n=== EDA 요약 결론 ===\n")
cat("1. 데이터 불균형(약 22%만 연체)이 존재하므로 평가 지표로 F1-score, AUC가 필요함.\n")
cat("2. PAY 변수들 간의 상관계수가 0.8 이상으로 매우 높아 다중공선성 문제가 확인됨.\n")
cat("   -> 해결책: 07_Regularization(Lasso) 또는 11_Unsupervised(PCA) 적용 예정.\n")







library(factoextra)  # PCA, 군집화 시각화용

# X(설명변수)와 Y(타겟변수) 분리
# 타겟 변수명은 'default.payment.next.month' 입니다.
X <- df %>% select(-default.payment.next.month)
y <- df$default.payment.next.month

# [중요] 비지도 학습과 규제 모델은 '스케일링(Standardization)'이 필수입니다.
# 평균 0, 표준편차 1로 변환
X_scaled <- scale(X)

print("=== 데이터 준비 완료 ===")

# ====================================================
# PCA (주성분 분석): 차원 축소 및 정보 보존량 확인
# ====================================================
# PCA 수행
pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# (1) Scree Plot: 주성분별 정보(분산) 설명력 시각화
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50), 
         main = "Scree Plot: Explained Variance by PC")

# (2) 누적 설명력 확인
eig_val <- get_eigenvalue(pca_result)
print("=== PCA 주성분 누적 설명력 (상위 10개) ===")
print(head(eig_val, 10))

# 해석: "누적 분산(cumulative.variance.percent)을 보면, 
# PC1~PC15 정도면 전체 정보의 83% 이상을 설명함을 알 수 있음."