setwd("C:/Users/yehye/OneDrive/바탕 화면/기계학습_프로젝트")


# [중요] 타겟 변수를 범주형(Factor)으로 변환
target_col_name <- "default.payment.next.month"
df[[target_col_name]] <- factor(df[[target_col_name]], 
                                levels = c(0, 1), 
                                labels = c("No", "Yes"))

# ==========================================================
# 2. [순서 수정] 분할 전에 PAY 변수 이진화 먼저 수행!
# (그래야 train/test 데이터에도 반영됩니다)
# ==========================================================

# 이진화 대상 컬럼 목록
pay_cols <- c("PAY_0", paste0("PAY_", 2:6)) 

# 모든 PAY 컬럼에 대해 이진화 적용
for (col in pay_cols) {
  # (1) 0보다 큰 값은 1(연체), 나머지는 0(정상)으로 변환
  df[[col]] <- as.numeric(df[[col]] > 0)
  
  # (2) Factor 변환 (선택사항)
  df[[col]] <- factor(df[[col]], levels = c(0, 1), labels = c("No_Delay", "Delay"))
}

# ==========================================================
# 3. 데이터 분할 (Train 80% : Test 20%)
# ==========================================================
set.seed(123)
train_rows <- sample(nrow(df), size = floor(0.8 * nrow(df)))
trainData <- df[train_rows, ]
testData  <- df[-train_rows, ]


# ==========================================================
# 3. [최종 해결] 정규화 (Standardization) - 수동 계산 방식
# ==========================================================

# (1) 정규화 대상 연속형 변수 14개 선택
continuous_cols <- c("LIMIT_BAL", "AGE", 
                     paste0("BILL_AMT", 1:6), 
                     paste0("PAY_AMT", 1:6))

# (2) Train/Test 데이터를 행렬(Matrix)로 변환
X_train_cont <- data.matrix(trainData[, continuous_cols])
X_test_cont  <- data.matrix(testData[, continuous_cols])

# [확인] 컬럼 개수 재확인
cat("Train 컬럼 수:", ncol(X_train_cont), "/n")
cat("Test  컬럼 수:", ncol(X_test_cont), "/n")

# (3) [핵심] Train 데이터의 평균과 표준편차를 **직접 계산**
# scale()의 자동 계산에 의존하지 않고, 직접 구해서 변수에 저장합니다.
train_mean <- colMeans(X_train_cont, na.rm = TRUE)
train_sd   <- apply(X_train_cont, 2, sd, na.rm = TRUE)

# [디버깅] 계산된 통계량 개수 확인 (여기서 14가 나와야 합니다)
cat("계산된 평균(Mean) 개수:", length(train_mean), "/n")
cat("계산된 표준편차(SD) 개수:", length(train_sd), "/n")

# (4) Train 데이터 스케일링
# 위에서 구한 값을 직접 넣어줍니다.
X_train_scaled_matrix <- scale(X_train_cont, center = train_mean, scale = train_sd)

# (5) Test 데이터 스케일링
# Train에서 구한 train_mean과 train_sd를 그대로 사용합니다.
# 직접 변수를 넣기 때문에 '길이 불일치' 에러가 날 수 없습니다.
X_test_scaled_matrix <- scale(X_test_cont, center = train_mean, scale = train_sd)

# 4. 데이터 재결합 (이전과 동일)
X_train_scaled_df <- as.data.frame(X_train_scaled_matrix)
X_test_scaled_df <- as.data.frame(X_test_scaled_matrix)

non_scaled_cols <- setdiff(names(trainData), continuous_cols)

trainData_scaled <- cbind(X_train_scaled_df, trainData[, non_scaled_cols])
testData_scaled <- cbind(X_test_scaled_df, testData[, non_scaled_cols])

cat("/n=== 정규화가 에러 없이 완료되었습니다! ===/n")

# 5. 결과 확인
cat("=== [변환 후] LIMIT_BAL 요약 ===/n")
summary(trainData_scaled$LIMIT_BAL)


# ==========================================================
# 6. 결과 확인
# ==========================================================
cat("=== [변환 후] LIMIT_BAL 요약 ===/n")
summary(trainData_scaled$LIMIT_BAL)
cat("표준편차:", sd(trainData_scaled$LIMIT_BAL), "/n")

# 시각화
par(mfrow=c(1,2))
hist(trainData$LIMIT_BAL, main="Original", col="lightblue", xlab="Limit Balance")
hist(trainData_scaled$LIMIT_BAL, main="Scaled", col="salmon", xlab="Scaled Value")