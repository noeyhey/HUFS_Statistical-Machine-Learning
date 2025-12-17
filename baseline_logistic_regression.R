# 필수 패키지 로드 (없으면 install.packages("pROC") 실행)
library(pROC)

# ==========================================================
# 1. 모델 학습 (Logistic Regression)
# ==========================================================
cat("=== [Baseline] 로지스틱 회귀 학습 시작 ===\n")

# glm 함수 사용 (family = "binomial"은 이항 분류를 뜻함)
# 타겟 변수: default.payment.next.month
# 입력 데이터: 정규화 완료된 trainData_scaled
model_logit <- glm(default.payment.next.month ~ ., 
                   data = trainData_scaled, 
                   family = "binomial")

# 모델 요약 정보 (어떤 변수가 유의미한지 확인용)
# summary(model_logit) # 너무 길면 생략 가능

# ==========================================================
# 2. 예측 수행 (Prediction)
# ==========================================================
cat("=== Test 데이터 예측 수행 ===\n")

# (1) 확률 예측 (ROC-AUC 계산용)
# type="response"는 0~1 사이의 확률값을 반환합니다.
prob_logit <- predict(model_logit, newdata = testData_scaled, type = "response")

# (2) 클래스 예측 (Accuracy, F1 계산용)
# 확률이 0.5보다 크면 "Yes"(연체), 작으면 "No"(정상)
pred_logit_class <- ifelse(prob_logit > 0.5, "Yes", "No")

# 실제 정답 (Factor)
actual_class <- testData_scaled$default.payment.next.month

# ==========================================================
# 3. 성능 평가 (Metrics Calculation)
# ==========================================================
cat("\n=== [Baseline 성적표] ===\n")

# (1) Confusion Matrix (혼동 행렬) 생성
#      Predicted
# Actual  No  Yes
#    No   TN  FP
#    Yes  FN  TP
tbl <- table(Actual = actual_class, Predicted = pred_logit_class)
print(tbl)

# (2) 지표 계산 (Base R로 직접 계산 - 에러 방지)
TP <- tbl["Yes", "Yes"]
TN <- tbl["No", "No"]
FP <- tbl["No", "Yes"]
FN <- tbl["Yes", "No"]

# Accuracy (정확도)
acc <- (TP + TN) / sum(tbl)

# Precision (정밀도): 연체라고 예측한 것 중 실제 연체 비율
precision <- TP / (TP + FP)
if(is.na(precision)) precision <- 0 # 분모가 0일 경우 방지

# Recall (재현율): 실제 연체 중 연체라고 맞춘 비율
recall <- TP / (TP + FN)

# F1-Score (조화평균)
f1 <- 2 * (precision * recall) / (precision + recall)
if(is.na(f1)) f1 <- 0

# AUC (Area Under Curve)
roc_obj <- roc(actual_class, prob_logit, levels = c("No", "Yes"), direction = "<")
auc_score <- auc(roc_obj)

# (3) 결과 출력
cat(sprintf("\n1. Accuracy : %.4f (목표: 0.82+)\n", acc))
cat(sprintf("2. F1-Score : %.4f (목표: 0.53+)\n", f1))
cat(sprintf("3. ROC-AUC  : %.4f (목표: 0.76+)\n", auc_score))

# ==========================================================
# 4. ROC 커브 시각화
# ==========================================================
par(mfrow=c(1,1))
plot(roc_obj, col="Midnight Blue", lwd=2, main="ROC Curve: Logistic Regression (Baseline)")
text(0.5, 0.5, paste0("AUC = ", round(auc_score, 4)), col="Midnight Blue", cex=1.2)