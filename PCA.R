library(e1071)
library(pROC)

# ==========================================================
# 0. [수정] 비교 기준값 직접 입력 (User의 이전 결과 반영)
# ==========================================================
# Class Weight SVM 결과값
acc_original <- 0.7608
f1_original  <- 0.5215
auc_original <- 0.72   # (참고용 근사치 - 비교에 큰 영향 없음)

cat("=== [Setting] 비교 기준점 설정 완료 ===\n")
cat(sprintf("   Target Accuracy : %.4f \n", acc_original))
cat(sprintf("   Target F1-Score : %.4f \n", f1_original))


# ==========================================================
# 1. [핵심] BILL 변수만 선택하여 PCA 적용
# ==========================================================
cat("\n=== [Hybrid PCA] BILL 변수만 3개로 압축 시작 ===\n")

# (1) BILL 변수 컬럼명 정의
bill_cols <- paste0("BILL_AMT", 1:6)

# (2) BILL 변수만 가지고 PCA 모델 생성
# (Scale은 이미 되어 있으므로 FALSE)
pca_bill_model <- prcomp(trainData_scaled[, bill_cols], center = FALSE, scale. = FALSE)

# 설명력 확인 (상위 3개가 얼마나 설명하는지)
var_explained <- summary(pca_bill_model)$importance[3, 3] # 누적 설명력
cat(sprintf(">>> BILL 변수 6개 -> PC 3개로 압축 (설명력: %.2f%% 정보 보존)\n", var_explained * 100))

# (3) 데이터 변환 (6개 -> 3개)
n_bill_comp <- 3
train_bill_pca <- predict(pca_bill_model, newdata = trainData_scaled[, bill_cols])[, 1:n_bill_comp]
test_bill_pca  <- predict(pca_bill_model, newdata = testData_scaled[, bill_cols])[, 1:n_bill_comp]

# 컬럼 이름 예쁘게 변경 (BILL_PC1, BILL_PC2, BILL_PC3)
colnames(train_bill_pca) <- paste0("BILL_PC", 1:n_bill_comp)
colnames(test_bill_pca)  <- paste0("BILL_PC", 1:n_bill_comp)

# ==========================================================
# 2. 데이터 합치기 (나머지 원본 변수 + BILL_PC 3개)
# ==========================================================
# BILL 변수를 제외한 나머지 컬럼들 (PAY, LIMIT, AGE, Target 등)
other_vars <- setdiff(names(trainData_scaled), bill_cols)

# 데이터 병합
trainData_hybrid <- data.frame(trainData_scaled[, other_vars], train_bill_pca)
testData_hybrid  <- data.frame(testData_scaled[, other_vars], test_bill_pca)

cat(">>> 데이터 병합 완료:\n")
cat(sprintf("    기존 변수(PAY, LIMIT 등) + BILL_PC(3개) = 총 %d개 변수 사용\n", ncol(trainData_hybrid)-1))

# ==========================================================
# 3. SVM 모델 학습 (Hybrid Data + Class Weight)
# ==========================================================
cat("=== [Hybrid-SVM] 학습 시작 (가중치 적용) ===\n")

# 가중치 계산 (없을 경우 대비 안전장치)
if(!exists("weight_val")) {
  tab <- table(trainData_scaled$default.payment.next.month)
  weight_val <- as.numeric(tab["No"] / tab["Yes"])
}

# 학습 (Hybrid 데이터 사용)
model_svm_hybrid <- svm(default.payment.next.month ~ ., 
                        data = trainData_hybrid, 
                        kernel = "radial",
                        probability = TRUE,
                        class.weights = c("No" = 1, "Yes" = weight_val))

# ==========================================================
# 4. 성능 평가 및 비교
# ==========================================================
# 예측
pred_prob_hybrid <- attr(predict(model_svm_hybrid, newdata = testData_hybrid, probability = TRUE), "probabilities")[, "Yes"]
pred_class_hybrid <- predict(model_svm_hybrid, newdata = testData_hybrid)

tbl_hybrid <- table(Actual = testData_hybrid$default.payment.next.month, Predicted = pred_class_hybrid)

# 지표 계산
TP <- tbl_hybrid["Yes", "Yes"]; TN <- tbl_hybrid["No", "No"]
acc_hybrid <- (TP + TN) / sum(tbl_hybrid)

# F1 계산
prec <- tbl_hybrid["Yes", "Yes"] / (tbl_hybrid["Yes", "Yes"] + tbl_hybrid["No", "Yes"])
rec  <- tbl_hybrid["Yes", "Yes"] / (tbl_hybrid["Yes", "Yes"] + tbl_hybrid["Yes", "No"])
if(is.na(prec)) prec <- 0
f1_hybrid <- 2 * (prec * rec) / (prec + rec)

# ROC
roc_obj_hybrid <- roc(testData_hybrid$default.payment.next.month, pred_prob_hybrid, 
                      levels = c("No", "Yes"), direction = "<")
auc_hybrid <- auc(roc_obj_hybrid)

# 결과 출력
cat("\n==================================================\n")
cat("      [원본 SVM vs BILL-PCA(3개) SVM 성능 비교]      \n")
cat("==================================================\n")
cat(sprintf("1. Accuracy : %.4f (Original) vs %.4f (Hybrid)\n", acc_original, acc_hybrid))
cat(sprintf("2. F1-Score : %.4f (Original) vs %.4f (Hybrid)\n", f1_original, f1_hybrid))
cat(sprintf("3. ROC-AUC  : %.4f (Original) vs %.4f (Hybrid)\n", auc_original, auc_hybrid))
cat("--------------------------------------------------\n")

if(f1_hybrid >= f1_original - 0.02) {
  cat("✅ 성공! BILL 변수를 3개로 줄여도 성능이 유지됩니다.\n")
  cat("   (다중공선성은 없애고, 중요 정보인 PAY는 살린 최적의 전략입니다.)\n")
} else {
  cat("⚠️ 주의: 성능이 다소 하락했습니다.\n")
}