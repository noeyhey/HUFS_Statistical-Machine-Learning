library(e1071) # SVM 전용 패키지

# ==========================================================
# 1. SVM 모델 학습 (RBF Kernel)
# ==========================================================
cat("=== [Challenger] SVM (비선형) 학습 시작... (잠시만 기다려주세요) ===\n")

# probability = TRUE: 나중에 AUC를 구하기 위해 확률값이 필요함
# gamma, cost는 기본값 사용 (시간 관계상 튜닝 생략, 기본 성능으로도 충분히 비교 가능)
model_svm <- svm(default.payment.next.month ~ ., 
                 data = trainData_scaled, 
                 kernel = "radial",      # 비선형(RBF) 커널 사용
                 probability = TRUE)     # 확률 예측 활성화

cat("=== SVM 학습 완료! 예측을 수행합니다 ===\n")

# ==========================================================
# 2. 예측 수행 (Prediction)
# ==========================================================

# (1) 확률 예측 (ROC-AUC용)
# SVM은 predict 결과의 속성(attribute)에 확률이 숨어있습니다.
pred_obj <- predict(model_svm, newdata = testData_scaled, probability = TRUE)
prob_svm <- attr(pred_obj, "probabilities")[, "Yes"]

# (2) 클래스 예측 (Accuracy, F1용)
pred_svm_class <- predict(model_svm, newdata = testData_scaled)

# ==========================================================
# 3. 성능 평가 (SVM Scorecard)
# ==========================================================
cat("\n=== [SVM (비선형) 성적표] ===\n")

# 혼동 행렬
tbl_svm <- table(Actual = actual_class, Predicted = pred_svm_class)
print(tbl_svm)

# 지표 계산
TP_svm <- tbl_svm["Yes", "Yes"]
TN_svm <- tbl_svm["No", "No"]
FP_svm <- tbl_svm["No", "Yes"]
FN_svm <- tbl_svm["Yes", "No"]

# Accuracy
acc_svm <- (TP_svm + TN_svm) / sum(tbl_svm)

# Precision & Recall
prec_svm <- TP_svm / (TP_svm + FP_svm)
if(is.na(prec_svm)) prec_svm <- 0
rec_svm <- TP_svm / (TP_svm + FN_svm)

# F1-Score
f1_svm <- 2 * (prec_svm * rec_svm) / (prec_svm + rec_svm)
if(is.na(f1_svm)) f1_svm <- 0

# AUC
roc_obj_svm <- roc(actual_class, prob_svm, levels = c("No", "Yes"), direction = "<")
auc_svm <- auc(roc_obj_svm)

# 결과 출력
cat(sprintf("\n1. Accuracy : %.4f (Logistic: 0.8102)\n", acc_svm))
cat(sprintf("2. F1-Score : %.4f (Logistic: 0.3996)\n", f1_svm))
cat(sprintf("3. ROC-AUC  : %.4f (Logistic: 0.7425)\n", auc_svm))

# ==========================================================
# 4. 최종 비교 시각화 (하이라이트)
# ==========================================================
# 두 모델의 ROC 곡선을 겹쳐 그려서 성능 차이를 시각적으로 증명
par(mfrow=c(1,1))
plot(roc_obj, col="Midnight Blue", lty=2, main="Final Battle: Linear vs Non-Linear")
plot(roc_obj_svm, col="Burlywood 3", add=TRUE) # SVM 곡선 추가
legend("bottomright", 
       legend=c(paste0("Logistic (AUC=", round(auc_score, 4), ")"), 
                paste0("SVM (AUC=", round(auc_svm, 4), ")")),
       col=c("Midnight Blue", "Burlywood 3"), lty=c(2, 1), lwd=2)




# 그리드 서치치
library(e1071)
library(pROC)

# ==========================================================
# 1. 튜닝을 위한 데이터 샘플링 (속도 향상)
# ==========================================================
set.seed(123)
# 전체 Train 데이터 중 2,000개만 무작위 추출하여 최적값 탐색
sample_idx <- sample(nrow(trainData_scaled), 2000)
tune_data <- trainData_scaled[sample_idx, ]

cat("=== [Hyperparameter Tuning] 최적 파라미터 탐색 시작 (Grid Search) ===\n")
cat("시간 절약을 위해 2,000개 샘플로 진행합니다...\n")

# ==========================================================
# 2. Grid Search 실행 (tune 함수)
# ==========================================================
# ranges 리스트에 실험할 파라미터 후보들을 넣습니다.
# cost: 클수록 엄격함 (1, 10, 100)
# gamma: 클수록 경계가 복잡해짐 (0.01, 0.1, 0.5, 1)
tune_result <- tune(svm, default.payment.next.month ~ ., 
                    data = tune_data, 
                    kernel = "radial",
                    ranges = list(cost = c(1, 10, 100),
                                  gamma = c(0.01, 0.1, 0.5, 1)),
                    tunecontrol = tune.control(cross = 5)) # 5-Fold CV

# 최적 파라미터 확인
best_gamma <- tune_result$best.parameters$gamma
best_cost <- tune_result$best.parameters$cost

cat("\n>>> 찾은 최적 파라미터 <<<\n")
cat("Best Gamma:", best_gamma, "\n")
cat("Best Cost :", best_cost, "\n")

# ==========================================================
# 3. 최적 파라미터로 '전체 데이터' 재학습 (Final Model)
# ==========================================================
cat("\n=== [Final Battle] 최적 파라미터로 전체 데이터 학습 시작 ===\n")

model_svm_best <- svm(default.payment.next.month ~ ., 
                      data = trainData_scaled, 
                      kernel = "radial",
                      gamma = best_gamma,    # 찾은 최적값 적용
                      cost = best_cost,      # 찾은 최적값 적용
                      probability = TRUE)    # AUC용 확률 계산 필수

# ==========================================================
# 4. 최종 예측 및 성능 평가
# ==========================================================
# (1) 확률 예측
pred_obj_best <- predict(model_svm_best, newdata = testData_scaled, probability = TRUE)
prob_svm_best <- attr(pred_obj_best, "probabilities")[, "Yes"]

# (2) 클래스 예측
pred_class_best <- predict(model_svm_best, newdata = testData_scaled)

# (3) 성적표 작성
tbl_best <- table(Actual = testData_scaled$default.payment.next.month, 
                  Predicted = pred_class_best)

# 지표 계산
TP <- tbl_best["Yes", "Yes"]
TN <- tbl_best["No", "No"]
FP <- tbl_best["No", "Yes"]
FN <- tbl_best["Yes", "No"]

acc_best <- (TP + TN) / sum(tbl_best)
prec_best <- TP / (TP + FP)
if(is.na(prec_best)) prec_best <- 0
rec_best <- TP / (TP + FN)
f1_best <- 2 * (prec_best * rec_best) / (prec_best + rec_best)

roc_obj_best <- roc(testData_scaled$default.payment.next.month, prob_svm_best, 
                    levels = c("No", "Yes"), direction = "<")
auc_best <- auc(roc_obj_best)

# ==========================================================
# 5. 결과 출력 (Before vs After)
# ==========================================================
cat("\n============================================\n")
cat("      [SVM 튜닝 전후 성능 비교]      \n")
cat("============================================\n")
cat(sprintf("1. Accuracy : %.4f  -->  %.4f\n", 0.8137, acc_best))
cat(sprintf("2. F1-Score : %.4f  -->  %.4f\n", 0.4371, f1_best))
cat(sprintf("3. ROC-AUC  : %.4f  -->  %.4f\n", 0.7186, auc_best))
cat("--------------------------------------------\n")
cat("사용된 파라미터: Gamma =", best_gamma, "/ Cost =", best_cost, "\n")

# ROC 커브 비교 시각화 (튜닝 전 vs 튜닝 후)
par(mfrow=c(1,1))
plot(roc_obj_best, col="Midnight Blue", lwd=2, main="SVM: Default vs Tuned")
# (참고: 이전 roc_obj_svm이 메모리에 있다면 아래 주석 해제하여 비교 가능)
# plot(roc_obj_svm, col="grey", lty=2, add=TRUE) 
# legend("bottomright", legend=c("Tuned SVM", "Default SVM"), col=c("red", "grey"), lwd=2)






# class weight

# ==========================================================
# [Final Fix] 데이터 레벨("No"/"Yes")에 맞춰 가중치 적용
# ==========================================================

# 1. 안전을 위해 가중치 비율 다시 계산 (변수가 꼬였을 수 있으므로)
tab <- table(trainData_scaled$default.payment.next.month)
# No 개수 대비 Yes가 얼마나 적은지 계산 (예: No가 4배 많으면 가중치 4)
weight_val <- as.numeric(tab["No"] / tab["Yes"]) 

cat(sprintf(">>> 데이터 레벨 확인: %s, %s \n", levels(trainData_scaled$default.payment.next.month)[1], levels(trainData_scaled$default.payment.next.month)[2]))
cat(sprintf(">>> 적용할 가중치(Yes): %.2f (No는 1로 고정)\n", weight_val))

# 2. SVM 학습 실행
# 주의: class.weights의 이름(Key)이 데이터의 레벨("No", "Yes")과 정확히 같아야 함!
model_svm_weighted <- svm(default.payment.next.month ~ ., 
                          data = trainData_scaled, 
                          kernel = "radial",
                          probability = TRUE,
                          class.weights = c("No" = 1, "Yes" = weight_val)) 

cat("\n>>> 모델 학습 완료! (성공)\n")

# ==========================================================
# 3. 결과 확인 (F1-Score 상승 확인)
# ==========================================================
# 예측
pred_class <- predict(model_svm_weighted, newdata = testData_scaled)
tbl <- table(Actual = testData_scaled$default.payment.next.month, Predicted = pred_class)

# 성능 지표 계산
TP <- tbl["Yes", "Yes"]
TN <- tbl["No", "No"]
FP <- tbl["No", "Yes"]
FN <- tbl["Yes", "No"]

acc <- (TP + TN) / sum(tbl)
prec <- TP / (TP + FP)
rec  <- TP / (TP + FN)
f1   <- 2 * (prec * rec) / (prec + rec)

cat("\n=== [최종 결과: Class Weight SVM] ===\n")
cat(sprintf("1. Accuracy : %.4f\n", acc))
cat(sprintf("2. F1-Score : %.4f  (목표: 0.53+)\n", f1))
cat(sprintf("3. Recall   : %.4f  (실제 연체자를 찾은 비율)\n", rec))






# ROC 커브 비교 시각화 (튜닝 전 vs 튜닝 후)
par(mfrow=c(1,1))
plot(roc_obj_best, col="Midnight Blue", lwd=2, main="SVM: Default vs Tuned")
# (참고: 이전 roc_obj_svm이 메모리에 있다면 아래 주석 해제하여 비교 가능)
plot(roc_obj_svm, col="Burlywood 3", lty=2, add=TRUE) 
legend("bottomright", legend=c("Tuned SVM", "Default SVM"), col=c("Midnight Blue", "Burlywood 3"), lwd=2)







# F1-Score 비교 막대그래프 코드
f1_scores <- c(0.3996, 0.4541)
names(f1_scores) <- c("Logistic (Linear)", "SVM (Non-Linear)")

par(mfrow=c(1,1))
barplot(f1_scores, 
        col=c("Burlywood 3", "Midnight Blue"), 
        ylim=c(0, 0.6), 
        main="결과 비교: F1-Score",
        ylab="F1-Score")
text(x=c(0.7, 1.9), y=f1_scores, label=round(f1_scores, 4), pos=3, cex=1.5, col="black")













library(ggplot2)

# 1. 데이터 생성 (사용자의 실제 실험 결과 반영)
results_df <- data.frame(
  Method = rep(c("Grid Search", "Class Weight"), each = 3),
  Metric = rep(c("Accuracy", "F1-Score", "Recall"), 2),
  Value = c(
    0.8117, 0.4223, 0.2800,  # Grid Search 결과 (Recall은 추정치: 낮음)
    0.7608, 0.5215, 0.6062   # Class Weight 결과 (Recall 대폭 상승)
  )
)

# 2. 그래프 그리기
ggplot(results_df, aes(x = Metric, y = Value, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = round(Value, 2)), 
            position = position_dodge(width = 0.6), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Midnight Blue", "Burlywood 3")) + # 빨강(CW), 파랑(GS)
  labs(title = "Grid Search vs Class Weight 성능 비교",
       y = "Score (0~1)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")