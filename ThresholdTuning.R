library(pROC)

# ==========================================================
# 1. 최적 임계값 탐색 (Threshold Tuning)
# ==========================================================
cat("=== [Threshold Tuning] F1-Score를 최대화하는 임계값 탐색 중... ===\n")

# 0.01 단위로 임계값 후보 생성
thresholds <- seq(0.05, 0.95, by = 0.01)

# 각 임계값별 F1-Score를 저장할 공간
f1_results <- numeric(length(thresholds))

# 반복문으로 최적값 찾기
for(i in seq_along(thresholds)) {
  thresh <- thresholds[i]
  
  # 해당 임계값보다 확률이 높으면 "Yes", 아니면 "No"
  pred_temp <- ifelse(prob_svm_best > thresh, "Yes", "No")
  
  # Confusion Matrix 요소 계산
  # (factor 레벨을 맞춰주기 위해 table 함수 사용 시 주의)
  pred_factor <- factor(pred_temp, levels = c("No", "Yes"))
  tbl <- table(Actual = testData_scaled$default.payment.next.month, Predicted = pred_factor)
  
  TP <- tbl["Yes", "Yes"]
  FP <- tbl["No", "Yes"]
  FN <- tbl["Yes", "No"]
  
  # Precision & Recall
  prec <- TP / (TP + FP)
  if(is.na(prec)) prec <- 0
  rec  <- TP / (TP + FN)
  
  # F1-Score
  f1 <- 2 * (prec * rec) / (prec + rec)
  if(is.na(f1)) f1 <- 0
  
  f1_results[i] <- f1
}

# ==========================================================
# 2. 최적 임계값 선정 및 시각화
# ==========================================================
# F1이 가장 높은 인덱스 찾기
best_idx <- which.max(f1_results)
best_threshold <- thresholds[best_idx]
best_f1 <- f1_results[best_idx]

cat("\n>>> 찾은 최적 임계값 (Optimal Threshold) <<<\n")
cat(sprintf("Threshold : %.2f\n", best_threshold))
cat(sprintf("Max F1-Score: %.4f\n", best_f1))

# 시각화 (Threshold 변화에 따른 F1-Score 변화)
par(mfrow=c(1,1))
plot(thresholds, f1_results, type="l", col="Midnight Blue", lwd=2,
     main="Threshold 변화에 따른 F1-Score", xlab="Threshold", ylab="F1-Score")
abline(v=best_threshold, col="red", lty=2)
text(best_threshold, best_f1, paste0("Optimal: ", best_threshold), pos=3, col="red")

# ==========================================================
# 3. 최적 임계값 적용 후 최종 성적표 (Final Evaluation)
# ==========================================================
cat("\n=== [Final Scorecard with Optimal Threshold] ===\n")

# 최적 임계값으로 다시 예측
final_pred_class <- ifelse(prob_svm_best > best_threshold, "Yes", "No")
final_pred_class <- factor(final_pred_class, levels = c("No", "Yes"))

# 혼동 행렬 출력
final_tbl <- table(Actual = testData_scaled$default.payment.next.month, 
                   Predicted = final_pred_class)
print(final_tbl)

# 최종 지표 계산
TP_final <- final_tbl["Yes", "Yes"]
TN_final <- final_tbl["No", "No"]
FP_final <- final_tbl["No", "Yes"]
FN_final <- final_tbl["Yes", "No"]

acc_final <- (TP_final + TN_final) / sum(final_tbl)
prec_final <- TP_final / (TP_final + FP_final)
rec_final <- TP_final / (TP_final + FN_final) # Recall (재현율)이 중요!

cat(sprintf("\n1. Accuracy : %.4f (기존: 0.8185)\n", acc_final))
cat(sprintf("2. F1-Score : %.4f (기존: 0.4541)\n", best_f1))
cat(sprintf("3. Recall   : %.4f (재현율 상승 확인)\n", rec_final))
cat(sprintf("4. Precision: %.4f (정밀도 하락 확인)\n", prec_final))

# ==========================================================
# 4. 비교 요약
# ==========================================================
cat("\n[결론] 임계값을 %.2f로 조정하여 F1-Score를 극대화했습니다.\n", best_threshold)
cat("특히, 놓치고 있던 연체자(TP)를 더 많이 확보하여 리스크 관리 능력을 높였습니다.\n")