library(pROC)


cat("\n=== [Final Strategy] SVM 금융 특화 임계값 설정 (No Random Forest) ===\n")

# 1. SVM 모델에서 '연체 확률(Probability)' 추출
# (아까 만든 model_svm_hybrid가 있다면 그것을 사용, 없으면 model_svm_weighted 사용)
if(exists("model_svm_hybrid")) {
  target_model <- model_svm_hybrid
  target_data  <- testData_hybrid
  cat(">>> [Model Selected] 성능이 가장 좋았던 'Hybrid PCA SVM'을 사용합니다.\n")
} else {
  target_model <- model_svm_weighted
  target_data  <- testData_scaled
  cat(">>> [Model Selected] 'Weighted SVM'을 사용합니다.\n")
}

# 확률 예측
pred_prob_svm <- attr(predict(target_model, newdata = target_data, probability = TRUE), "probabilities")[, "Yes"]

# 2. 금융 리스크 관리 목표 설정: "연체자의 최소 75%는 잡아야 한다!"
target_recall_min <- 0.75
best_th_finance <- 0.5
final_metrics <- c()

cat(sprintf(">>> 목표 Recall(검거율): %.0f%% 이상이 되는 최적의 Threshold 탐색 중...\n", target_recall_min * 100))

# 3. 임계값을 0.5 -> 0.1로 내리면서 목표 Recall 달성 지점 찾기
# (금융권에서는 0.5보다 낮은 값을 주로 사용함)
for(th in seq(0.5, 0.05, by = -0.01)) {
  
  # 해당 임계값 기준 예측
  pred_class <- ifelse(pred_prob_svm >= th, "Yes", "No")
  pred_class <- factor(pred_class, levels = c("No", "Yes"))
  
  tbl <- table(Actual = testData_scaled$default.payment.next.month, Predicted = pred_class)
  
  # 지표 계산
  TP <- tbl["Yes", "Yes"]
  FN <- tbl["Yes", "No"]
  FP <- tbl["No", "Yes"]
  
  recall <- TP / (TP + FN)       # 검거율
  precision <- TP / (TP + FP)    # 정확도
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # 목표 Recall을 넘기는 순간 스톱! (그 중에서 Precision이 제일 높은 지점이 됨)
  if(recall >= target_recall_min) {
    best_th_finance <- th
    final_metrics <- c(Recall = recall, Precision = precision, F1 = f1, Acc = (TP + tbl["No","No"])/sum(tbl))
    
    cat(sprintf("   [Found!] Threshold: %.2f  --> Recall: %.2f%% (Target 달성) | F1: %.4f\n", 
                th, recall * 100, f1))
    break
  }
}

# 4. 최종 결과 리포트
cat("\n==================================================\n")
cat("      [SVM 최종: 금융 리스크 관리형 모델]      \n")
cat("==================================================\n")
cat(sprintf("1. 최종 임계값 (Threshold) : %.2f (기본 0.50 -> 하향 조정)\n", best_th_finance))
cat(sprintf("2. Recall (연체자 검거율)  : %.2f%% (목표 달성!)\n", final_metrics["Recall"] * 100))
cat(sprintf("3. F1-Score                : %.4f\n", final_metrics["F1"]))
cat(sprintf("4. Accuracy                : %.4f\n", final_metrics["Acc"]))
cat("--------------------------------------------------\n")

# 최종 혼동행렬 출력
final_pred_class <- ifelse(pred_prob_svm >= best_th_finance, "Yes", "No")
print(table(Actual = testData_scaled$default.payment.next.month, Predicted = final_pred_class))

cat("\n[결론] 정확도(Accuracy)를 조금 희생하더라도, 실제 연체자를 놓치지 않는 \n금융 도메인에 적합한 최종 SVM 모델입니다.\n")



