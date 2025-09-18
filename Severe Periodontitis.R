
# --- Objective 2: Severe Periodontitis ---

cat("\n--- Objective 2: Analysis for Severe Periodontitis ---\n\n")

# 2.1 Prevalence Comparison (Severe Periodontitis)
cat("2.1 Prevalence of Severe Periodontitis:\n")
prev_cdc_severe <- prop.table(table(periodontal_data$CDC_AAP_Severe))["1"] * 100
prev_efp_severe <- prop.table(table(periodontal_data$EFP_AAP_Severe))["1"] * 100
cat(paste0("  Prevalence by 2012 CDC/AAP Severe: ", round(prev_cdc_severe, 2), "%\n"))
cat(paste0("  Prevalence by 2018 EFP/AAP Stage III-IV: ", round(prev_efp_severe, 2), "%\n"))

# Chi-square test for prevalence comparison
chi_sq_severe <- chisq.test(table(periodontal_data$CDC_AAP_Severe, periodontal_data$EFP_AAP_Severe))
cat("\n  Chi-square test for prevalence difference:\n")
print(chi_sq_severe)

# 2.2 Diagnostic Accuracy (EFP/AAP Severe vs. CDC/AAP Severe as reference)
cat("\n2.2 Diagnostic Accuracy for Severe Periodontitis (CDC/AAP as Reference):\n")

# Confusion Matrix
cm_severe <- confusionMatrix(data = periodontal_data$EFP_AAP_Severe,
                             reference = periodontal_data$CDC_AAP_Severe,
                             positive = "1")
print(cm_severe)

# Extract Sensitivity and Specificity
sensitivity_severe <- cm_severe$byClass["Sensitivity"]
specificity_severe <- cm_severe$byClass["Specificity"]
ppv_severe <- cm_severe$byClass["Pos Pred Value"]
npv_severe <- cm_severe$byClass["Neg Pred Value"]

cat(paste0("  Sensitivity (SS): ", round(sensitivity_severe, 3), "\n"))
cat(paste0("  Specificity (SP): ", round(specificity_severe, 3), "\n"))
cat(paste0("  Positive Predictive Value (PPV): ", round(ppv_severe, 3), "\n"))
cat(paste0("  Negative Predictive Value (NPV): ", round(npv_severe, 3), "\n"))

# ROC Curve and AUC
roc_curve_severe <- roc(response = periodontal_data$CDC_AAP_Severe,
                        predictor = as.numeric(as.character(periodontal_data$EFP_AAP_Severe)),
                        levels = c("0", "1"),
                        direction = ">")

auc_severe <- auc(roc_curve_severe)
cat(paste0("  Area Under the ROC Curve (AUC): ", round(auc_severe, 3), " (95% CI: ",
           round(ci.auc(roc_curve_severe), 3), "-", round(ci.auc(roc_curve_severe), 3), ")\n"))

# Plot ROC curve
plot(roc_curve_severe, main = "ROC Curve: EFP/AAP Stage III-IV vs. CDC/AAP Severe",
     col = "#d95f02", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = paste("AUC =", round(auc_severe, 3)),
       col = "#d95f02", lwd = 2, bty = "n")
