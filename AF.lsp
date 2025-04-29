; --- FilletSelLoop (FSL) - v2 ---
; 修正 ssget 引數問題，將提示訊息分離
; 允許使用者框選兩個物件或分步選擇，使用目前的設定自動執行圓角指令
; 完成一次後自動重複，直到使用者按 Esc 取消選取

(defun c:AF (/ ss ent1 ent2 count temp_ss) ; 定義命令 FSL，宣告區域變數
  (princ "\n框選自動折線工具，目前以最長兩條邊去做折線")
  (princ "\n提示：您可以一次框選兩個物件，或分步選擇。按 Esc 可結束指令。")
  
  (setq ent1 nil)
  (setq ent2 nil)
  
  ;; --- 開始迴圈 ---
  (while (or (null ent1) (null ent2))
    (if (null ent1)
      (progn
        (prompt "\n請選擇物件 (可一次選擇兩個，或分步選擇) (按 Esc 結束): ")
        (if (setq temp_ss (ssget ":S" '((0 . "*"))))
          (cond 
            ((= (sslength temp_ss) 1) ; 如果只選了一個
             (setq ent1 (ssname temp_ss 0))
            )
            ((= (sslength temp_ss) 2) ; 如果選了兩個
             (setq ent1 (ssname temp_ss 0))
             (setq ent2 (ssname temp_ss 1))
            )
            (t ; 如果選擇數量不對
             (princ "\n請選擇一個或兩個物件。")
            )
          )
          (return-from nil) ; 如果按了 Esc
        )
      )
      ;; 如果已有第一個物件但沒有第二個
      (if (null ent2)
        (progn
          (prompt "\n請選擇第二個物件 (按 Esc 結束): ")
          (if (setq temp_ss (ssget ":S" '((0 . "*"))))
            (if (= (sslength temp_ss) 1)
              (setq ent2 (ssname temp_ss 0))
              (princ "\n請只選擇一個物件。")
            )
            (return-from nil)
          )
        )
      )
    )
    
    ;; 如果兩個物件都選好了，執行圓角
    (if (and ent1 ent2)
      (progn
        (princ "\n -> 正在執行圓角...")
        (command "_.FILLET" ent1 ent2)
        (princ " 完成。")
        ;; 重置物件選擇，準備下一輪
        (setq ent1 nil)
        (setq ent2 nil)
      )
    )
  )
  
  (princ "\n指令結束。(Command ended.)") ; 迴圈結束後提示
  (princ) ; 清理命令列
)

; --- 提示使用者指令已載入 ---
(princ "\nFilletSelLoop (指令: FSL) v2 已載入。")
(princ)