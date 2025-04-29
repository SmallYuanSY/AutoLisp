(defun c:COUNTSTR ( / ent edata str rec pt x y )
  (vl-load-com)

  ;; 初始化記錄表（如果第一次使用）
  (if (not *text-count-table*) (setq *text-count-table* (list)))

  ;; 進入連續點選模式
  (prompt "\n請點選要統計的文字（Enter 結束）：")
  (while (setq ent (car (entsel)))
    (setq edata (entget ent))
    (if (member (cdr (assoc 0 edata)) '("TEXT" "MTEXT"))
      (progn
        (setq str (cdr (assoc 1 edata))) ; 取得文字內容
        (redraw ent 2) ; 隱藏文字

        ;; 更新記錄表
        (setq rec (assoc str *text-count-table*))
        (if rec
          (setq *text-count-table*
                (subst (cons str (1+ (cdr rec))) rec *text-count-table*))
          (setq *text-count-table* (cons (cons str 1) *text-count-table*))
        )
      )
    )
  )

  ;; 重新顯示統計結果列表（畫在固定位置）
  (setq pt (getpoint "\n請點選統計顯示位置："))
  (setq x (car pt) y (cadr pt))

  (foreach item *text-count-table*
    (entmakex
      (list '(0 . "TEXT")
            (cons 10 (list x y 0))
            (cons 40 100.0)
            (cons 1 (strcat (car item) " : " (itoa (cdr item))))
            (cons 7 "Standard")
            (cons 72 0)
            (cons 73 0)
            (cons 11 (list x y 0))
      )
    )
    (setq y (- y 120)) ; 下一列往下移
  )

  (princ)
)
