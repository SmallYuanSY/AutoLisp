(defun c:STMC ( / ss i ent entData oldJust oldInsPt newInsPt)
  (setq ss (ssget '((0 . "TEXT,MTEXT")))) ; 選取所有文字類型物件
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        
        (cond
          ;; 處理單行文字
          ((= (cdr (assoc 0 entData)) "TEXT")
           (setq oldInsPt (cdr (assoc 10 entData)))
           (setq oldJust (cdr (assoc 72 entData)))
           ;; 設定對正方式為中中 (72=1 中間, 73=2 中)
           (setq entData (subst (cons 72 1) (assoc 72 entData) entData))
           (setq entData (subst (cons 73 2) (assoc 73 entData) entData))
           ;; 設定對齊點為原位置
           (setq entData (subst (cons 11 oldInsPt) (assoc 11 entData) entData))
           (entmod entData)
           (entupd ent)
          )

          ;; 處理多行文字
          ((= (cdr (assoc 0 entData)) "MTEXT")
           (setq oldInsPt (cdr (assoc 10 entData)))
           ;; 轉成中中對齊：AttachmentPoint 5 = Middle Center
           (setq entData (subst (cons 71 1) (assoc 71 entData) entData)) ; Left to Right
           (setq entData (subst (cons 72 5) (assoc 72 entData) entData)) ; Middle Center
           ;; 將插入點設為原位置
           (setq entData (subst (cons 10 oldInsPt) (assoc 10 entData) entData))
           (entmod entData)
           (entupd ent)
          )
        )
        (setq i (1+ i))
      )
      (princ "\n所有文字已設為正中對齊，並保持原位置。")
    )
    (princ "\n未選取任何文字。")
  )
  (princ)
)
