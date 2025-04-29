(defun c:MM () ; Move Multiple in Place
  (prompt "\n選擇要移動的物件（依順序）:")
  (setq ss (ssget))
  (if ss
    (progn
      (setq count 0)
      (setq total (sslength ss))
      (while (< count total)
        (setq ent (ssname ss count))
        (setq base (cdr (assoc 10 (entget ent)))) ; 取得插入點/基點
        (prompt (strcat "\n指定物件 #" (itoa (+ count 1)) " 的目標位置: "))
        (setq pt (getpoint))
        (command "_.MOVE" ent "" base pt)
        (setq count (1+ count))
      )
    )
    (prompt "\n沒有選取任何物件")
  )
  (princ)
)
