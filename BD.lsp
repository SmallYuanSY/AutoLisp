(defun c:BD ( / txtList txtEnt pt1 pt2 center count total done txtData ss i txtDataList sortedList pt)

  ;; 清理殘留變數
  (setq pb nil)

  (princ "\n請框選要放進格子的文字（僅限 TEXT）...")
  (setq ss (ssget '((0 . "TEXT"))))
  (if (not ss)
    (progn (princ "\n未選取任何文字。") (exit))
  )

  ;; 組合資料 ((ent pt) ...)
  (setq txtDataList '())
  (setq i 0)
  (repeat (sslength ss)
    (setq txtEnt (ssname ss i))
    (setq txtData (entget txtEnt))
    (setq pt (cdr (assoc 10 txtData)))
    (if pt
      (setq txtDataList (cons (list txtEnt pt) txtDataList))
    )
    (setq i (1+ i))
  )

  ;; 按 Y 座標由上到下排序（Y 越大越上）
  (setq sortedList
    (vl-sort txtDataList
      (function
        (lambda (a b)
          (> (cadr (cadr a)) (cadr (cadr b))) ; Y比較
        )
      )
    )
  )

  (setq txtList (mapcar 'car sortedList))
  (setq total (length txtList))

  (princ (strcat "\n✅ 已選取並排序 " (itoa total) " 個文字（由上至下）。"))
  (princ "\n請依序點選每個格子的兩角（可鎖點），按 ESC 結束...")

  (setq done nil)
  (while (and txtList (not done))
    (setq pt1 (getpoint "\n第一點（格子對角）: "))
    (if (not pt1) (setq done T)
      (progn
        (setq pt2 (getpoint pt1 "\n第二點（對角點）: "))
        (if (not pt2) (setq done T)
          (progn
            (setq center (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))

            (setq txtEnt (car txtList))
            (setq txtList (cdr txtList))
            (setq txtData (entget txtEnt))

            ;; 對齊設定
            (setq txtData (subst (cons 72 1) (assoc 72 txtData) txtData)) ; 水平中
            (setq txtData (subst (cons 73 2) (assoc 73 txtData) txtData)) ; 垂直中
            (if (assoc 11 txtData)
              (setq txtData (subst (cons 11 center) (assoc 11 txtData) txtData))
              (setq txtData (append txtData (list (cons 11 center))))
            )
            (setq txtData (subst (cons 10 center) (assoc 10 txtData) txtData))

            (entmod txtData)
            (entupd txtEnt)

            (princ (strcat "\n✔ 放置 1 個文字，剩餘: " (itoa (length txtList))))
          )
        )
      )
    )
  )

  (princ "\n🎉 結束：所有文字已置中或你中途結束。")
  (princ)
)
