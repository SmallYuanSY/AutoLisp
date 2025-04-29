
(defun c:TMUTI ( / ss idx ent edata val num newval multiplier)
  (vl-load-com)
  (prompt "\n請輸入要乘的倍率: ")
  (initget 7)
  (setq multiplier (getreal "\n倍率: "))

  (prompt "\n請選取要處理的文字（支援 TEXT 與 MTEXT）...")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx))
        (setq edata (entget ent))
        (setq val (cdr (assoc 1 edata)))
        (setq num (distof val))
        (if num
          (progn
            (setq newval (rtos (* num multiplier) 2 4))
            (setq edata (subst (cons 1 newval) (assoc 1 edata) edata))
            (entmod edata)
            (entupd ent)
          )
        )
        (setq idx (1+ idx))
      )
      (princ "\n文字已更新。")
    )
    (prompt "\n未選取任何文字。")
  )
  (princ)
)
