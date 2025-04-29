(defun c:IN2MM ( / ss i ent txt newtxt map)
  (setq map '(("2\"" . "50A") 
              ("3\"" . "75A")
              ("4\"" . "100A")
              ("5\"" . "125A")
              ("6\"" . "150A")
              ("8\"" . "200A")))

  (prompt "\n請選取要轉換的文字物件：")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq txt (cdr (assoc 1 (entget ent))))
        
        ;; 逐一比對英吋→mm對照
        (foreach pair map
          (if (wcmatch txt (strcat "*" (car pair) "*"))
            (progn
              (setq newtxt (vl-string-subst (cdr pair) (car pair) txt))
              (entmod (subst (cons 1 newtxt) (assoc 1 (entget ent)) (entget ent)))
              (entupd ent)
            )
          )
        )
        (setq i (1+ i))
      )
      (princ "\n轉換完成。")
    )
    (prompt "\n沒有選取任何文字。")
  )
  (princ)
)
