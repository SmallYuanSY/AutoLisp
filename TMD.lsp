(defun c:TXT2MID2 ( / ent1 ent2 pt1 pt2 mid1 mid2 vec )

  (vl-load-com)

  ;; 取得插入點
  (defun get-inspt (ename)
    (cdr (assoc 10 (entget ename)))
  )

  ;; 移動文字插入點
  (defun move-to (ename target)
    (let ((inspt (get-inspt ename)))
      (if (and inspt target)
        (progn
          (apply 'command
            (append (list "_.MOVE" ename "" "_non") inspt '("_non") target))
        )
      )
    )
  )

  ;; 安全選取文字
  (defun get-text (msg)
    (let (ent)
      (while (not ent)
        (setq ent (car (entsel msg)))
        (if (not (and ent (member (cdr (assoc 0 (entget ent))) '("TEXT" "MTEXT"))))
          (progn
            (prompt "\n請選擇文字物件（TEXT/MTEXT）！")
            (setq ent nil)
          )
        )
      )
      ent
    )
  )

  ;; 主流程
  (setq ent1 (get-text "\n請選擇第一個文字: "))
  (setq ent2 (get-text "\n請選擇第二個文字: "))
  (setq pt1  (getpoint "\n請選擇第一個點: "))
  (setq pt2  (getpoint "\n請選擇第二個點: "))

  (if (and pt1 pt2)
    (progn
      (setq vec  (mapcar '(lambda (a b) (/ (- b a) 3.0)) pt1 pt2))
      (setq mid1 (mapcar '+ pt1 vec))            ; 1/3
      (setq mid2 (mapcar '- pt2 vec))            ; 2/3

      (move-to ent1 mid1)
      (move-to ent2 mid2)
    )
  )

  (princ)
)
