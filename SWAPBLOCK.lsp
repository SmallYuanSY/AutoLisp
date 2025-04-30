(defun c:SWB ( / blkA blkB pt1 pt2 ss i ename data name blkBname insPt rot scaleX scaleY scaleZ)
  (vl-load-com)

  (princ "\n請選擇來源圖塊（要被換掉的圖塊）:")
  (setq blkA (car (entsel)))
  (if (not blkA)
    (progn (princ "\n沒有選取來源圖塊，指令結束。") (exit))
  )

  (princ "\n請選擇目標圖塊（要換成的圖塊）:")
  (setq blkB (car (entsel)))
  (if (not blkB)
    (progn (princ "\n沒有選取目標圖塊，指令結束。") (exit))
  )

  ;; 取得目標圖塊名稱與比例
  (setq blkBname (cdr (assoc 2 (entget blkB))))
  (setq scaleX (cdr (assoc 41 (entget blkB))))
  (setq scaleY (cdr (assoc 42 (entget blkB))))
  (setq scaleZ (cdr (assoc 43 (entget blkB))))
  (setq rot (cdr (assoc 50 (entget blkB)))) 
  (setq rot-deg (* rot (/ 180.0 pi)))

  ;; 框選範圍
  (setq pt1 (getpoint "\n請指定框選範圍的第一個點（左上角）: "))
  (setq pt2 (getcorner pt1 "\n請指定框選範圍的第二個點（右下角）: "))
  (setq ss (ssget "W" pt1 pt2 '((0 . "INSERT"))))

  (if (not ss)
    (progn (princ "\n範圍內沒有找到任何圖塊，指令結束。") (exit))
  )

  (setq name (cdr (assoc 2 (entget blkA)))) ;; 來源圖塊名稱

  ;; 逐一處理選到的物件
  (repeat (setq i (sslength ss))
    (setq ename (ssname ss (setq i (1- i))))
    (setq data (entget ename))
    (if (= (cdr (assoc 2 data)) name)
      (vl-catch-all-apply
        (function
          (lambda ()
            ;; 取得原本圖塊的位置與旋轉角
            (setq insPt (cdr (assoc 10 data)))

            ;; 刪除舊的圖塊
            (entdel ename)

            ;; 插入新的圖塊（用選到B圖塊的比例）
            (command "_.-INSERT" blkBname insPt scaleX scaleY rot-deg)
          )
        )
      )
    )
  )

  (princ "\n替換完成。")
  (princ)
)
