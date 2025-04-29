(defun c:SUMFFR ( / ss i ent entType blklist txtlist blkpt txtpt blkname val pair grouped insertPt name count namx namy blksizeMap)

  (vl-load-com)

  ;; 判斷是否接近距離
  (defun near-equal (a b tol) (<= (abs (- a b)) tol))

  ;; 建立文字
  (defun draw-text (pt str)
    (entmakex
      (list
        (cons 0 "TEXT")
        (cons 10 pt)
        (cons 40 30)
        (cons 1 str)
        (cons 7 (getvar "TEXTSTYLE"))
        (cons 8 (getvar "CLAYER"))
      )
    )
  )

  ;; 取得選取集
  (setq ss (ssget '((0 . "INSERT,TEXT"))))
  (if (not ss)
    (progn (prompt "\n請選擇物件。") (exit))
  )

  (setq blklist '() txtlist '() pair '() blksizeMap '())

  ;; 拆分圖塊與文字 + 儲存圖塊比例
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq entType (cdr (assoc 0 (entget ent))))
    (cond
      ((= entType "INSERT")
       (setq blklist (cons ent blklist))
       (setq blkdata (entget ent))
       (setq blkname (cdr (assoc 2 blkdata)))
       (setq sx (cdr (assoc 41 blkdata)))
       (setq sy (cdr (assoc 42 blkdata)))
       (if (not (assoc blkname blksizeMap))
         (setq blksizeMap (cons (list blkname sx sy) blksizeMap)))
      )
      ((= entType "TEXT") (setq txtlist (cons ent txtlist)))
    )
    (setq i (1+ i))
  )

  ;; 建立圖塊 + 數值配對
  (foreach blk blklist
    (setq blkpt (cdr (assoc 10 (entget blk))))
    (setq blkname (cdr (assoc 2 (entget blk))))
    (foreach txt txtlist
      (setq txtpt (cdr (assoc 10 (entget txt))))
      (if (and
            (near-equal (cadr blkpt) (cadr txtpt) 20)
            (>= (- (car txtpt) (car blkpt)) 0)
            (<= (- (car txtpt) (car blkpt)) 300)
          )
        (progn
          (setq val (atoi (cdr (assoc 1 (entget txt)))))
          (setq pair (cons (cons blkname val) pair))
        )
      )
    )
  )

  ;; 加總
  (setq grouped '())
  (foreach p pair
    (if (assoc (car p) grouped)
      (setq grouped (subst (cons (car p) (+ (cdr p) (cdr (assoc (car p) grouped))))
                           (assoc (car p) grouped)
                           grouped))
      (setq grouped (cons p grouped))
    )
  )

  ;; 讓使用者選擇插入位置
  (prompt "\n請選擇總表插入點：")
  (setq insertPt (getpoint))

  ;; 開始輸出圖塊與數字（不含框）
  (setq i 0)
  (foreach g grouped
    (setq name (car g))
    (setq count (cdr g))

    (setq sizeinfo (assoc name blksizeMap))
    (setq namx (if sizeinfo (cadr sizeinfo) 1.0))
    (setq namy (if sizeinfo (caddr sizeinfo) 1.0))

    (setq x (car insertPt))
    (setq y (- (cadr insertPt) (* i 100)))

    ;; 插入圖塊
    (command "_.INSERT" name (list x y) namx namy "")

    ;; 插入數字在右邊 100 位置
    (draw-text (list (+ x 100) (- y 10)) (itoa count))

    (setq i (1+ i))
  )

  (princ)
) 
