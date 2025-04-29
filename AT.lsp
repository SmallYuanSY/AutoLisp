(defun get-sort-order ( / primary dirX dirY )
  (princ "\n請選擇排序邏輯：")
  (initget "H V")
  (setq primary (getkword "\n主要排序方向 [水平(H)/垂直(V)] <H>: "))
  (if (null primary) (setq primary "H"))

  (initget "L2R R2L")
  (setq dirX (getkword "\n水平方向 [左→右(L2R)/右→左(R2L)] <L2R>: "))
  (if (null dirX) (setq dirX "L2R"))

  (initget "T2B B2T")
  (setq dirY (getkword "\n垂直方向 [上→下(T2B)/下→上(B2T)] <T2B>: "))
  (if (null dirY) (setq dirY "T2B"))

  (list primary dirX dirY)
)

(defun sort-entities (entWithPos sortOrder / primary dirX dirY)
  (setq primary (car sortOrder))
  (setq dirX (cadr sortOrder))
  (setq dirY (caddr sortOrder))

  ;; entWithPos = ((x y ent) (x y ent) ...)
  (vl-sort entWithPos
    (function
      (lambda (a b)
        (setq xa (car a) ya (cadr a))
        (setq xb (car b) yb (cadr b))
        (cond
          ((= primary "H")
           (if (< (abs (- ya yb)) 1.0)
             (cond ((= dirX "L2R") (< xa xb)) ((= dirX "R2L") (> xa xb)) (t nil))
             (cond ((= dirY "T2B") (> ya yb)) ((= dirY "B2T") (< ya yb)) (t nil))
           )
          )
          ((= primary "V")
           (if (< (abs (- xa xb)) 1.0)
             (cond ((= dirY "T2B") (> ya yb)) ((= dirY "B2T") (< ya yb)) (t nil))
             (cond ((= dirX "L2R") (< xa xb)) ((= dirX "R2L") (> xa xb)) (t nil))
           )
          )
          (t nil)
        )
      )
    )
  )
)

(defun arrange-text ( / ss sortOrder entWithPos sortedEnts startPt endPt direction lastPos newPos)
  (setq sortOrder (get-sort-order))
  (setq ss (ssget '((0 . "TEXT"))))
  (if ss
    (progn
      ;; 將選取到的圖元轉成 ((x y ent) ...) 格式
      (setq entWithPos '())
      (repeat (sslength ss)
        (setq ent (ssname ss 0))
        (setq entData (entget ent))
        (setq pos (cdr (assoc 10 entData)))
        (setq entWithPos (cons (list (car pos) (cadr pos) ent) entWithPos))
        (ssdel ent ss) ; 刪掉處理過的項目
      )

      ;; 排序
      (setq sortedEnts (sort-entities entWithPos sortOrder))

      ;; 點選兩點決定起點與方向
      (setq startPt (getpoint "\n指定第一個文字插入點: "))
      (setq endPt   (getpoint "\n指定第二個點來決定排列方向: "))
      (setq direction (mapcar '- endPt startPt))
      (setq spacing 1.0)
      (setq lastPos startPt)

      ;; 開始移動每個文字
      (foreach item sortedEnts
        (setq ent (nth 2 item)) ; 取得原圖元名稱
        (setq newPos lastPos)
        (command "move" ent "" "_non" (cdr (assoc 10 (entget ent))) "_non" newPos)
        (setq lastPos (mapcar '+ lastPos (mapcar '(lambda (x) (* x spacing)) direction)))
      )
    )
    (princ "\n沒有選擇任何文字物件。")
  )
)

(defun c:AT ()
  (arrange-text)
)
