(defun c:CR ( / pt1 pt2 width height num isHorizontal spacing i startPt endPt x y)
  (setq pt1 (getpoint "\n請選擇矩形對角點1: "))
  (setq pt2 (getpoint "\n請選擇矩形對角點2: "))

  ;; 計算寬高
  (setq width (abs (- (car pt1) (car pt2))))
  (setq height (abs (- (cadr pt1) (cadr pt2))))

  ;; 判斷長邊方向
  (setq isHorizontal (> width height))

  ;; 輸入要畫幾條線
  (setq num (getint "\n請輸入要畫幾條線（不含邊界）："))

  (if (> num 0)
    (progn
      ;; 分 num+1 段 → 每段間距
      (setq spacing (/ (if isHorizontal width height) (+ num 1)))
      (setq i 1)
      (while (<= i num)
        (if isHorizontal
          (progn
            (setq x (+ (min (car pt1) (car pt2)) (* spacing i)))
            (setq startPt (list x (min (cadr pt1) (cadr pt2))))
            (setq endPt   (list x (max (cadr pt1) (cadr pt2))))
          )
          (progn
            (setq y (+ (min (cadr pt1) (cadr pt2)) (* spacing i)))
            (setq startPt (list (min (car pt1) (car pt2)) y))
            (setq endPt   (list (max (car pt1) (car pt2)) y))
          )
        )
        (entmakex (list '(0 . "LINE")
                        (cons 10 startPt)
                        (cons 11 endPt)))
        (setq i (1+ i))
      )
    )
    (princ "\n線條數需大於 0")
  )
  (princ)
)
