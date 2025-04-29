(setq *OFF-last* "Polyline") ; 初始預設為 Polyline

(defun c:OFF ( / opt )
  ;; 顯示選項：Polyline or Line
  (initget "Polyline Line")
  (setq opt (getkword (strcat "\n請選擇處理方式 [聚合線Polyline(P)/線段Line(L)] <" *OFF-last* ">: ")))
  (if (or (null opt) (= opt "")) (setq opt *OFF-last*)) ; 沒輸入就用上次
  (setq *OFF-last* opt) ; 記住本次選擇

  ;; 呼叫對應功能
  (cond
    ((equal opt "Line") (c:OFFA))
    ((equal opt "Polyline") (c:OFFT))
    (t (princ "\n⚠️ 無效的選項。"))
  )

  (princ)
)




(defun c:OFFA ( / entList ent entData pt1 pt2 width halfW dx dy len norm perp ptA ptB ptC ptD plData i explodedEnts)

  ;; 选择多个实体
  (setq entList (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if entList
    (progn
      (setq width (getreal "\n輸入總寬度："))
      (setq halfW (/ width 2.0))

      ;; 遍历每个选定的实体
      (setq i 0)
      (repeat (sslength entList)
        (setq ent (ssname entList i))  
        (setq entData (entget ent))

        ;; 支援 LINE 或 LWPOLYLINE
        (cond
          ((= (cdr (assoc 0 entData)) "LINE")
           (process-line ent width))  ;; 处理单条直线
          ((= (cdr (assoc 0 entData)) "LWPOLYLINE")
           (command "._explode" ent)  ;; 炸开 POLYLINE
           (setq explodedEnts (ssget "P"))  ;; 获取刚才炸开的实体
           (if explodedEnts
             (progn
               (setq j 0)
               (repeat (sslength explodedEnts)
                 (process-line (ssname explodedEnts j) width)
                 (setq j (1+ j))
               )
             )
           ))
          (t (prompt "\n⚠️ 不支援的物件類型。") (exit))
        )
        
        (setq i (1+ i))  
      )
      (princ (strcat "\n✅ 成功建立封閉區域並刪除原始線段。"))
    )
    (prompt "\n⚠️ 沒有選取任何物件。")
  )
  (princ)
)

;; 处理单条直线的函数
(defun process-line (ent width / entData pt1 pt2 dx dy len norm perp ptA ptB ptC ptD plData halfW)
  (setq entData (entget ent))
  (setq halfW (/ width 2.0))
  
  (setq pt1 (cdr (assoc 10 entData)))
  (setq pt2 (cdr (assoc 11 entData)))
  
  ;; 计算方向与垂直向量
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq len (sqrt (+ (* dx dx) (* dy dy))))
  (setq norm (list (/ dx len) (/ dy len)))
  (setq perp (list (- (cadr norm)) (car norm)))

  ;; 四个角点
  (setq ptA (to-3d (mapcar '+ pt1 (mapcar '(lambda (x) (* x halfW)) perp))))
  (setq ptB (to-3d (mapcar '+ pt2 (mapcar '(lambda (x) (* x halfW)) perp))))
  (setq ptC (to-3d (mapcar '- pt2 (mapcar '(lambda (x) (* x halfW)) perp))))
  (setq ptD (to-3d (mapcar '- pt1 (mapcar '(lambda (x) (* x halfW)) perp))))

  ;; 建立封闭 polyline
  (setq plData (list
    '(0 . "LWPOLYLINE")
    '(100 . "AcDbEntity")
    '(100 . "AcDbPolyline")
    (cons 90 4)
    (cons 70 1)
    (cons 10 ptA)
    (cons 10 ptB)
    (cons 10 ptC)
    (cons 10 ptD)
  ))
  (entmakex plData)
  
  ;; 删除原始实体
  (entdel ent)
)

(defun number-sequence (start end)
  (if (> start end)
    nil
    (cons start (number-sequence (+ start 1) end))
  )
)

(defun to-3d (pt)
  (if (= (length pt) 2)
    (append pt (list 0.0))
    pt
  )
)

(defun draw-line (pt1 pt2)
  (entmakex (list '(0 . "LINE")
                  (cons 10 (to-3d pt1))
                  (cons 11 (to-3d pt2))
                  '(62 . 256))) ; ByLayer
)

(defun get-pt-list (coords)
  ;; 把 1D coords 座標轉成 ((x y) (x y) ...) 點串
  (if coords
    (mapcar
      '(lambda (i)
         (list (nth (* 2 i) coords)
               (nth (+ 1 (* 2 i)) coords)))
      (number-sequence 0 (- (/ (length coords) 2) 1))
    )
  )
)

(defun offset-poly (entObj dist)
  ;; 回傳 offset 出來的 entname（穩定抓）
  (setq before (entlast))
  (vlax-invoke entObj 'Offset dist)
  (setq after (entlast))
  (if (/= before after) after nil)
)

(defun process-poly (ent width)
  (setq half (/ width 2.0))
  (setq entObj (vlax-ename->vla-object ent))

  ;; Offset 左右
  (setq A_ent (offset-poly entObj (- half)))
  (setq B_ent (offset-poly entObj half))

  (if (and A_ent B_ent)
    (progn
      (setq Aobj (vlax-ename->vla-object A_ent))
      (setq Bobj (vlax-ename->vla-object B_ent))

      (setq coordsA (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates Aobj))))
      (setq coordsB (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates Bobj))))
      (setq ptsA (get-pt-list coordsA))
      (setq ptsB (get-pt-list coordsB))

      ;; 封邊
      (setq line1 (draw-line (car ptsA)  (car ptsB)))
      (setq line2 (draw-line (last ptsA) (last ptsB)))

      ;; 刪除原始
      (entdel ent)

      (command "_.PEDIT" A_ent "_J" B_ent line1 line2 "" "")

      (setq joinedEnt (entlast))
      (setq currentLayer (getvar "CLAYER"))
      (setq joinedObj (vlax-ename->vla-object joinedEnt))
      (vlax-put joinedObj 'Layer currentLayer)
      (vlax-put joinedObj 'Linetype "ByLayer")
      (vlax-put joinedObj 'Color 256)
      (vlax-put-property (vlax-ename->vla-object joinedEnt) 'Layer currentLayer)
    )
    (prompt "\n❌ Offset 失敗，跳過")
  )
)

(defun c:OFFT ( / ss i ent width)
  (vl-load-com)
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (not ss)
    (progn (prompt "\n❌ 沒有選取任何 Polyline") (exit))
  )

  (setq width (getreal "\n請輸入總寬度："))
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (process-poly ent width)
    (setq i (1+ i))
  )

  (prompt "\n✅ OFFT 完成：全部封邊 + Join 完成")
  (princ)
)
