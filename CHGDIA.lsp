;============================================================
;  Auto  CHGALL  v2024-05-xx    (括號修正版)
;------------------------------------------------------------
;───────── ①  外/內徑資料 ─────────
(vl-load-com)
(setq circleTable
  '(
    ; 不鏽鋼管
    (13 1.588 1.428)
    (20 2.222 2.022)
    (25 2.858 2.658)
    (40 4.270 4.030)
    (50 4.860 4.620)
    (65 7.630 7.030)
    (80 8.910 8.310)
    (100 11.430 10.830)
    (125 13.980 13.300)
    (150 16.520 15.840)
  )
)

;───────── ②  PE 厚度表 ─────────
(setq peThicknessTable
  '(
    (13 0.8)
    (20 0.8)
    (25 0.8)
    (40 0.8)
    (50 0.8)
    (65 1.5)
    (80 1.5)
    (100 1.5)
    (125 1.5)
    (150 1.5)
  )
)

;───────── ③  U 型封邊資料 ───────
(setq udata
  '(
    (8 1.95 2.1 0.6) (10 2.35 2.5 0.6) (15 2.4 2.95 0.55)
    (20 2.7 3.4 0.4) (25 3.1 4.1 0.5)  (32 3.4 5.05 0.45)
    (40 4.2 5.7 0.5) (50 4.8 7 0.6)   (65 5.6 8.4 0.4)
    (80 7.3 10.3 1.1) (90 7.9 11.6 1.2) (100 9.1 12.9 1.1)
    (125 10.3 15.4 1) (150 11.5 18.2 1.2)
    (200 14.1 23.2 1) (250 17.8 28.9 1.5)
    (300 20.9 34.4 2) (350 22.2 37.7 1.5)
    (400 24.8 42.9 1.5) (450 27.4 48.1 1.5) (500 30.1 53.2 1.4)
  )
)

;; CHGPVC - 根據圈內的文字自動對應 PVC 管內外徑，並設定兩個圓的半徑
(defun c:CHGPVC ( / ss i ent obj textEnt textStr pvcSize outerDia innerDia circles radius1 radius2 obj1 obj2 )
  
  (setq ss (ssget '((0 . "CIRCLE,TEXT"))))

  (if (and ss (= (sslength ss) 3))
    (progn
      ;; 分類選到的物件：兩個圓 + 一個文字
      (setq circles '())
      (setq textEnt nil)
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (cond
          ((= (cdr (assoc 0 (entget ent))) "CIRCLE")
           (setq circles (cons (vlax-ename->vla-object ent) circles))
          )
          ((= (cdr (assoc 0 (entget ent))) "TEXT")
           (setq textEnt (vlax-ename->vla-object ent))
          )
        )
        (setq i (1+ i))
      )

      ;; 取得文字內容並轉成對應尺寸
      (if (and textEnt (= (vla-get-objectname textEnt) "AcDbText"))
        (progn
          (setq textStr (vla-get-textstring textEnt))
          (setq pvcSize (atoi textStr))

          ;; PVC 表查詢（cm）
          (setq pvcTable
            '(
              (13 1.588 1.43)
              (20 2.222 2.02)
              (25 2.858 2.66)
              (30 3.4 3.16)
              (40 4.27 4.03)
              (50 4.86 4.62)
              (60 6.05 5.75)
            )
          )


          ;; 查表找對應外徑與內徑
          (setq match (assoc pvcSize pvcTable))
          (if match
            (progn
              (setq outerDia (cadr match))
              (setq innerDia (caddr match))

              ;; 對兩個圓排序：大的當外圓，小的當內圓
              (setq obj1 (car circles))
              (setq obj2 (cadr circles))
              (setq radius1 (vla-get-radius obj1))
              (setq radius2 (vla-get-radius obj2))

              (if (> radius1 radius2)
                (progn
                  (vla-put-radius obj1 (/ outerDia 2.0))
                  (vla-put-radius obj2 (/ innerDia 2.0))
                )
                (progn
                  (vla-put-radius obj2 (/ outerDia 2.0))
                  (vla-put-radius obj1 (/ innerDia 2.0))
                )
              )
              (princ (strcat "\nPVC " (itoa pvcSize) " 設定完成，外徑: " (rtos outerDia 2 1) ", 內徑: " (rtos innerDia 2 1)))
            )
            (princ "\n⚠️ 查無對應 PVC 尺寸。")
          )
        )
        (princ "\n⚠️ 找不到有效文字，請確認有選取 PVC 數字。")
      )
    )
    (princ "\n⚠️ 請一次選取兩個圓與一個文字，共三個物件。")
  )
  (princ)
)

;; CHGU - 根據圈內文字設定 U 型封邊的直線長與寬度（不動圓弧）
(defun c:CHGU ( / ent obj coords ptlist i pt textEnt textStr tag udata match len height width
                   rightX topY botY dx dy newpts )
  (vl-load-com)
  (setq sel (ssget '((0 . "LWPOLYLINE,TEXT"))))
  (if (and sel (= (sslength sel) 2))
    (progn
      ;; 分類 Polyline 和文字
      (repeat 2
        (setq ent (ssname sel (if (boundp 'i) (setq i (1+ i)) (setq i 0))))
        (cond
          ((= (cdr (assoc 0 (entget ent))) "LWPOLYLINE") (setq obj (vlax-ename->vla-object ent)))
          ((= (cdr (assoc 0 (entget ent))) "TEXT") (setq textEnt (vlax-ename->vla-object ent)))
        )
      )

      (if (and obj textEnt)
        (progn
          (setq textStr (vla-get-textstring textEnt))
          (setq tag (atoi textStr))

          ;; 套管資料（含整體寬度）
          (setq udata
            '(
                (8 1.95 2.1 0.6)
                (10 2.35 2.5 0.6)
                (15 2.4 2.95 0.55)
                (20 2.7 3.4 0.4)
                (25 3.1 4.1 0.5)
                (32 3.4 5.05 0.45)
                (40 4.2 5.7 0.5)
                (50 4.8 7 0.6)
                (65 5.6 8.4 0.4)
                (80 7.3 10.3 1.1)
                (90 7.9 11.6 1.2)
                (100 9.1 12.9 1.1)
                (125 10.3 15.4 1)
                (150 11.5 18.2 1.2)
                (200 14.1 23.2 1)
                (250 17.8 28.9 1.5)
                (300 20.9 34.4 2)
                (350 22.2 37.7 1.5)
                (400 24.8 42.9 1.5)
                (450 27.4 48.1 1.5)
                (500 30.1 53.2 1.4)
            )
          )


          (setq match (assoc tag udata))
          (if match
            (progn
              (setq len (nth 1 match))
              (setq height (nth 2 match))
              (setq width (nth 3 match))

              ;; 調整頂點
              (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-coordinates obj))))
              (setq ptlist '() i 0)
              (while (< i (length coords))
                (setq pt (list (nth i coords) (nth (1+ i) coords)))
                (setq ptlist (cons pt ptlist))
                (setq i (+ i 2))
              )
              (setq ptlist (reverse ptlist))

              (setq rightX (apply 'max (mapcar 'car ptlist)))
              (setq topY (apply 'max (mapcar 'cadr ptlist)))
              (setq botY (apply 'min (mapcar 'cadr ptlist)))
              (setq dx (- len (- rightX (apply 'min (mapcar 'car ptlist)))))
              (setq dy (/ (- height (- topY botY)) 2.0))

              ;; 生成新頂點
              (setq newpts
                (mapcar
                  (function
                    (lambda (p)
                      (list
                        (+ (car p) (if (= (car p) rightX) dx 0))
                        (+ (cadr p)
                           (cond
                             ((= (cadr p) topY) dy)
                             ((= (cadr p) botY) (- dy))
                             (t 0)))))
                  )
                  ptlist))

              ;; 更新座標與線寬
              (vla-put-coordinates obj
                (vlax-make-variant
                  (vlax-safearray-fill
                    (vlax-make-safearray vlax-vbDouble (cons 0 (- (* 2 (length newpts)) 1)))
                    (apply 'append newpts))))
              (vla-put-constantwidth obj width)

              (princ (strcat "\n✅ 已更新 U 型套管 " (itoa tag)
                             "，長: " (rtos len 2 2)
                             " 高: " (rtos height 2 2)
                             " 寬度: " (rtos width 2 2)))
            )
            (princ "\n⚠️ 查無對應尺寸")
          )
        )
        (princ "\n⚠️ 需選取 Polyline + 數字文字")
      )
    )
    (princ "\n⚠️ 請選取 1 polyline + 1 尺寸文字")
  )
  (princ)
)




;───────── ④  主指令 CHGALL ───────
(defun c:CHGALL ( / ss i ent typ obj circles polyline textObj
                    sizeTag circleMatch outerDia innerDia peMatch pe
                    match len height width
                    coords ptlist topY botY rightX dx dy newpts )

  (setq ss (ssget '((0 . "LWPOLYLINE,CIRCLE,TEXT"))))
  (if (not ss)
    (progn (prompt "\n⚠️  沒有選取物件。") (princ)))

  (setq circles '() polyline nil textObj nil i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
          typ (cdr (assoc 0 (entget ent)))
          obj (vlax-ename->vla-object ent))
    (cond
      ((and (= typ "CIRCLE")
            (= (strcase (cdr (assoc 8 (entget ent)))) "機電_PIPE"))
       (setq circles (cons obj circles)))

      ((and (= typ "LWPOLYLINE")
            (= (strcase (cdr (assoc 8 (entget ent)))) "機電_PIPE另件"))
       (setq polyline obj))

      ((= typ "TEXT") (setq textObj obj)))
    (setq i (1+ i)))

  (if (not (and (<= 2 (length circles) 3) polyline textObj))
    (progn (prompt "\n⚠️  請選 2~3 圓 + 1 Polyline + 1 尺寸文字。") (princ)))

  (setq sizeTag (atoi (vla-get-textstring textObj)))

  ;; 圓依半徑排序 (大→小)
  (setq circles
    (vl-sort circles '(lambda (a b) (> (vla-get-radius a) (vla-get-radius b)))))

  ;;──  圓半徑處理  ──────────────────
  (setq circleMatch (assoc sizeTag circleTable))
  (if (not circleMatch)
    (progn (prompt "\n⚠️  circleTable 無此尺寸。") (princ)))

  (setq outerDia (cadr circleMatch)
        innerDia (caddr circleMatch))

  (cond
    ;; 兩圓
    ((= (length circles) 2)
     (vla-put-radius (nth 0 circles) (/ outerDia 2.0))
     (vla-put-radius (nth 1 circles) (/ innerDia 2.0)))

    ;; 三圓
    ((= (length circles) 3)
     (vla-put-radius (nth 1 circles) (/ outerDia 2.0))
     (vla-put-radius (nth 2 circles) (/ innerDia 2.0))
     (setq peMatch (assoc sizeTag peThicknessTable))
     (if peMatch
       (progn
         (setq pe (cadr peMatch))
         (vla-put-radius (nth 0 circles) (/ (+ outerDia (* 2.0 pe)) 2.0)))
       (prompt "\n⚠️  PE 厚度缺資料，最外圈未改。")))

    (t (prompt "\n⚠️  圓數量異常。")))

  ;;──  U 型封邊  ──────────────
  (setq match (assoc sizeTag udata))
  (if (not match)
    (progn (prompt "\n⚠️ udata 無此尺寸。") (princ)))

  (setq len (nth 1 match)  height (nth 2 match)  width (nth 3 match))

  ;; 重新計算 polyline 頂點
  (setq coords (vlax-safearray->list
                 (vlax-variant-value (vla-get-coordinates polyline))))
  (setq ptlist '() i 0)
  (repeat (/ (length coords) 2)
    (setq ptlist (cons (list (nth i coords) (nth (1+ i) coords)) ptlist)
          i (+ i 2)))
  (setq ptlist (reverse ptlist))

  (setq topY   (apply 'max (mapcar 'cadr ptlist))
        botY   (apply 'min (mapcar 'cadr ptlist))
        rightX (apply 'max (mapcar 'car ptlist))
        dx (- len   (- rightX (apply 'min (mapcar 'car ptlist))))
        dy (/ (- height (- topY botY)) 2.0))

  (setq newpts
    (mapcar
      (function
        (lambda (p)
          (list
            (+ (car p) (if (= (car p) rightX) dx 0))
            (+ (cadr p)
               (cond ((= (cadr p) topY) dy)
                     ((= (cadr p) botY) (- dy))
                     (t 0))))) )
      ptlist))

  (vla-put-coordinates polyline
    (vlax-make-variant
      (vlax-safearray-fill
        (vlax-make-safearray vlax-vbDouble
                             (cons 0 (- (* 2 (length newpts)) 1)))
        (apply 'append newpts))))

  (vla-put-constantwidth polyline width)
  (prompt (strcat "\n✅ 尺寸 " (itoa sizeTag) " 完成。"))
  (princ)
)


(defun getpropertyvalue (ename prop)
  (cdr (assoc 8 (entget ename))) ; 圖層名稱
)
