(defun c:BRKBYB ( / tol breakLen previousBreak inputBreak breakFile aSel bSel breakHalf iA iB entA entB objA objB intPt paramA vecA ptFrom ptTo )

  (vl-load-com)
  (setq tol 1e-6)

  ;; 讀取自訂的環境變數 BRK_DISTANCE
  (setq previousBreak (getenv "BRK_DISTANCE"))

  ;; 如果之前沒有儲存的值，使用預設值 10
  (if (not previousBreak)
    (setq breakLen 10)
    (setq breakLen (read previousBreak))
  )

  ;; 顯示先前儲存的值或預設值
  (setq inputBreak (getreal (strcat "
請輸入斷開總距離（目前值：" (rtos breakLen 2 2) "）：")))

  ;; 如果按空白鍵，保持先前的值
  (if (not inputBreak)
    (setq inputBreak breakLen)
  )

  ;; 確保 breakLen 為數值型態
  (setq breakLen (if (not (numberp inputBreak)) breakLen inputBreak))

  ;; 儲存新的斷開距離到環境變數
  (setenv "BRK_DISTANCE" (rtos breakLen 2 2))

  ;; 顯示正在使用的斷開距離
  (prompt (strcat "正在使用斷開距離：" (rtos breakLen 2 2)))

  ;; 輸入斷開總距離
  (setq breakHalf (/ breakLen 2.0))

  ;; 選擇 A 線段（會被斷開的）
  (prompt "\n請選擇 A 群組（要被斷開的 LINE / POLYLINE）...")
  (setq aSel (ssget '((0 . "LINE,LWPOLYLINE"))))

  ;; 選擇 B 線段（作為交點基準）
  (prompt "\\n請選擇 B 群組（交點參考線）...")
  (setq bSel (ssget '((0 . "LINE,LWPOLYLINE"))))

  ;; 逐條處理 A 線段
  (setq iA 0)
  (while (< iA (sslength aSel))
    (setq entA (ssname aSel iA))
    (setq objA (vlax-ename->vla-object entA))

    ;; 對每條 B 判斷是否有交點
    (setq iB 0)
    (while (< iB (sslength bSel))
      (setq entB (ssname bSel iB))
      (setq objB (vlax-ename->vla-object entB))

      ;; 嘗試取得交點
      (setq intPt (vlax-invoke objA 'IntersectWith objB acExtendNone))

      ;; 若有交點，執行 break
      (if (and intPt (= (length intPt) 3))
        (progn
          ;; 計算 A 線上參數值（用來取方向）
          (setq paramA (vlax-curve-getparamatpoint entA intPt))

          ;; 線上某點的方向向量（順著曲線）
          (setq vecA (vlax-curve-getfirstderiv entA paramA))

          ;; 正規化向量
          (setq vecA (mapcar '(lambda (x) (/ x (distance '(0 0 0) vecA))) vecA))

          ;; 取得 break 點 ±方向
          (setq ptFrom (mapcar '+ intPt (mapcar '(lambda (x) (* -1 breakHalf x)) vecA)))
          (setq ptTo   (mapcar '+ intPt (mapcar '(lambda (x) (* breakHalf x)) vecA)))

          ;; 執行 break
          (command "_.BREAK" entA "_non" ptFrom "_non" ptTo)
        )
      )
      (setq iB (1+ iB))
    )
    (setq iA (1+ iA))
  )

  (princ)
)
