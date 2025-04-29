(defun c:QS ( / optMap get-multi-kword selectedList acadTypes ss baseEnt baseLayer ents i ent entData entLayer entType result )

  (vl-load-com)

  ;; 類型縮寫對照表
  (setq optMap
    '(("T"  . "TEXT")         ;; 文字
      ("I"  . "INSERT")       ;; 圖塊
      ("L"  . "LINE")         ;; 線
      ("PL" . "LWPOLYLINE"))  ;; 聚合線
  )

  ;; 自定函數：多次 getkword 選擇
  (defun get-multi-kword ( / opt selected done )
    (setq selected '())
    (setq done nil)
    (while (not done)
      (initget "T I L PL")
      (setq opt (getkword "\n選擇類型 [文字(T)/圖塊(I)/線(L)/聚合線(PL)]，按 Enter 結束："))
      (cond
        ((null opt) (setq done T))  ;; Enter 結束
        ((not (member opt selected)) (setq selected (cons opt selected)))
        (T (princ "\n已選擇過，略過。"))
      )
    )
    selected
  )

  ;; 呼叫選單
  (setq selectedList (get-multi-kword))
  (if (null selectedList)
    (progn (princ "\n未選擇任何類型，結束。") (exit))
  )

  ;; 將縮寫轉換為 AutoCAD 類型名稱
  (setq acadTypes (mapcar '(lambda (x) (cdr (assoc x optMap))) selectedList))

  ;; 選擇一個基準物件（決定圖層）
  (setq ss (ssget "_I"))
  (if (or (not ss) (= (sslength ss) 0))
    (progn
      (princ "\n請選擇一個基準物件（作為圖層參考）：")
      (setq ss (ssget "_+.:S"))
    )
  )
  (if (or (not ss) (= (sslength ss) 0))
    (progn (princ "\n未選擇任何物件，結束。") (exit))
  )
  (setq baseEnt (ssname ss 0))
  (setq baseLayer (cdr (assoc 8 (entget baseEnt))))

  ;; 拉框圈選物件（自由選擇）
  (princ "\n請框選區域（像 SELECT 一樣）：")
  (setq ents (ssget))
  (if (not ents)
    (progn (princ "\n沒有選取任何物件，結束。") (exit))
  )

  ;; 遍歷並比對圖層與類型
  (setq result (ssadd))
  (setq i 0)
  (while (< i (sslength ents))
    (setq ent (ssname ents i))
    (setq entData (entget ent))
    (setq entLayer (cdr (assoc 8 entData)))
    (setq entType (cdr (assoc 0 entData)))
    (if (and (= entLayer baseLayer)
             (member entType acadTypes))
      (ssadd ent result)
    )
    (setq i (1+ i))
  )

  ;; 顯示結果
  (if (> (sslength result) 0)
    (progn
      (sssetfirst nil result)
      (princ (strcat "\n選取成功，共 " (itoa (sslength result)) " 個符合物件。"))
    )
    (princ "\n找不到符合條件的物件。")
  )
  (princ)
)
