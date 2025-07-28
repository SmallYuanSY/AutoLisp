(defun c:RA ( / ss i ent total area obj unit-choice conversion-factor area-results area-m2 write-to-text label-areas)
  (vl-load-com)
  (setq total 0.0)
  (setq area-results '()) ; 存儲所有面積結果
  
  ;; 選擇單位
  (initget "Mm Cm")
  (setq unit-choice (getkword "\n您的圖面單位是什麼？ [Mm/<Cm>]: "))
  (if (or (null unit-choice) (= unit-choice "")) (setq unit-choice "Cm"))
  
  ;; 統一轉換為小寫
  (setq unit-choice (strcase unit-choice T))
  
  ;; 設定換算係數（將AutoCAD面積單位轉換為m²）
  (cond
    ((= unit-choice "mm") 
     (setq conversion-factor (/ 1.0 1000000.0)) ; mm² to m²
     (prompt "\n圖面單位: mm (面積將從 mm² 轉換為 m²)")
    )
    ((= unit-choice "cm") 
     (setq conversion-factor (/ 1.0 10000.0))   ; cm² to m²
     (prompt "\n圖面單位: cm (面積將從 cm² 轉換為 m²)")
    )
  )
  
  ;; 無條件捨去到小數第二位的函數
  (defun floor-to-decimal (num places)
    (/ (fix (* num (expt 10.0 places))) (expt 10.0 places))
  )
  
  ;; 選擇物件
  (prompt "\n請選取要讀取面積的物件：")
  (setq ss (ssget))
  
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
                 ;; 嘗試直接讀取面積屬性
         (if (vlax-property-available-p obj 'Area)
           (progn
             (setq area (vlax-get-property obj 'Area))
             (if (> area 0)
               (progn
                 ;; 顯示原始面積值（用於調試）
                 (prompt (strcat "\n物件 " (itoa (+ i 1)) " 原始面積: " (rtos area 2 4) " (" unit-choice "²)"))
                 ;; 換算成m²並無條件捨去到小數第二位
                 (setq area-m2 (floor-to-decimal (* area conversion-factor) 2))
                 (setq total (+ total area-m2))
                 ;; 將結果存儲到列表中
                 (setq area-results (cons (list (+ i 1) area-m2) area-results))
                 (prompt (strcat "   → 轉換為: " (rtos area-m2 2 2) " m²"))
               )
               (prompt (strcat "\n物件 " (itoa (+ i 1)) " 無面積資料"))
             )
           )
           (prompt (strcat "\n物件 " (itoa (+ i 1)) " 不支援面積查詢"))
         )
        (setq i (1+ i))
      )
      
             ;; 顯示總計
       (prompt (strcat "\n" (chr 13) (chr 10) "=== 面積讀取結果 ==="))
       (prompt (strcat "\n總面積: " (rtos total 2 2) " m²"))
       
       ;; 詢問是否要將結果寫入TEXT物件
       (initget "Yes No")
       (setq write-to-text (getkword "\n是否要將結果寫入TEXT物件？ [Yes/<No>]: "))
       (if (= write-to-text "Yes")
         (write-results-to-text area-results total)
       )
       
       ;; 詢問是否要在物件中心標註面積
       (initget "Yes No")
       (setq label-areas (getkword "\n是否要在每個物件中心標註面積？ [Yes/<No>]: "))
       (if (= label-areas "Yes")
         (label-areas-on-objects area-results ss)
       )
    )
    (prompt "\n沒有選取任何物件。")
  )
  (princ)
)

;; 單一物件面積查詢
(defun c:GETAREA ( / ent obj area)
  (vl-load-com)
  (setq ent (car (entsel "\n請選取一個物件：")))
  
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (if (vlax-property-available-p obj 'Area)
        (progn
          (setq area (vlax-get-property obj 'Area))
          (if (> area 0)
            (progn
              (prompt (strcat "\n物件面積: " (rtos area 2 4)))
              (prompt (strcat "\n物件面積 (m²): " (rtos (/ area 1000000.0) 2 6)))
              (prompt (strcat "\n物件面積 (cm²): " (rtos (/ area 100.0) 2 2)))
            )
            (prompt "\n此物件無面積資料")
          )
        )
        (prompt "\n此物件不支援面積查詢")
      )
    )
    (prompt "\n沒有選取任何物件。")
  )
  (princ)
)

;; 顯示物件的所有可用屬性
(defun c:SHOWPROPS ( / ent obj props)
  (vl-load-com)
  (setq ent (car (entsel "\n請選取一個物件：")))
  
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (prompt (strcat "\n物件類型: " (vla-get-ObjectName obj)))
      
      ;; 檢查常用屬性
      (if (vlax-property-available-p obj 'Area)
        (prompt (strcat "\n面積: " (rtos (vlax-get-property obj 'Area) 2 4)))
      )
      (if (vlax-property-available-p obj 'Length)
        (prompt (strcat "\n長度: " (rtos (vlax-get-property obj 'Length) 2 4)))
      )
      (if (vlax-property-available-p obj 'Radius)
        (prompt (strcat "\n半徑: " (rtos (vlax-get-property obj 'Radius) 2 4)))
      )
      (if (vlax-property-available-p obj 'Perimeter)
        (prompt (strcat "\n周長: " (rtos (vlax-get-property obj 'Perimeter) 2 4)))
      )
      (if (vlax-property-available-p obj 'Closed)
        (prompt (strcat "\n封閉: " (if (= (vlax-get-property obj 'Closed) :vlax-true) "是" "否")))
      )
    )
    (prompt "\n沒有選取任何物件。")
  )
  (princ)
)

;; 將面積結果寫入TEXT物件的函數
(defun write-results-to-text (results total / text-ent text-obj result-string item)
  (prompt "\n請選擇要更新的TEXT物件：")
  (setq text-ent (car (entsel)))
  
  (if (and text-ent 
           (or (= (cdr (assoc 0 (entget text-ent))) "TEXT")
               (= (cdr (assoc 0 (entget text-ent))) "MTEXT")))
    (progn
      ;; 反轉結果列表（因為cons是反向添加的）
      (setq results (reverse results))
      
      ;; 建立數學表達式格式的結果字符串
      (setq result-string "(")
      (setq first-item T)
      (foreach item results
        (if first-item
          (progn
            (setq result-string (strcat result-string (rtos (cadr item) 2 2)))
            (setq first-item nil)
          )
          (setq result-string (strcat result-string "+" (rtos (cadr item) 2 2)))
        )
      )
      (setq result-string 
        (strcat result-string ")㎡ =" (rtos total 2 2) "㎡"))
      
      ;; 更新TEXT物件
      (setq text-obj (vlax-ename->vla-object text-ent))
      (vla-put-TextString text-obj result-string)
      (prompt "\n✅ 已成功將面積結果寫入TEXT物件")
    )
    (prompt "\n❌ 請選擇有效的TEXT或MTEXT物件")
  )
)

;; 獲取物件中心點的函數
(defun get-object-center (obj / center minPt maxPt)
  (cond
    ;; 嘗試使用質心
    ((vlax-property-available-p obj 'Centroid)
     (vlax-safearray->list (vlax-get-property obj 'Centroid))
    )
    ;; 嘗試使用圓心
    ((vlax-property-available-p obj 'Center)
     (vlax-safearray->list (vlax-get-property obj 'Center))
    )
    ;; 使用邊界框計算幾何中心
    (t
     (vla-GetBoundingBox obj 'minPt 'maxPt)
     (setq minPt (vlax-safearray->list minPt))
     (setq maxPt (vlax-safearray->list maxPt))
     (list 
       (/ (+ (car minPt) (car maxPt)) 2.0)
       (/ (+ (cadr minPt) (cadr maxPt)) 2.0)
       0.0
     )
    )
  )
)

;; 在物件中心標註面積的函數
(defun label-areas-on-objects (results ss / sample-text sample-ent i ent obj center-pt new-text area-value sample-pt)
  (prompt "\n請選擇範例文字（作為複製模板）：")
  (setq sample-ent (car (entsel)))
  
  (if (and sample-ent 
           (or (= (cdr (assoc 0 (entget sample-ent))) "TEXT")
               (= (cdr (assoc 0 (entget sample-ent))) "MTEXT")))
    (progn
      (setq sample-text (vlax-ename->vla-object sample-ent))
      (setq results (reverse results)) ; 反轉結果列表
      
      ;; 獲取範例文字的插入點（只需要獲取一次）
      (setq sample-pt (vlax-safearray->list (vlax-variant-value (vlax-get-property sample-text 'InsertionPoint))))
      
      ;; 對每個物件進行標註
      (setq i 0)
      (foreach result-item results
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq center-pt (get-object-center obj))
        (setq area-value (rtos (cadr result-item) 2 2))
        
        ;; 使用COPY指令複製範例文字到中心點
        (command "_COPY" sample-ent "" "_non" sample-pt "_non" center-pt)
        
        ;; 取得新複製的文字並修改內容
        (setq new-text (vlax-ename->vla-object (entlast)))
        (vlax-put-property new-text 'TextString (strcat area-value "㎡"))
        
        (setq i (1+ i))
      )
      (prompt (strcat "\n✅ 已在 " (itoa (length results)) " 個物件中心標註面積"))
    )
    (prompt "\n❌ 請選擇有效的TEXT或MTEXT物件作為範例")
  )
)

;; 載入提示
(prompt "\n面積讀取工具已載入:")
(prompt "\n  RA - 讀取多個物件面積")
(prompt "\n  GETAREA  - 讀取單一物件面積")
(prompt "\n  SHOWPROPS - 顯示物件屬性")
(princ) 