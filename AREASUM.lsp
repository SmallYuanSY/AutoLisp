(defun c:AREASUM ( / ss i ent total area entType obj)
  (vl-load-com)
  (setq total 0.0)
  
  ;; 選擇物件（支援封閉的多段線、圓形、區域等）
  (prompt "\n請選取要計算面積的物件（支援封閉多段線、圓形、區域等）：")
  (setq ss (ssget '((0 . "LWPOLYLINE,CIRCLE,REGION,ELLIPSE,HATCH,SOLID,3DFACE"))))
  
  (if ss
    (progn
      (setq i 0)
      (prompt "\n正在計算面積...")
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entType (cdr (assoc 0 (entget ent))))
        
        ;; 根據物件類型計算面積
        (cond
          ;; 封閉的多段線
          ((= entType "LWPOLYLINE")
           (setq obj (vlax-ename->vla-object ent))
           (if (= (vla-get-Closed obj) :vlax-true)
             (progn
               (setq area (vlax-get-property obj 'Area))
               (setq total (+ total area))
               (prompt (strcat "\n多段線面積: " (rtos area 2 4)))
             )
             (prompt "\n警告: 發現未封閉的多段線，已跳過")
           )
          )
          
          ;; 圓形
          ((= entType "CIRCLE")
           (setq obj (vlax-ename->vla-object ent))
           (setq area (vlax-get-property obj 'Area))
           (setq total (+ total area))
           (prompt (strcat "\n圓形面積: " (rtos area 2 4)))
          )
          
          ;; 區域
          ((= entType "REGION")
           (setq obj (vlax-ename->vla-object ent))
           (setq area (vlax-get-property obj 'Area))
           (setq total (+ total area))
           (prompt (strcat "\n區域面積: " (rtos area 2 4)))
          )
          
          ;; 橢圓
          ((= entType "ELLIPSE")
           (setq obj (vlax-ename->vla-object ent))
           (setq area (vlax-get-property obj 'Area))
           (setq total (+ total area))
           (prompt (strcat "\n橢圓面積: " (rtos area 2 4)))
          )
          
          ;; 填充圖樣
          ((= entType "HATCH")
           (setq obj (vlax-ename->vla-object ent))
           (setq area (vlax-get-property obj 'Area))
           (setq total (+ total area))
           (prompt (strcat "\n填充面積: " (rtos area 2 4)))
          )
          
          ;; 實體填充
          ((= entType "SOLID")
           (setq obj (vlax-ename->vla-object ent))
           (setq area (vlax-get-property obj 'Area))
           (setq total (+ total area))
           (prompt (strcat "\n實體面積: " (rtos area 2 4)))
          )
          
          ;; 3D面
          ((= entType "3DFACE")
           (setq obj (vlax-ename->vla-object ent))
           (setq area (vlax-get-property obj 'Area))
           (setq total (+ total area))
           (prompt (strcat "\n3D面積: " (rtos area 2 4)))
          )
          
          ;; 其他類型
          (t
           (prompt (strcat "\n警告: 不支援的物件類型 " entType "，已跳過"))
          )
        )
        (setq i (1+ i))
      )
      
      ;; 顯示最終結果
      (prompt (strcat "\n" (chr 13) (chr 10) "=== 面積加總結果 ==="))
      (prompt (strcat "\n已處理物件數量: " (itoa (sslength ss))))
      (prompt (strcat "\n總面積: " (rtos total 2 4)))
      (prompt (strcat "\n總面積 (平方公尺): " (rtos (/ total 1000000.0) 2 6)))
      (prompt (strcat "\n總面積 (平方公分): " (rtos (/ total 100.0) 2 2)))
      
      ;; 可選擇是否在圖面上標註結果
      (initget "Yes No")
      (setq choice (getkword "\n是否要在圖面上標註總面積？ [Yes/No] <No>: "))
      (if (= choice "Yes")
        (progn
          (setq pt (getpoint "\n請選擇標註位置："))
          (if pt
            (progn
              (command "_TEXT" "_J" "_ML" pt 300 0 (strcat "總面積: " (rtos total 2 4)))
              (prompt "\n已在圖面上標註面積")
            )
          )
        )
      )
    )
    (prompt "\n沒有選取任何物件。")
  )
  (princ)
)

;; 載入提示
(prompt "\n面積加總工具已載入，輸入 AREASUM 開始使用。")
(princ) 