(defun c:DIVCPY ( / entA objA pt1 pt2 n i param pts entB objB baseVec prevN doDelete doc space)
  (vl-load-com)
  
  ;; 開始 UNDO 群組
  (command "_.UNDO" "_Begin")
  
  ;; 獲取文件和空間對象
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        space (vla-get-ModelSpace doc))
  
  ;; 讀取上次的分段數
  (setq prevN (getenv "DIVCPY_N"))
  (if (not prevN) (setq prevN "2"))
  
  ;; 選擇 A 線段
  (princ "\n請選擇要等分的線段 A：")
  (setq entA (car (entsel)))
  (if (not entA)
    (progn 
      (command "_.UNDO" "_End")
      (princ "\n未選擇有效物件 A，結束。") 
      (exit))
  )
  (setq objA (vlax-ename->vla-object entA))
  
  ;; 檢查物件類型
  (if (/= (vla-get-ObjectName objA) "AcDbLine")
    (progn 
      (command "_.UNDO" "_End")
      (princ "\n請選擇直線物件。") 
      (exit))
  )

  ;; 分段數量（顯示上次使用的值）
  (setq n (getint (strcat "\n請輸入要分成幾段（整數 ≥ 2）<" prevN ">：")))
  (if (not n) 
    (setq n (atoi prevN))
    (setenv "DIVCPY_N" (itoa n)) ;; 儲存新的值
  )
  (if (< n 2)
    (progn 
      (command "_.UNDO" "_End")
      (princ "\n分段數必須 ≥ 2，結束。") 
      (exit))
  )

  ;; 詢問是否要刪除原始線段
  (initget "Yes No")
  (setq doDelete (getkword "\n是否要刪除原始線段？[Yes/No] <No>："))

  ;; 取得線段端點
  (setq pt1 (vlax-get objA 'StartPoint)
        pt2 (vlax-get objA 'EndPoint))

  ;; 計算等分點
  (setq pts nil)
  (setq i 1)
  (repeat (1- n)
    (setq param (/ (float i) n))
    (setq pts (cons
               (mapcar '(lambda (p1 p2) 
                         (+ p1 (* param (- p2 p1))))
                      pt1 pt2)
               pts))
    (setq i (1+ i))
  )
  (setq pts (reverse pts))

  ;; 選擇 B 物件
  (princ "\n請選擇要複製的物件 B：")
  (setq entB (car (entsel)))
  (if (not entB)
    (progn 
      (command "_.UNDO" "_End")
      (princ "\n未選擇有效物件 B，結束。") 
      (exit))
  )
  (setq objB (vlax-ename->vla-object entB))

  ;; 嘗試獲取物件B的基準點
  (setq baseVec (vl-catch-all-apply 'vlax-get (list objB 'InsertionPoint)))
  (if (vl-catch-all-error-p baseVec)
    (progn
      (vla-GetBoundingBox objB 'minPt 'maxPt)
      (setq minPt (vlax-safearray->list minPt)
            maxPt (vlax-safearray->list maxPt)
            baseVec (mapcar '(lambda (a b) (/ (+ a b) 2.0)) 
                           minPt maxPt))
    )
  )

  ;; 將線段 A 在等分點處裁斷
  (if (/= doDelete "Yes")
    (progn
      ;; 創建新線段
      (setq prev-pt pt1)
      (foreach pt pts
        (vla-AddLine space 
          (vlax-3d-point prev-pt)
          (vlax-3d-point pt)
        )
        (setq prev-pt pt)
      )
      ;; 添加最後一段
      (vla-AddLine space 
        (vlax-3d-point prev-pt)
        (vlax-3d-point pt2)
      )
      ;; 刪除原始線段
      (vla-Delete objA)
    )
  )

  ;; 將 B 複製到各分點（包括起點）
  (foreach pt (cons pt1 pts)
    (command "_.COPY" entB "" "_non" baseVec "_non" pt)
  )

  ;; 如果需要刪除原始線段
  (if (= doDelete "Yes")
    (entdel entA)
  )

  ;; 結束 UNDO 群組
  (command "_.UNDO" "_End")

  (princ (strcat "\n完成！線段已等分為 " (itoa n) " 段"))
  (princ)
)
