(defun c:TXTEXPORT ( / ss lst rows sorted f filepath doc ytol i ent pt txt found sortedRow a b pairs item row y)
  (vl-load-com)
  
  (princ "\n=== 文字配對輸出工具 ===")
  (princ "\n此工具會將文字按行配對並追加到TXT檔案")

  (setq ytol 10.0)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq filepath (strcat (vl-filename-directory (vla-get-FullName doc)) "\\exported_text.txt"))

  (princ "\n")
  (princ "\n📝 請選取要處理的文字物件...")
  
  ;; 選取所有文字
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if (not ss) 
    (progn 
      (princ "\n❌ 未選取任何文字物件") 
      (princ "\n💡 請選取 TEXT 或 MTEXT 物件")
      (princ)
      (exit)
    )
  )

  (princ (strcat "\n✅ 已選取 " (itoa (sslength ss)) " 個文字物件"))

  ;; 收集資料 ((pt) . "文字")
  (princ "\n")
  (princ "\n📊 正在分析文字位置...")
  (setq lst '())
  (repeat (setq i (sslength ss))
    (setq ent (vlax-ename->vla-object (ssname ss (setq i (1- i)))))
    (setq pt (vlax-get ent 'InsertionPoint))
    (setq txt (vla-get-TextString ent))
    (setq lst (cons (cons pt txt) lst))
  )

  ;; Y 值排序（上到下），並依據列群組
  (princ "\n🔄 正在依位置排序...")
  (setq lst
    (vl-sort lst
      (function
        (lambda (a b)
          (> (cadr (car a)) (cadr (car b))) ; 從上到下
        )
      )
    )
  )

  ;; 分組：同一列的放一起
  (princ "\n📋 正在分組同一列的文字...")
  (setq rows '())
  (foreach item lst
    (setq pt (car item))
    (setq y (cadr pt))
    (setq found nil)
    (setq rows
      (mapcar
        (function
          (lambda (row)
            (if (and (not found) (equal y (cadr (car (car row))) ytol))
              (progn (setq found T) (append row (list item)))
              row
            )
          )
        )
        rows
      )
    )
    (if (not found) (setq rows (append rows (list (list item)))))
  )

  (princ (strcat "\n✅ 已分組成 " (itoa (length rows)) " 列"))

  ;; 每一列：X 排序 → 每兩項組成 pair
  (princ "\n🔗 正在配對同列文字...")
  (setq pairs '())
  (foreach row rows
    (setq sortedRow
      (vl-sort row
        (function (lambda (a b) (< (car (car a)) (car (car b)))))
      )
    )
    (while (>= (length sortedRow) 2)
      (setq a (car sortedRow))
      (setq b (cadr sortedRow))
      (setq pairs (cons (list (cdr a) (cdr b)) pairs))
      (setq sortedRow (cddr sortedRow))
    )
  )

  (princ (strcat "\n✅ 已產生 " (itoa (length pairs)) " 組配對"))

  ;; 顯示配對結果預覽
  (if (> (length pairs) 0)
    (progn
      (princ "\n")
      (princ "\n📋 配對結果預覽：")
      (princ "\n--------------------------------------------------")
      (setq i 0)
      (foreach pair pairs
        (setq i (1+ i))
        (princ (strcat "\n" (itoa i) ". " (car pair) " + " (cadr pair) " = " (car pair) "_" (cadr pair)))
        (if (> i 5) 
          (progn
            (princ (strcat "\n... 還有 " (itoa (- (length pairs) 5)) " 組配對"))
            (setq pairs (reverse pairs)) ; 結束迴圈
          )
        )
      )
      (princ "\n--------------------------------------------------")
    )
  )

  ;; 對所有代號排序（字串排序）
  (princ "\n🔤 正在按字母順序排序...")
  (setq sorted
    (vl-sort pairs
      (function (lambda (a b) (< (car a) (car b))))
    )
  )

  ;; 寫入文字檔（改用追加模式）
  (princ "\n💾 正在追加到檔案...")
  (setq f (open filepath "a"))
  (if f
    (progn
      (foreach pair sorted
        (write-line (strcat (car pair) "_" (cadr pair)) f)
      )
      (close f)
      
      (princ "\n")
      (princ "\n============================================================")
      (princ "\n🎉 文字配對追加完成！")
      (princ "\n============================================================")
      (princ (strcat "\n📁 輸出檔案：" filepath))
      (princ (strcat "\n📊 處理文字：" (itoa (sslength ss)) " 個"))
      (princ (strcat "\n🔗 配對數量：" (itoa (length pairs)) " 組"))
      (princ (strcat "\n📋 分組列數：" (itoa (length rows)) " 列"))
      (princ "\n--------------------------------------------------")
      (princ "\n💡 提示：內容已追加到現有檔案")
    )
    (progn
      (princ "\n❌ 無法開啟輸出檔案")
      (princ (strcat "\n📁 嘗試路徑：" filepath))
      (princ "\n💡 請檢查目錄權限或磁碟空間")
    )
  )
  
  (princ)
)

(princ "\n✅ TXTEXPORT.lsp 已載入")
(princ "\n📖 指令：TXTEXPORT - 將選取的文字按位置配對並追加到TXT檔案")
(princ)
