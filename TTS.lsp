;; TEXTTOOLS - 文字工具整合入口，記住上一次使用的選項
(setq *texttools-last* "RET") ; 初始預設為 RET

(defun c:TTS ( / opt )
  (princ "\n請選擇文字處理工具：")
  (initget "RET RETM RETX ART RT TCO TS")

  ;; 檢查預設值是否存在，避免 nil 錯誤
  (if (not *texttools-last*) (setq *texttools-last* "RET"))

  ;; 顯示選單，預設為上一次使用的選項
  (setq opt (getkword (strcat "\n功能選項: [單數字遞增(RET)/區間文字遞增(RETM)/樣式插入(RETX)/文字全替換(ART)/文字替換(RT)/文字覆蓋(TCO)/文字交換(TS)] <" *texttools-last* ">: ")))

  ;; 使用預設值（若直接按 Enter）
  (if (or (null opt) (= opt "")) (setq opt *texttools-last*))

  ;; 更新預設為這次選擇
  (setq *texttools-last* opt)

  ;; 執行對應指令
  (cond
    ((= opt "RET") (c:RET))
    ((= opt "RETM") (c:RETM))
    ((= opt "RETX") (c:RETX))
    ((= opt "ART") (c:ART))
    ((= opt "RT") (c:RT))
    ((= opt "TCO") (c:TCO))
    ((= opt "TS") (c:TS))
    (t (princ "\n無效的選項，請重新執行。"))
  )
  (princ)
)


;; RET - Rename Text with Pattern and Sorting Options
(defun c:RET ( / inputStr startNum ss lst sorted i ent obj pt prefix suffix asteriskPos numStr newText starLen sortOrder len )
  (vl-load-com)
  (princ "輸入樣式會將文字更換成對應格式，只能夠輸入一組**")
  (setq inputStr (getstring "\n請輸入樣式（使用 ** 表示位數，例如 V1-(**B)）: "))
  (setq asteriskPos (vl-string-search "*" inputStr))
  (if (not asteriskPos)
    (princ "\n樣式中找不到 *，請重新輸入。")
    (progn
      (setq starLen 1)
      (while (= (substr inputStr (+ asteriskPos starLen +1) 1) "*")
        (setq starLen (1+ starLen))
      )
      (initget 7)
      (setq startNum (getint (strcat "\n請輸入起始數字（補零至 " (itoa starLen) " 位）: ")))
      (setq prefix (substr inputStr 1 asteriskPos))
      (setq suffix (substr inputStr (+ asteriskPos starLen +1)))
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
      (if ss
        (progn
          (setq lst '() i 0 len (sslength ss))
          (while (< i len)
            (setq ent (ssname ss i))
            (setq obj (vlax-ename->vla-object ent))
            (setq pt (vlax-get obj 'InsertionPoint))
            (setq lst (cons (list (car pt) (cadr pt) obj) lst))
            (setq i (1+ i))
          )
          (setq sortOrder (get-sort-order))
          (setq sorted (sort-entities lst sortOrder))
          (setq i 0)
          (foreach item sorted
            (setq obj (nth 2 item))
            (setq numStr (itoa (+ startNum i)))
            (while (< (strlen numStr) starLen)
              (setq numStr (strcat "0" numStr))
            )
            (setq newText (strcat prefix numStr suffix))
            (if (= (vla-get-ObjectName obj) "AcDbText")
              (vla-put-TextString obj newText)
              (vla-put-Contents obj newText)
            )
            (setq i (1+ i))
          )
          (princ (strcat "\n成功從 " (itoa startNum) " 開始命名，共 " (itoa i) " 個文字。"))
        )
        (princ "\n未選取任何文字。")
      )
    )
  )
  (princ)
)

;; RETM - Rename Text with Range Format (e.g., AA-(**~**))
(defun c:RETM ( / inputStr ss lst sorted i ent obj pt prefix joiner suffix s1 s2 startNum stepNum padZero n1 n2 numStr1 numStr2 pos1 pos2 joinerInput len )
  (vl-load-com)
  (setq inputStr (getstring "\n請輸入樣式（使用 **~** 或 ***~** 表示區段範圍，例如 AA-(**~**)）: "))
  (if (not (wcmatch inputStr "*`**`**"))
    (progn (princ "\n格式錯誤，請使用兩組連續星號 **~** 或 ***~**。") (exit))
  )
  (setq prefix "" joiner "" suffix "" s1 0 s2 0)
  (setq pos1 (vl-string-search "*" inputStr))
  (setq s1 1)
  (while (= (substr inputStr (+ pos1 s1) 1) "*") (setq s1 (1+ s1)))
  (setq prefix (substr inputStr 1 pos1))
  (setq pos2 (vl-string-search "*" inputStr (+ pos1 s1)))
  (setq s2 1)
  (while (= (substr inputStr (+ pos2 s2) 1) "*") (setq s2 (1+ s2)))
  (setq joiner (substr inputStr (+ pos1 s1) (- pos2 (+ pos1 s1))))
  (setq suffix (substr inputStr (+ pos2 s2)))
  (setq joinerInput (getstring "\n請輸入區段連接符號（例如 ~ 或 -），預設為 -："))
  (if (or (null joinerInput) (= joinerInput "")) (setq joinerInput "-"))
  (initget 7)
  (setq startNum (getint "\n請輸入起始數字（如 1）: "))
  (initget 6)
  (setq stepNum (getint "\n請輸入每段區間數（如 5）: "))
  (initget "Y N")
  (setq padZero (getkword "\n是否補 0 至指定位數？ [Y/N] <N>: "))
  (if (null padZero) (setq padZero "N"))
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq lst '() i 0 len (sslength ss))
      (while (< i len)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq pt (vlax-get obj 'InsertionPoint))
        (setq lst (cons (list (car pt) (cadr pt) obj) lst))
        (setq i (1+ i))
      )
      (setq sorted (sort-entities lst (get-sort-order)))
      (setq i 0)
      (foreach item sorted
        (setq obj (nth 2 item))
        (setq n1 (+ startNum (* i stepNum)))
        (setq n2 (+ n1 (- stepNum 1)))
        (setq numStr1 (itoa n1))
        (setq numStr2 (itoa n2))
        (if (= padZero "Y")
          (progn
            (while (< (strlen numStr1) s1) (setq numStr1 (strcat "0" numStr1)))
            (while (< (strlen numStr2) s2) (setq numStr2 (strcat "0" numStr2)))
          )
        )
        (setq joiner joinerInput)
        (setq newText (strcat prefix numStr1 joiner numStr2 suffix))
        (if (= (vla-get-ObjectName obj) "AcDbText")
          (vla-put-TextString obj newText)
          (vla-put-Contents obj newText)
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n成功套用 " (itoa i) " 筆區段範圍文字。"))
    )
    (princ "\n未選取任何文字。")
  )
  (princ)
)

;; RETX - 樣式插入文字（e.g. XRA**P）
;; 這個函數會將選取的文字物件的內容改為指定的樣式
;; 例如：將所有選取的文字改為 "XRA**P"，其中 ** 會被替換為原文字
(defun c:RETX ( / style ss ent obj baseText resultText pos pre suf i len )
  (vl-load-com)
  (setq style (getstring T "\n請輸入樣式（使用 ** 表示原文字插入位置，例如 XRA**P）: "))
  (setq pos (vl-string-search "**" style))
  (if (not pos)
    (progn (princ "\n格式錯誤，請輸入包含 ** 的樣式。") (exit))
  )
  (setq pre (substr style 1 pos))
  (setq suf (substr style (+ pos 3)))
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0 len (sslength ss))
      (while (< i len)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (if (= (vla-get-ObjectName obj) "AcDbText")
          (setq baseText (vla-get-TextString obj))
          (setq baseText (vla-get-Contents obj))
        )
        (setq resultText (strcat pre baseText suf))
        (if (= (vla-get-ObjectName obj) "AcDbText")
          (vla-put-TextString obj resultText)
          (vla-put-Contents obj resultText)
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n成功處理 " (itoa len) " 個文字。"))
    )
    (princ "\n未選取任何文字。")
  )
  (princ)
)

;; ATX - 將所有選取文字改為同一內容
;; 這個函數會將選取的所有文字物件的內容改為指定的文字
;; 例如：將所有選取的文字改為 "Hello World"
(defun c:ART ( / newval ss ent obj i len )
  (vl-load-com)
  (setq newval (getstring T "\n請輸入要套用的文字內容: "))
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0 len (sslength ss))
      (while (< i len)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (if (= (vla-get-ObjectName obj) "AcDbText")
          (vla-put-TextString obj newval)
          (vla-put-Contents obj newval)
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n已成功將 " (itoa len) " 個文字物件內容改為：" newval))
    )
    (princ "\n未選取任何文字。")
  )
  (princ)
)

;; RT - 替換文字內容
;; 這個函數會將選取的文字物件中，包含指定的舊文字（oldStr）替換為新的文字（newStr）
;; 例如：將所有包含 "AA" 的文字替換為 "BB"
(defun c:RT ( / ss i ent txt oldStr newStr newtxt)
  (setq oldStr (getstring "\n請輸入要被替換的文字（如 AA）: "))
  (setq newStr (getstring "\n請輸入新的文字（如 BB）: "))

  (setq ss (ssget '((0 . "TEXT")))) ; 只選 TEXT，若要支援 MTEXT 可擴充
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq txt (cdr (assoc 1 (entget ent)))) ; 取得原始文字
        (if (wcmatch txt (strcat "*" oldStr "*")) ; 檢查是否包含 oldStr
          (progn
            (setq newtxt (vl-string-subst newStr oldStr txt)) ; 替換文字
            (entmod (subst (cons 1 newtxt) (assoc 1 (entget ent)) (entget ent)))
            (entupd ent)
          )
        )
        (setq i (1+ i))
      )
      (princ "\n替換完成。")
    )
    (princ "\n請選取要替換的文字。")
  )
  (princ)
)

;; TCO - 覆蓋文字內容
;; 將文字內容為來源文字的內容
(defun c:TCO ( / srcSet dstSet srcList dstList sortOrder i srcEnt dstEnt srcText dstData)
  (vl-load-com)

  (princ "\n請選擇來源文字（可多選）：")
  (setq srcSet (ssget '((0 . "TEXT,MTEXT"))))

  (princ "\n請選擇目標文字（可多選）：")
  (setq dstSet (ssget '((0 . "TEXT,MTEXT"))))

  (if (and srcSet dstSet)
    (progn
      (setq sortOrder (get-sort-order))
      (setq srcList (ss-to-poslist srcSet))
      (setq dstList (ss-to-poslist dstSet))

      (setq srcList (TCO-sort-entities srcList sortOrder))
      (setq dstList (TCO-sort-entities dstList sortOrder))

      (setq i 0)
      (while (and (< i (length srcList)) (< i (length dstList)))
        (setq srcEnt (cdr (nth i srcList)))
        (setq dstEnt (cdr (nth i dstList)))

        (vl-catch-all-apply
          (function
            (lambda ()
              (setq srcText (cdr (assoc 1 (entget srcEnt))))
              (setq srcText (vl-string-subst "" "%%U" srcText)) ; 去除下劃線
              (setq dstData (entget dstEnt))
              (setq dstData (subst (cons 1 srcText) (assoc 1 dstData) dstData))
              (entmod dstData)
              (entupd dstEnt)
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n完成複製 " (itoa i) " 筆文字資料。"))
    )
    (princ "\n請正確選取來源和目標文字。")
  )
  (princ)
)

;; TS - 交換兩個文字物件的內容
;; 這個函數會將選取的兩個文字物件的內容互相交換
(defun c:TS ( / ss ent1 ent2 edata1 edata2 txt1 txt2 pair1 pair2)
  ;; / 後面宣告的是區域變數，避免影響全域環境

  ;; 提示使用者操作
  (princ "\n請用框選或點選方式選擇兩個文字物件 (TEXT 或 MTEXT) 來交換內容：")

  ;; 選取物件，並進行篩選
  ;; '((0 . "TEXT,MTEXT")) 表示只選取物件類型 (DXF code 0) 為 "TEXT" 或 "MTEXT" 的圖元
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))

  ;; 使用 cond 處理不同的選取情況
  (cond
    ;; 情況一：沒有選取到物件 (ss 為 nil) 或 選取的物件少於 2 個
    ( (or (not ss) (< (sslength ss) 2))
      (princ "\n錯誤：未選取物件或選取的文字物件少於兩個。請重新執行指令並選取兩個文字。")
    )

    ;; 情況二：剛好選取了 2 個物件
    ( (= (sslength ss) 2)
      ;; 從選擇集中取得兩個物件的圖元名稱 (Entity Name)
      (setq ent1 (ssname ss 0)) ; 第一個物件
      (setq ent2 (ssname ss 1)) ; 第二個物件

      ;; 取得兩個物件的 DXF 資料列表 (Entity Data List)
      ;; 將結果存入變數，避免重複呼叫 entget
      (setq edata1 (entget ent1))
      (setq edata2 (entget ent2))

      ;; 從 DXF 資料中找到代表文字內容的配對 (DXF code 1)
      (setq pair1 (assoc 1 edata1)) ; 尋找 (1 . "文字內容1")
      (setq pair2 (assoc 1 edata2)) ; 尋找 (1 . "文字內容2")

      ;; 檢查是否成功找到文字內容 (理論上篩選後應該都會有)
      (if (and pair1 pair2)
        (progn
          ;; 提取文字字串
          (setq txt1 (cdr pair1)) ; 取得 "文字內容1"
          (setq txt2 (cdr pair2)) ; 取得 "文字內容2"

          ;; 修改第一個物件：將第二個物件的文字替換進第一個物件的 DXF 資料中
          ;; (cons 1 txt2) 建立新的配對 (1 . "文字內容2")
          ;; subst 會用 (cons 1 txt2) 取代 edata1 中的 pair1
          (entmod (subst (cons 1 txt2) pair1 edata1))

          ;; 修改第二個物件：將第一個物件的文字替換進第二個物件的 DXF 資料中
          (entmod (subst (cons 1 txt1) pair2 edata2))

          ;; 更新物件在螢幕上的顯示 (雖然 entmod 常會觸發，但明確呼叫更保險)
          (entupd ent1)
          (entupd ent2)

          ;; 提示成功
          (princ "\n文字內容已成功交換。")
        )
        ;; 如果 assoc 沒找到 DXF code 1 (不太可能發生，但做個保護)
        (princ "\n錯誤：無法讀取所選物件的文字內容。")
      )
    )

    ;; 情況三：選取的物件超過 2 個 (cond 的預設情況 T)
    ( T
      (princ "\n錯誤：選取的文字物件超過兩個。請重新執行指令並只選取兩個文字。")
    )
  ) ; cond 結束

  ;; 清理命令列輸出，不顯示最後一個表達式回傳的值 (通常是 nil 或 最後一個 princ 的回傳值)
  (princ)
)


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

(defun sort-entities (entities sortOrder / primary dirX dirY)
  (setq primary (car sortOrder))
  (setq dirX (cadr sortOrder))
  (setq dirY (caddr sortOrder))

  (vl-sort entities
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

(defun TCO-sort-entities (entities sortOrder / primary dirX dirY)
  (setq primary (car sortOrder))
  (setq dirX (cadr sortOrder))
  (setq dirY (caddr sortOrder))

  (vl-sort entities
    (function
      (lambda (a b)
        (setq xa (car (car a)) ya (cadr (car a)))
        (setq xb (car (car b)) yb (cadr (car b)))
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

(defun ss-to-poslist (ss / lst idx)
  (setq lst '())
  (setq idx 0)
  (repeat (sslength ss)
    (setq lst (cons (cons (cdr (assoc 10 (entget (ssname ss idx)))) (ssname ss idx)) lst))
    (setq idx (1+ idx))
  )
  lst
)
