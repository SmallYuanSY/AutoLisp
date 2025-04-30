(defun c:ROB ( / blkRef ptBase name previewEnts wasdAngles wasdMap pt i angKey angDeg pt1 pt2 ss ename data insPt scaleX scaleY angRad lastCmd)
  (vl-load-com)

  ;; 1. 選擇參考圖塊
  (princ "\n請選擇要旋轉方向的圖塊參考:")
  (setq blkRef (car (entsel)))
  (if (not blkRef)
    (progn (princ "\n沒有選取圖塊，指令結束。") (exit))
  )

  (setq name (cdr (assoc 2 (entget blkRef))))
  (setq insPt (cdr (assoc 10 (entget blkRef))))
  (setq scaleX (cdr (assoc 41 (entget blkRef))))
  (setq scaleY (cdr (assoc 42 (entget blkRef))))
  (setq previewEnts '())

  ;; 2. 預覽四個方向圖塊
  (setq wasdMap '(("W" . 0.0) ("D" . 90.0) ("S" . 180.0) ("A" . 270.0)))
  (setq spacing 300)
  (setq ptPre (getpoint "\n請指定預覽圖塊的基準點: "))
  (setq wasdAngles
    (list
        (list "W" (list (car ptPre) (+ (cadr ptPre) spacing)))
        (list "A" (list (- (car ptPre) spacing) (cadr ptPre)))
        (list "S" (list (car ptPre) (- (cadr ptPre) spacing)))
        (list "D" (list (+ (car ptPre) spacing) (cadr ptPre)))
    )
  )
  (foreach item wasdAngles
    (setq angKey (car item))
    (setq ptBase (cadr item))
    (setq angDeg (cdr (assoc angKey wasdMap)))
    (command "_.-INSERT" name ptBase scaleX scaleY angDeg)
    (setq newEnt (entlast))
    ;; 套用原始圖塊圖層
    (entmod (subst (assoc 8 (entget blkRef)) (assoc 8 (entget newEnt)) (entget newEnt)))
    (entupd newEnt)
    (setq previewEnts (cons newEnt previewEnts))
  )

  ;; 3. 框選範圍
  (vl-catch-all-apply
    (function
      (lambda ()
        (setq pt1 (getpoint "\n請指定框選範圍的第一個點（左上角）: "))
        (setq pt2 (getcorner pt1 "\n請指定框選範圍的第二個點（右下角）: "))
        (setq ss (ssget "W" pt1 pt2 '((0 . "INSERT"))))
        (if (not ss)
          (progn (princ "\n沒有選到圖塊，指令結束。") (exit))
        )

        ;; 4. 處理每個圖塊
        (setq i (sslength ss))
        (setq lastCmd "W")
        (while (> i 0)
          (setq i (1- i))
          (setq ename (ssname ss i))
          (setq data (entget ename))
          (if (= (cdr (assoc 2 data)) name)
            (vl-catch-all-apply
              (function
                (lambda ()
                  (setq insPt (cdr (assoc 10 data)))
                  (redraw ename 3)
                  (command "_.ZOOM" "_C" insPt 100)

                  (setq keepGoing T)
                  (while keepGoing
                    (initget "W A S D RO Z E")
                    (setq angKey (getkword (strcat "\n請輸入方向鍵 [上(W)/ 下(S)/ 左(A)/ 右(D)/ 手動旋轉(RO)/ 回到圖塊(Z)/ 結束(E)] <" lastCmd ">: ")))
                    (if (not angKey) (setq angKey lastCmd))
                    (setq lastCmd angKey)

                    (cond
                      ((= angKey "E") (progn (setq i 0) (setq keepGoing nil)))
                      ((= angKey "Z") (command "_.ZOOM" "_C" insPt 100))
                      ((= angKey "RO")
                        (progn
                          (setq angRad (getangle insPt "\n請輸入旋轉角度 (rad): "))
                          (setq data (subst (cons 50 angRad) (assoc 50 data) data))
                          (entmod data)
                          (entupd ename)
                          (setq keepGoing nil)
                        )
                      )
                      (T
                        (setq angDeg (cdr (assoc angKey wasdMap)))
                        (setq angRad (* angDeg (/ pi 180.0)))
                        (setq data (subst (cons 50 angRad) (assoc 50 data) data))
                        (entmod data)
                        (entupd ename)
                        (setq keepGoing nil)
                      )
                    )
                  )
                  (redraw ename 4)
                )
              )
            )
          )
        )
      )
    )
  )

  ;; 5. 刪除預覽圖塊
  (foreach e previewEnts (entdel e))
  (command "_.REGEN")
  (princ "\n完成旋轉設定。")
  (princ)
)
