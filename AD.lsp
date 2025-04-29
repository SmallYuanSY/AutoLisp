(setq *adi-total* 0.0)
(setq *adi-pls* nil)

(defun ensure-adi-layer ()
  (if (not (tblsearch "LAYER" "ADI"))
    (command "-layer" "M" "ADI" "C" "7" "" "")
  )
)

(defun c:AD ( / pt1 pt2 dist plEnt)
  (vl-catch-all-apply
    (function
      (lambda ()
        (ensure-adi-layer)
        (if (not *adi-pls*) (setq *adi-pls* '()))
        (setq pt1 (getpoint "\n[ADI] 請點選起點（Enter 結束）："))
        (while pt1
          (setq pt2 (getpoint pt1 "\n請點選下一點（Enter 結束）："))
          (if pt2
            (progn
              ;; 加總距離
              (setq dist (distance pt1 pt2))
              (setq *adi-total* (+ *adi-total* dist))

              ;; 設定圖層與畫線
              (setvar "clayer" "ADI")
              (command "_.PLINE" pt1 pt2 "")
              (setq plEnt (entlast))

              ;; 儲存 pline 物件
              (setq *adi-pls* (cons plEnt *adi-pls*))

              ;; 顯示狀態
              (princ (strcat "\n段距離：" (rtos dist 2 3)))
              (princ (strcat "\n目前總距離：" (rtos *adi-total* 2 3)))

              ;; 下一個起點
              (setq pt1 pt2)
            )
            (progn
              (princ (strcat "\n[ADI] 最終總距離：" (rtos *adi-total* 2 3)))
              (setq pt1 nil)
            )
          )
        )
      )
    )
  )
  (princ)
)

(defun c:CD ( / pl )
  (vl-catch-all-apply
    (function
      (lambda ()
        (if *adi-pls*
          (progn
            (foreach pl *adi-pls*
              (if (and pl (entget pl)) (entdel pl))
            )
            (princ "\n[CDI] 已刪除所有 ADI Polyline。")
          )
        )
        (setq *adi-total* 0.0)
        (setq *adi-pls* nil)
        (princ "\n[CDI] 總距離已清空。")
      )
    )
  )
  (princ)
)
