(defun c:CountBlocksOnPipe ( / ss ent blk-list line-list blk-pt line start end total-count )
  (vl-load-com)
  (setq ss (ssget)) ; 框選範圍

  (if ss
    (progn
      (setq blk-list '())
      (setq line-list '())

      ;; 分類圖塊跟線段
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq enttype (cdr (assoc 0 (entget ent))))
        (cond
          ((= enttype "INSERT") (setq blk-list (cons ent blk-list))) ; BLOCK
          ((or (= enttype "LINE") (= enttype "LWPOLYLINE")) (setq line-list (cons ent line-list))) ; LINE
        )
      )

      ;; 對每條主管線進行計算
      (setq pipe-index 1)
      (foreach line line-list
        (setq count 0)
        (setq vla-line (vlax-ename->vla-object line))

        ;; 跟每個圖塊比對
        (foreach blk blk-list
          (setq vla-blk (vlax-ename->vla-object blk))
          (setq blk-pt (vlax-get vla-blk 'InsertionPoint))

          ;; 判斷圖塊是否靠近主管線（用距離）
          (if (< (vlax-curve-getdistatpoint vla-line (vlax-curve-getclosestpointto vla-line blk-pt)) 5.0) ; 5.0單位內算碰到
            (setq count (1+ count))
          )
        )

        ;; 顯示結果
        (princ (strcat "\n主管 " (itoa pipe-index) " 碰到圖塊數量: " (itoa count)))
        (setq pipe-index (1+ pipe-index))
      )
    )
    (princ "\n沒有選到任何物件")
  )
  (princ)
)
