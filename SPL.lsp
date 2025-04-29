(defun c:SPL ( / ss i ent ed total)
  (setq total 0.0)
  (setq ss (ssget '((0 . "LWPOLYLINE")))) 

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (setq total (+ total (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent))))
        (setq i (1+ i))
      )
      (prompt (strcat "\n總長度為: " (rtos total 2 6)))
    )
    (prompt "\n沒有選到任何多段線。")
  )
  (princ)
)
