;;;  TOZERO  —  Move selected objects so their chosen base-point ends up at (0,0,0)
(defun c:TOZERO ( / ss base )
  (vl-load-com)

  ;; ① 讓使用者選取要搬家的物件
  (setq ss (ssget "_:L" '((0 . "*"))))   ; 任意物件
  (if ss
    (progn
      ;; ② 指定基準點（要對準 0,0 的那一點）
      (setq base (getpoint "\n指定基準點 (將移到 0,0)： "))
      (if base
        (progn
          ;; ③ 執行 MOVE：從 base → 0,0
          (command "_.MOVE" ss "" "_non" base "_non" '(0 0 0))
          (princ "\n✅ 物件已移到原點 (0,0)。")
        )
        (princ "\n⚠️ 取消，未指定基準點。")
      )
    )
    (princ "\n⚠️ 取消，未選取任何物件。")
  )
  (princ)
)
