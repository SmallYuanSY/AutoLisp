(defun c:GTABLE ( / ent obj id)
  (vl-load-com)
  (setq ent (car (entsel "\n請選擇一個表格: ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq id (vla-get-ObjectID obj))
      (princ (strcat "\n表格的 Object ID 為: " (itoa id)))
    )
    (princ "\n沒有選到物件。")
  )
  (princ)
)
