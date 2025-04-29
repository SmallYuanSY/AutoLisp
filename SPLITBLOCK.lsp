(defun c:SPLITBLOCK ( / blk ent name insPt minPt maxPt plObj pts ss tempName)
  (vl-load-com)

  ;; 選取圖塊或外參
  (setq ent (car (entsel "\n請選取一個外部參考或圖塊：")))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn
      (setq blk (vlax-ename->vla-object ent))
      (setq name (vla-get-Name blk))
      (setq insPt (vlax-get blk 'InsertionPoint))

      ;; 取得 Bounding Box
      (vla-GetBoundingBox blk 'minPt 'maxPt)
      (setq minPt (vlax-safearray->list minPt))
      (setq maxPt (vlax-safearray->list maxPt))

      ;; 用這個範圍建立矩形 PL
      (setq pts (list
                  (list (car minPt) (cadr minPt))
                  (list (car maxPt) (cadr minPt))
                  (list (car maxPt) (cadr maxPt))
                  (list (car minPt) (cadr maxPt))
                  (list (car minPt) (cadr minPt))))
      (setq plObj (entmakex (list
                             (cons 0 "LWPOLYLINE")
                             (cons 100 "AcDbEntity")
                             (cons 100 "AcDbPolyline")
                             (cons 90 (length pts))
                             (cons 70 1) ; closed
                             ))
      )
      ;; 加頂點
      (foreach pt pts
        (entmod (append (entget plObj) (list (cons 10 pt))))
      )
      (entupd plObj)

      ;; 用 PL 範圍做 WP 擷取
      (setq ss (ssget "_WP" pts))
      (if ss
        (progn
          (setq tempName (strcat (getenv "TEMP") "\\" name "_" (rtos (getvar "DATE") 2 0) ".dwg"))
          (command "_.WBLOCK" tempName "" insPt ss "")
          (princ (strcat "\n✅ 已成功輸出至：" tempName))
        )
        (princ "\n⚠️ 找不到範圍內的物件")
      )

      ;; 刪掉臨時 PL
      (if plObj (entdel plObj))
    )
    (princ "\n請選一個圖塊或外部參考")
  )
  (princ)
)
