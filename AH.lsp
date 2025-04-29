(defun c:AH ( / hatchEnt hatchData pattern scale angle patternType ss i
              plObj hatchObj loops acad doc ms saMin saMax)
  (vl-load-com)

  (prompt "\n請選擇一個作為範本的 HATCH...")
  (setq hatchEnt (car (entsel "\n選取一個 hatch: ")))
  (if (and hatchEnt (= (cdr (assoc 0 (entget hatchEnt))) "HATCH"))
    (progn
      (setq hatchData   (entget hatchEnt))
      (setq pattern     (cdr (assoc 2 hatchData)))
      (setq scale       (cdr (assoc 41 hatchData)))
      (setq angle       (cdr (assoc 52 hatchData)))
      (setq patternType (cdr (assoc 76 hatchData)))

      (prompt "\n請框選封閉 polyline（LWPOLYLINE 70=1）...")
      (setq ss (ssget '((0 . "LWPOLYLINE") (70 . 1))))

      (if ss
        (progn
          (setq acad (vlax-get-acad-object))
          (setq doc  (vla-get-ActiveDocument acad))
          (setq ms   (vla-get-ModelSpace doc))
          (setq i 0)

          (while (< i (sslength ss))
            (setq plObj (vlax-ename->vla-object (ssname ss i)))

            ;; 建立 hatch
            (setq hatchObj (vla-AddHatch ms patternType pattern :vlax-true))
            (vla-put-PatternScale hatchObj scale)
            (vla-put-PatternAngle hatchObj angle)
            (vla-put-Layer hatchObj (vla-get-Layer plObj))

            ;; 加入邊界
            (setq loops (vlax-make-safearray vlax-vbObject '(0 . 0)))
            (vlax-safearray-put-element loops 0 plObj)
            (vla-AppendOuterLoop hatchObj loops)

            (vla-Evaluate hatchObj)

            (setq i (1+ i))
          )
          (prompt (strcat "\n✅ 完成建立 " (itoa i) " 個 HATCH！"))
        )
        (prompt "\n⚠️ 沒有選取封閉 polyline。")
      )
    )
    (prompt "\n⚠️ 選到的不是 HATCH 物件。")
  )
  (princ)
)
