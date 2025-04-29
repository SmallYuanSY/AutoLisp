(defun c:MOD-U ( / ent obj coords rightX newLen topY botY newWidth dx dy ptlist i pt newpts)
  (vl-load-com)
  ;; 只選擇 LWPOLYLINE
  (setq ent (car (entsel "\n請選擇 U 型 polyline：")))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq coords (vlax-safearray->list (vlax-variant-value (vla-get-coordinates obj))))

      ;; 拆成點組
      (setq ptlist '())
      (setq i 0)
      (while (< i (length coords))
        (setq pt (list (nth i coords) (nth (+ i 1) coords)))
        (setq ptlist (cons pt ptlist))
        (setq i (+ i 2))
      )
      (setq ptlist (reverse ptlist))

      ;; 找出最大 X（右邊）、最大/最小 Y（上下）
      (setq rightX (apply 'max (mapcar 'car ptlist)))
      (setq topY (apply 'max (mapcar 'cadr ptlist)))
      (setq botY (apply 'min (mapcar 'cadr ptlist)))

      ;; 輸入目標長度與寬度
      (setq newLen (getreal "\n請輸入新的右側直線長度: "))
      (setq newWidth (getreal "\n請輸入新的上下距離（整體寬度）: "))

      ;; 計算差值
      (setq dx (- newLen (- rightX (apply 'min (mapcar 'car ptlist)))))
      (setq dy (/ (- newWidth (- topY botY)) 2.0))

      ;; 調整每個點
      (setq newpts
        (mapcar
          (function
            (lambda (p)
              (list
                (+ (car p) (if (= (car p) rightX) dx 0))
                (+ (cadr p)
                   (cond
                     ((= (cadr p) topY) dy)
                     ((= (cadr p) botY) (- dy))
                     (t 0)
                   )
                )
              )
            )
          )
          ptlist
        )
      )

      ;; 寫入新的座標
      (vla-put-coordinates obj
        (vlax-make-variant
          (vlax-safearray-fill
            (vlax-make-safearray vlax-vbDouble (cons 0 (- (* 2 (length newpts)) 1)))
            (apply 'append newpts)
          )
        )
      )
      (princ "\n✅ 已修改成功。")
    )
    (princ "\n⚠️ 請選擇一條 LWPOLYLINE。")
  )
  (princ)
)
