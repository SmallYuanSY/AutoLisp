(defun c:CHECKCROSS ( / entMain objMain ssOther i entOther objOther interPt crossList)
  (vl-load-com)
  (setq entMain (car (entsel "\n請選擇主線：")))
  (setq objMain (vlax-ename->vla-object entMain))

  ;; 選取其他線段
  (setq ssOther (ssget '((0 . "LINE,LWPOLYLINE"))))
  (setq crossList '())

  (repeat (setq i (sslength ssOther))
    (setq entOther (ssname ssOther (setq i (1- i))))
    (if (/= entOther entMain)
      (progn
        (setq objOther (vlax-ename->vla-object entOther))
        (setq interPt (vl-catch-all-apply
                        (function (lambda ()
                                    (vlax-invoke objMain 'IntersectWith objOther acExtendNone)))))
        (if (and (not (vl-catch-all-error-p interPt))
                 (> (length interPt) 0))
          (setq crossList (cons entOther crossList))
        )
      )
    )
  )

  (princ (strcat "\n穿過 " (itoa (length crossList)) " 個線段"))
  (foreach ent crossList
    (redraw ent 3)
  )

  (princ)
)
