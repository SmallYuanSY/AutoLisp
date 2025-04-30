(defun c:FHB ( / pt1 pt2 ss i ename )
  (vl-load-com)

  (princ "\n啟動 FHB，按 ESC 或 Enter 結束。")

  (while t
    (setq pt1 (getpoint "\n請指定第一個點（左上角），或 Enter 結束: "))
    (if (null pt1) (progn (princ "\n結束 FHB。") (exit)))

    (setq pt2 (getcorner pt1 "\n請指定第二個點（右下角）: "))
    (if (null pt2) (progn (princ "\n未指定第二點，結束。") (exit)))

    (setq ss (ssget "W" pt1 pt2))

    (if ss
      (progn
        (repeat (setq i (sslength ss))
          (setq i (1- i))
          (setq ename (ssname ss i))
          (if ename
            (vl-catch-all-apply
              (function (lambda () (redraw ename 2)))
            )
          )
        )
        (princ (strcat "\n成功隱藏 " (itoa (sslength ss)) " 個物件。"))
      )
      (princ "\n這次沒有選到物件。")
    )
  )

  (princ)
)
