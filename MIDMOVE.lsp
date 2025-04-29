(defun c:MIDMOVE ( / *doc* selbase basept pt1 pt2 midpoint)

  (vl-load-com)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark *doc*) ;✅ 靜音開始 UNDO 群組

  ;; 使用者操作
  (setq selbase (ssget))
  (if selbase
    (progn
      (setq basept (getpoint "\n請點選插入點（基準點）: "))
      (setq pt1 (getpoint "\n請點選第一點: "))
      (setq pt2 (getpoint "\n請點選第二點: "))

      (if (and basept pt1 pt2)
        (progn
          (setq midpoint (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))
          (vl-cmdf "_.MOVE" selbase "" "_NON" basept "_NON" midpoint)
        )
        (princ "\n請確認所有點位都正確。")
      )
    )
    (princ "\n未選取任何物件。")
  )

  (vla-EndUndoMark *doc*) ;✅ 結束 UNDO 群組
  (princ)
)
