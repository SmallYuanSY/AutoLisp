(defun c:SATSF ( / acadDoc styleDict styleObj styleName)
  (vl-load-com)
  (setq acadDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq styleDict (vla-get-TextStyles acadDoc))

  (vlax-for styleObj styleDict
    (setq styleName (strcase (vla-get-Name styleObj))) ; 取得樣式名稱轉大寫

    ;; 如果樣式名稱是 "標楷體" 就跳過
    (if (not (wcmatch styleName "*標楷體*")) ; 可改成 "=標楷體" 完全比對
      (progn
        (vla-put-FontFile styleObj "simplex.shx")
        (vla-put-BigFontFile styleObj "genie.shx")
      )
    )
  )
  (princ "\n✅ 已完成，樣式名稱為「標楷體」的未修改，其餘已統一字型。")
  (princ)
)
