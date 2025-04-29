(defun c:COPYOFFSET ( / ent basept destpt offset currentpt nextpt )
  (vl-load-com)

  ;; 嘗試選擇物件
  (princ "\n請選擇要複製的物件：")
  (setq ent (car (entsel)))
  (if (not ent)
    (progn
      (princ "\n❌ 沒有選到物件，指令已取消。")
      (exit)
    )
  )

  ;; 選擇基準點
  (setq basept (getpoint "\n請選擇基準點："))
  (if (not basept)
    (progn
      (princ "\n❌ 沒有選到基準點，指令已取消。")
      (exit)
    )
  )

  ;; 選擇第一次目標點
  (setq destpt (getpoint basept "\n請選擇第一次目標點："))
  (if (not destpt)
    (progn
      (princ "\n❌ 沒有選到目標點，指令已取消。")
      (exit)
    )
  )

  ;; 計算偏移向量
  (setq offset (mapcar '- destpt basept))

  ;; 初次複製
  (command "_.COPY" ent "" "_non" basept "_non" destpt)

  ;; 從第一次開始連續偏移複製
  (setq currentpt destpt)
  (while (setq nextpt (getpoint currentpt "\n點擊繼續複製（Enter 結束）："))
    (setq currentpt (mapcar '+ currentpt offset))
    (command "_.COPY" ent "" "_non" basept "_non" currentpt)
  )

  (princ "\n✅ 已完成複製。")
  (princ)
)
