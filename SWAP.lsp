(defun c:SWAP ( / ss1 ss2 pt1 pt2 vec1 vec2 )
  (prompt "\\n選擇第一組物件：")
  (setq ss1 (ssget))
  (if ss1
    (progn
      (setq pt1 (getpoint "\\n指定第一組基準點（插入點）："))
      (if pt1
        (progn
          (prompt "\\n選擇第二組物件：")
          (setq ss2 (ssget))
          (if ss2
            (progn
              (setq pt2 (getpoint "\\n指定第二組基準點（插入點）："))
              (if pt2
                (progn
                  ;; 計算位移向量
                  (setq vec1 (mapcar '- pt2 pt1))
                  (setq vec2 (mapcar '- pt1 pt2))
                  ;; 執行交換
                  (move-selection ss1 vec1)
                  (move-selection ss2 vec2)
                  (prompt "\\n✅ 已依據基準點交換物件。")
                )
                (prompt "\\n❌ 未指定第二組基準點。")
              )
            )
            (prompt "\\n⚠️ 沒有選取第二組物件。")
          )
        )
        (prompt "\\n❌ 未指定第一組基準點。")
      )
    )
    (prompt "\\n⚠️ 沒有選取第一組物件。")
  )
  (princ)
)

(defun move-selection (ss vec / i ent)
  (if (and (listp vec) (= (length vec) 3) (apply 'and (mapcar 'numberp vec)))
    (progn
      (prompt (strcat "\\n🚚 平移向量為: " (vl-princ-to-string vec)))
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (vla-Move (vlax-ename->vla-object ent)
                  (vlax-3d-point '(0.0 0.0 0.0))
                  (vlax-3d-point (list (car vec) (cadr vec) (caddr vec))))
        (setq i (1+ i))
      )
    )
    (prompt "\\n❌ 無效的移動向量。")
  )
)
