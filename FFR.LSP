(defun c:FFR ()
  (setq old_osmode (getvar "osmode"))

  (vl-catch-all-apply
    (function
      (lambda ()
        (setvar "regenmode" 0)
        (setvar "osmode" 0)

        (if (not blk-multiplier)
          (progn
            (setq blk-multiplier (getreal "\n請輸入圖塊數量倍率（預設為 1）: "))
            (if (null blk-multiplier) (setq blk-multiplier 1.0))
          )
        )

        (setq xy nil)
        (if (tblsearch "STYLE" "simchin")
          (princ "\n已有 simchin 樣式。")
          (command "style" "simchin" "simplex,chineset" "30" "" "" "" "" "")
        )

        (setq Pt1 (getpoint "\n框選的左上角: "))
        (setq Pt2 (getcorner Pt1 "\n框選的右下角: "))

        ;; 初次選擇圖塊（可按 Enter 跳出）
        (setq lnam nil)
        (setq finished nil)
        (while (not finished)
          (setq sel (ssget "+.:E:S"))
          (cond
            ((null sel)
             (setq finished T)) ; Enter/空白跳出選擇流程
            ((= (sslength sel) 1)
             (setq temp (ssname sel 0))
             (if (= (cdr (assoc 0 (entget temp))) "INSERT")
               (progn
                 (setq lnam temp)
                 (setq finished T))
               (princ "\n請選擇一個圖塊（INSERT）物件。")
             )
            )
            (T (princ "\n請只選擇一個圖塊。"))
          )
        )

        (while lnam
          (setq nam (entget lnam))
          (setq name (cdr (assoc 2 nam)))
          (setq namx (abs (cdr (assoc 41 nam))))
          (setq namy (abs (cdr (assoc 42 nam))))

          (command "zoom" "w" pt1 pt2)
          (setq ssx (ssget "c" Pt2 Pt1 (list (cons 0 "INSERT") (cons 2 name))))

          (setq i 0)
          (if ssx
            (while (< i (sslength ssx))
              (setq era (entget (ssname ssx i)))
              (redraw (cdr (car era)) 2)
              (setq i (1+ i))
            )
          )

          (if (= nil xy)
            (setq xy (getpoint "請選擇一個點: "))
            (print "NIL")
          )

          (setq count (* i blk-multiplier))
          (setq xy1 (list (+ 100 (car xy)) (+ 0 (cadr xy))))
          (command "insert" name xy namx namy "")
          (command "text" "m" xy1 "0" count)

          ;; Draw border lines around block
          (setq xy2 (list (+ -50 (car xy)) (+ 40 (cadr xy))))
          (setq xy3 (list (+ 50 (car xy)) (+ 40 (cadr xy))))
          (setq xy4 (list (+ 150 (car xy)) (+ 40 (cadr xy))))
          (setq xy5 (list (+ 150 (car xy)) (+ -40 (cadr xy))))
          (setq xy6 (list (+ 50 (car xy)) (+ -40 (cadr xy))))
          (setq xy7 (list (+ -50 (car xy)) (+ -40 (cadr xy))))

          (command "line" xy2 xy3 xy6 xy7 "c")
          (command "line" xy3 xy4 xy5 xy6 "")

          (if (/= nil xy)
            (setq xy (list (car xy) (+ -80 (cadr xy))))
          )

          ;; 建立或更新清單中的塊名稱與編號
          (setq blksel (assoc name blklist))
          (if (= blksel nil)
            (progn
              (setq blksel (list name count))
              (setq blklist (cons blksel blklist))
            )
            (progn
              (setq blksize (car blksel))
              (setq blktol (car (cdr blksel)))
              (setq count (+ count blktol))
              (setq newblk (list name count))
              (setq blklist (subst newblk blksel blklist))
            )
          )

          (command "zoom" "p")

          ;; 下一輪選擇（可按 Enter 跳出）
          (setq lnam nil)
          (setq finished nil)
          (while (not finished)
            (setq sel (ssget "+.:E:S" '((0 . "INSERT"))))
            (cond
              ((null sel) (setq finished T))
              ((= (sslength sel) 1)
               (setq temp (ssname sel 0))
               (if (= (cdr (assoc 0 (entget temp))) "INSERT")
                 (progn
                   (setq lnam temp)
                   (setq finished T))
                 (princ "\n請選擇一個圖塊（INSERT）物件。")
               )
              )
              (T (princ "\n請只選擇一個圖塊。"))
            )
          )
        )

        (initget "Yes No")
        (setq aqq (getkword "\n是否要寫出累計的圖塊數量 Yes/No:<N>"))

        (if (= aqq "Yes")
          (progn
            (setq xy (getpoint "請選擇一個點: "))
            (setq j 0)
            (repeat (length blklist)
              (setq blksel (nth j blklist))
              (setq name (car blksel))
              (setq count (car (cdr blksel)))
              (setq xy1 (list (+ 100 (car xy)) (+ 0 (cadr xy))))
              (command "insert" name xy namx namy "")
              (command "text" "m" xy1 "0" count)

              (setq xy2 (list (+ -50 (car xy)) (+ 40 (cadr xy))))
              (setq xy3 (list (+ 50 (car xy)) (+ 40 (cadr xy))))
              (setq xy4 (list (+ 150 (car xy)) (+ 40 (cadr xy))))
              (setq xy5 (list (+ 150 (car xy)) (+ -40 (cadr xy))))
              (setq xy6 (list (+ 50 (car xy)) (+ -40 (cadr xy))))
              (setq xy7 (list (+ -50 (car xy)) (+ -40 (cadr xy))))

              (command "line" xy2 xy3 xy6 xy7 "c")
              (command "line" xy3 xy4 xy5 xy6 "")

              (setq xy (list (car xy) (+ -80 (cadr xy))))
              (setq j (+ j 1))
            )
          )
        )
      )
    )
  )

  (setvar "osmode" old_osmode)
  (setq blk-multiplier nil)
  (princ)
)
