;;; FINDNET_FINAL.LSP
;;; 功能：從選定的線段開始，自動尋找所有相連的 LINE / LWPOLYLINE
;;; 支援支線貼在線段中點的情況，不需端點重合，並依據主管線長度動態判斷搜尋範圍

(defun c:FINDNET ( / ent pt allLines connLines tol ss i entObj length minPt maxPt buffer minSA maxSA )

  (vl-load-com)
  (setq tol 0.5) ; 容差設定，可視圖比例調整

  ;; 選擇起始線段
  (setq ent (car (entsel "\n請選取起始線段：")))
  (if (not ent)
    (progn (princ "\n未選取任何物件。") (exit))
  )
  (setq entObj (vlax-ename->vla-object ent))

  ;; 根據主管線長度決定搜尋區域
  (setq length (vlax-curve-getDistAtParam entObj (vlax-curve-getEndParam entObj)))
  (setq minSA (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (setq maxSA (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vla-GetBoundingBox entObj minSA maxSA) ; 傳變數，非 symbol！
  (setq minPt (vlax-safearray->list minSA))
  (setq maxPt (vlax-safearray->list maxSA))
  (setq buffer (* length 1.5)) ; 動態擴張範圍
  (setq minPt (list (- (car minPt) buffer) (- (cadr minPt) buffer)))
  (setq maxPt (list (+ (car maxPt) buffer) (+ (cadr maxPt) buffer)))

  ;; 取得範圍內的線段（LINE / LWPOLYLINE）
  (setq allLines '())
  (if (setq ss (ssget "_C" minPt maxPt '((0 . "LINE,LWPOLYLINE"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq allLines (cons (vlax-ename->vla-object (ssname ss i)) allLines))
        (setq i (1+ i))
      )
    )
  )

  ;; 判斷兩條線是否接觸（不限端點）
  (defun is-touching (a b tol / int)
    (setq int (vlax-invoke a 'IntersectWith b acExtendNone))
    (and int (> (length int) 0))
  )

  ;; 判斷是否已存在於清單（以 Handle 為主）
  (defun in-list-by-handle (obj lst / result h1 h2)
    (setq h1 (vla-get-Handle obj))
    (setq result nil)
    (foreach x lst
      (setq h2 (vla-get-Handle x))
      (if (= h1 h2) (setq result T))
    )
    result
  )

  ;; 遞迴搜尋
  (defun find-connected (current connLines / other)
    (if (not (in-list-by-handle current connLines))
      (progn
        (setq connLines (cons current connLines))
        (foreach other allLines
          (if (and (not (in-list-by-handle other connLines))
                   (is-touching current other tol))
            (setq connLines (find-connected other connLines))
          )
        )
      )
    )
    connLines
  )

  ;; 執行搜尋
  (setq connLines (find-connected entObj '()))

  ;; Highlight 所有線段
  (foreach obj connLines
    (vla-highlight obj :vlax-true)
  )

  (princ (strcat "\n共找到 " (itoa (length connLines)) " 條相連線段。"))
  (princ)
)