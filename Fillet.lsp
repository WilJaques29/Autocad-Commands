(defun C:FMP ; = Fillet Multiple Polylines
  (/ plss n)
  (if (setq plss (ssget "_:L" '((0 . "LWPOLYLINE"))))
    (repeat (setq n (sslength plss))
      (command "_.fillet" "_polyline" (ssname plss (setq n (1- n))))
    ); repeat
  ); if
  (princ)
); defun