(defun write-csv (filename data)
  (setq f (open filename "w"))
  (foreach row data
    (write-line (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) row)) f)
  )
  (close f)
)

(defun run-python-script (script args)
  (startapp "python" (append (list script) args))
)

(defun C:RoomCounter ( / i elist at cmde cen rad p1 impl ss polylist blockdata blkattributes filename csvdata roomindex drawingpath csvfilename xlsxfilename)
  (setq cmde (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq i 0 
        polylist nil
        ss (ssget '((0 . "LWPOLYLINE,CIRCLE"))) ; Get all polylines and circles
        csvdata (list (list "Room" "Attribute Tag" "Attribute Value"))
        roomindex 1
        drawingpath (getvar "DWGPREFIX") ; Get the current drawing path
        csvfilename (strcat drawingpath "Autocad_Fixture_Report.csv") ; Set the CSV filename to be in the same directory as the drawing
        xlsxfilename (strcat drawingpath "Autocad_Fixture_Report(1).xlsx") ; Set the XLSX filename to be in the same directory as the drawing
  )
  (if ss
    (progn
      (repeat (sslength ss)
        (setq elist (entget (ssname ss i)))
        (setq i (1+ i))
        (setq polylist (cons elist polylist))
      )
    )
  )
  (foreach elist polylist
    (if (= (cdr (assoc 0 elist)) "CIRCLE")
      (progn
        (setq cen (cdr (assoc 10 elist))
              rad (cdr (assoc 40 elist)) 
        )
        (setq ss (ssget "WP" (mapcar '(lambda (ang) (polar cen ang rad)) (list 0 (* pi 0.5) pi (* pi 1.5)))))
      )
      (progn
        (setq ss (ssget "WP" (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) elist))))
      )
    )
    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq blockdata (entget (ssname ss i)))
          (if (= (cdr (assoc 0 blockdata)) "INSERT")
            (progn
              (setq blkattributes (entnext (cdr (assoc -1 blockdata))))
              (while (and blkattributes (= (cdr (assoc 0 (entget blkattributes))) "ATTRIB"))
                (setq tag (cdr (assoc 2 (entget blkattributes))))
                (setq value (cdr (assoc 1 (entget blkattributes))))
                (princ (strcat "\n" (itoa roomindex) " - Attribute Tag: " tag " - Attribute Value: " value))
                (setq csvdata (append csvdata (list (list (strcat "Room " (itoa roomindex)) tag value))))
                (setq blkattributes (entnext blkattributes))
              )
            )
          )
          (setq i (1+ i))
        )
        (setq roomindex (1+ roomindex))
      )
    )
  )
  
  (write-csv csvfilename csvdata)
  (run-python-script "convert_csv_to_xlsx.py" (list csvfilename xlsxfilename))
  (setvar "cmdecho" cmde)
  (princ (strcat "\nData has been written to " xlsxfilename))
  (princ)
)

(princ "\nType RoomCounter to run the command.")
(princ)
