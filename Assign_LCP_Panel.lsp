(defun c:AssignLCP ()

;; Paste the text extracted from excel right under this comment 

;; DO NOT TOUCH ANYTHING ABOVE
;;  ------------------------------------------------------------------

  (setq lcp01 '("101A" "102B" "102A" "103A" "103E" "103D" "103B" "103C" "104A" "105A" "105B" "106A" "109C" "109A" "109B" "111C" "112E" "114A" "114B" "601A" "601B" "601C" "602C" "602A" "602B" "603C" "603A" "603B"))
  (setq lcp02 '("107A" "107B" "111A" "111D" "111B" "111E" "112A" "112B" "112F" "112C" "112D" "115A" "115C" "115B" "116B" "116A-1" "116A-2" "116C" "117A" "118B" "118A" "118B" "129A"))
  (setq lcp03 '())
  (setq lcp04 '())
  (setq lcp05 '())
  (setq lcp06 '())













;;  ------------------------------------------------------------------
;; DO NOT TOUCH ANYTHING BELOW



 

  (setq lcpGroups (list (cons "LCP-01" lcp01) (cons "LCP-02" lcp02) (cons "LCP-03" lcp03) (cons "LCP-04" lcp04) (cons "LCP-05" lcp05) (cons "LCP-06" lcp06) (cons "LCP-07" lcp07) (cons "LCP-08" lcp08) (cons "LCP-09" lcp09)(cons "LCP-A01" lcpA01)(cons "LCP-A02" lcpb01)))
  (setq blockNames '("LC_ZONE_Tag")) ; List of block names to search
  (setq foundWords '()) ; Initialize list for found words

  ;; Select all block references 
  (setq ss (ssget "X" '((0 . "INSERT"))))
  
  (if ss
    (progn
      ;; Iterate through each LCP group
      (foreach lcpGroup lcpGroups
        (setq groupName (car lcpGroup))
        (setq words (cdr lcpGroup))
        (setq i 0)
        (setq ss-new (ssadd))
        
        ;; Iterate through the selection set
        (while (< i (sslength ss))
          (setq blk (ssname ss i))
          (setq blk-data (entget blk))
          
          ;; Check if block name is in the list
          (if (member (cdr (assoc 2 blk-data)) blockNames)
            (progn
              (setq blk-attributes (entnext blk))
              ;; Iterate through block attributes
              (while (and blk-attributes (/= (cdr (assoc 0 (entget blk-attributes))) "SEQEND"))
                (if (and (eq (cdr (assoc 0 (entget blk-attributes))) "ATTRIB")
                         (member (cdr (assoc 1 (entget blk-attributes))) words))
                  (progn
                    (ssadd blk ss-new)
                    (setq foundWords (cons (cdr (assoc 1 (entget blk-attributes))) foundWords)) ; Add to found words
                  )
                )
                (setq blk-attributes (entnext blk-attributes))
              )
            )
          )
          (setq i (1+ i))
        )

        (setq notFoundWords (vl-remove-if '(lambda (x) (member x foundWords)) words)) ; Get words not found
        
        (if (> (sslength ss-new) 0)
          (progn
            (sssetfirst nil ss-new)
            (princ (strcat "\nSelected " (itoa (sslength ss-new)) " block references containing the specified attribute values for " groupName "."))
            ;; Change the PNL attribute to the current LCP group for all selected blocks
            (setq i 0)
            (while (< i (sslength ss-new))
              (setq blk (ssname ss-new i))
              (setq blk-attributes (entnext blk))
              ;; Iterate through block attributes to update PNL
              (while (and blk-attributes (/= (cdr (assoc 0 (entget blk-attributes))) "SEQEND"))
                (setq att (entget blk-attributes))
                (if (and (eq (cdr (assoc 0 att)) "ATTRIB")
                         (equal (strcase (cdr (assoc 2 att))) "PNL"))
                  (progn
                    (setq att (subst (cons 1 groupName) (assoc 1 att) att)) ; Assign LCP group value to PNL attribute
                    (entmod att)
                  )
                )
                (setq blk-attributes (entnext blk-attributes))
              )
              (setq i (1+ i))
            )
          )
          (princ (strcat "\nNo block references found containing the specified attribute values for " groupName "."))
        )
        (if notFoundWords
          (princ (strcat "\nWords not found for " groupName ": " (apply 'strcat (mapcar '(lambda (x) (strcat x " ")) notFoundWords))))
        )
      )
    )
  )
  (princ)
)
