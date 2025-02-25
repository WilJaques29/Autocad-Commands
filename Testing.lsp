(defun c:Label ()
  ;; Prompt for initial letter and room number
  (setq firstLetter (getstring "\nEnter initial letter (K, D, S, B): "))
  (setq roomNumber (getstring "\nEnter initial room number: "))
  (setq secondLetter "A") ; Initial second letter

  ;; Set the current layer based on the initial letter
  (cond
    ((equal firstLetter "K") (command "_.layer" "set" "AV-LC-WALL-KEYPAD" ""))
    ((equal firstLetter "D") (command "_.layer" "set" "AV-LC-WALL-LOCAL DIMSW" ""))
    ((equal firstLetter "S") (command "_.layer" "set" "AV-SHADES" ""))
    ((equal firstLetter "B") (command "_.layer" "set" "AV-LC-WIRE" ""))
    (t (prompt "\nInvalid letter entered."))
  )

  (setq blkName "_LC Device ID Tage") ; Replace with your block name
  (setq incrementStep 1) ; Increment step for room number

  ;; Suppress the attribute prompt and dialog
  (setq oldAttReq (getvar 'ATTREQ))
  (setq oldAttDia (getvar 'ATTDIA))
  (setvar 'ATTREQ 0)
  (setvar 'ATTDIA 0)

  ;; Function to increment the second letter
  (defun IncrementSecondLetter (letter)
    (setq newLetter (chr (1+ (ascii letter))))
    (if (> (ascii newLetter) 90) ; If letter is greater than 'Z'
      (setq newLetter "A")
    )
    newLetter
  )

  ;; Function to create the ZONE attribute value
  (defun CreateZoneID (roomNumber firstLetter secondLetter)
    (strcat firstLetter "-" roomNumber "-" secondLetter)
  )

  ;; Function to place block with updated attributes
  (defun PlaceBlock (blkName pos roomNumber firstLetter secondLetter)
    (command "_.-insert" blkName pos "1" "1" "0") ; Insert block
    (setq blkID (entlast)) ; Get the last entity id
    (if blkID
      (progn
        ;; Get the attribute entity
        (setq ent (entnext blkID))
        (while ent
          (setq entity (entget ent))
          (if (and entity (eq (cdr (assoc 0 entity)) "ATTRIB"))
            (progn
              (setq tag (cdr (assoc 2 entity)))
              ;; Modify the ZONE attribute
              (if (equal tag "DEVICE_ID")
                (entmod (subst (cons 1 (CreateZoneID roomNumber firstLetter secondLetter)) (assoc 1 entity) entity))
              )
            )
          )
          (setq ent (entnext ent))
        )
      )
      (prompt "\nError: Block insertion failed.")
    )
  )

  ;; Main loop
  (while t
    (setq pt (getpoint "\nSpecify insertion point or press SPACE to increment room number: "))
    (if (not pt)
      (progn
        (setq roomNumber (itoa (+ (atoi roomNumber) incrementStep)))
        (setq secondLetter "A")
        (prompt (strcat "\nRoom number incremented to: " roomNumber))
      )
      (progn
        (PlaceBlock blkName pt roomNumber firstLetter secondLetter)
        (setq secondLetter (IncrementSecondLetter secondLetter))
      )
    )
  )

  ;; Restore the original system variable settings
  (setvar 'ATTREQ oldAttReq)
  (setvar 'ATTDIA oldAttDia)
)
