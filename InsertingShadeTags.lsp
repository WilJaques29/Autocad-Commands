(defun c:ShadeTags (/ selset obj point rotation visibilityValue insertedBlock dynamicProps propName)
  (vl-load-com) ; Load the Visual LISP extension

  ;; Define the block name to insert
  (setq blockName "_Shade Tag Rotate")

  ;; Prompt user to select objects
  (setq selset (ssget '((0 . "INSERT")))) ; Filter for blocks only
  (if (not selset)
    (progn
      (princ "\nNo blocks selected.")
      (exit)
    )
  )

  ;; Loop through selected objects
  (repeat (setq i (sslength selset))
    (setq obj (vlax-ename->vla-object (ssname selset (setq i (1- i)))))
    ;; Get the base point (insertion point) of the selected object
    (setq point (vlax-get obj 'InsertionPoint))
    ;; Get the rotation of the selected object in degrees
    (setq rotation (* (vlax-get obj 'Rotation) (/ 180.0 pi))) ; Convert radians to degrees
    (princ (strcat "\nSelected block rotation: " (rtos rotation 2 2)))

    ;; Determine the visibility value based on the rotation angle
    (if (and (>= rotation 0) (< rotation 45))
      (setq visibilityValue "Down")
      (if (and (>= rotation 45) (<= rotation 90))
        (setq visibilityValue "Left")
        (progn
          (princ "\nRotation angle out of expected range (0° to 90°). Skipping block.")
          (setq visibilityValue nil)
        )
      )
    )

    ;; Insert the block at the base point without rotation
    (if visibilityValue
      (progn
        (setq insertedBlock
              (vlax-invoke
               (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-Acad-Object)))
               'InsertBlock
               point
               blockName
               1.0 1.0 1.0
               0.0)) ; No rotation applied during insertion

        ;; Handle dynamic properties for "Visibility1"
        (if (vlax-method-applicable-p insertedBlock 'GetDynamicBlockProperties)
          (progn
            (setq dynamicProps (vlax-invoke insertedBlock 'GetDynamicBlockProperties))
            (vlax-for prop dynamicProps
              (setq propName (vlax-get prop 'PropertyName))
              (if (eq (strcase propName) "VISIBILITY1")
                (progn
                  (princ (strcat "\n    Found 'Visibility1', setting its value to: " visibilityValue))
                  (vlax-put prop 'Value visibilityValue) ; Set the value of "Visibility1"
                )
              )
            )
          )
          (princ "\n  No dynamic properties found for inserted block.")
        )

        (princ (strcat "\nBlock inserted at " (rtos (car point) 2 2) ", " (rtos (cadr point) 2 2) " with Visibility1 set to: " visibilityValue))
      )
    )
  )

  (princ "\nAll blocks processed.")
  (princ)
)
