(defun c:UpdateAngle1 (/ selset obj rotation dynamicProps propName)
  (vl-load-com) ; Load the Visual LISP extension

  ;; Prompt user to select blocks
  (setq selset (ssget '((0 . "INSERT")))) ; Filter for blocks
  (if (not selset)
    (progn
      (princ "\nNo blocks selected.")
      (exit)
    )
  )

  ;; Start debugging
  (princ "\nStarting UpdateAngle1 process...")

  ;; Loop through selected blocks
  (repeat (setq i (sslength selset))
    (setq obj (vlax-ename->vla-object (ssname selset (setq i (1- i)))))
    (princ (strcat "\nProcessing block: " (vlax-get obj 'EffectiveName)))

    ;; Get the current rotation of the block (in radians)
    (setq rotation (vlax-get obj 'Rotation))
    ;; Convert rotation from radians to degrees
    (setq rotationDegrees (* rotation (/ 180.0 pi)))
    (princ (strcat "\n  Current rotation (radians): " (rtos rotation 2 6)))
    (princ (strcat "\n  Current rotation (degrees): " (rtos rotationDegrees 2 2)))

    ;; Access dynamic properties of the block
    (if (vlax-method-applicable-p obj 'GetDynamicBlockProperties)
      (progn
        (setq dynamicProps (vlax-invoke obj 'GetDynamicBlockProperties))
        (princ "\n  Dynamic properties found:")
        ;; Iterate through dynamic properties
        (vlax-for prop dynamicProps
          (setq propName (vlax-get prop 'PropertyName))
          (princ (strcat "\n    Property name: " propName))
          ;; Check if the property is "Angle1"
          (if (eq (strcase propName) "ANGLE1")
            (progn
              (princ (strcat "\n    Found 'Angle1', setting its value to: " (rtos rotationDegrees 2 2)))
              (vlax-put prop 'Value rotationDegrees) ; Set "Angle1" to the block's current rotation in degrees
            )
          )
        )
      )
      (princ "\n  GetDynamicBlockProperties not supported for this block.")
    )

    ;; Reset the block's rotation to 0
    (vlax-put obj 'Rotation 0.0)
    (princ "\n  Block rotation reset to 0.")
  )

  (princ "\nAll selected blocks updated: 'Angle1' set to current rotation, and rotation reset to 0.")
  (princ)
)
