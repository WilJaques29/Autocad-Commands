(defun c:InsertBlockAtBasePoint (/ selset obj point rotation angle1)
  (vl-load-com) ; Load the Visual LISP extension

  ;; Define the block name to insert
  (setq blockName "LC_ZONE_Tag")

  ;; Prompt user to select objects
  (setq selset (ssget))
  (if (not selset)
    (progn
      (princ "\nNo objects selected.")
      (exit)
    )
  )

  ;; Loop through selected objects
  (repeat (setq i (sslength selset))
    (setq obj (vlax-ename->vla-object (ssname selset (setq i (1- i)))))
    ;; Get the base point (insertion point) of the selected object
    (setq point (vlax-get obj 'InsertionPoint))
    ;; Get the rotation of the selected object and add 90 degrees
    (setq rotation (+ (vlax-get obj 'Rotation) (/ pi 2))) ; Add 90 degrees
    ;; Get the dynamic property "Angle1" of the selected object, if it exists
    (setq angle1 (vl-catch-all-apply 'vlax-get-property (list obj "Angle1")))

    ;; Insert the block at the base point with the adjusted rotation
    (setq insertedBlock
          (vlax-invoke
           (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-Acad-Object)))
           'InsertBlock
           point
           blockName
           1.0 1.0 1.0
           rotation))

    ;; Set the dynamic property "Angle1" for the inserted block, if applicable
    (if (and insertedBlock (not (vl-catch-all-error-p angle1)))
      (vlax-for prop (vlax-invoke insertedBlock 'GetDynamicBlockProperties)
        (if (eq (strcase (vlax-get prop 'PropertyName)) "Angle1")
          (vlax-put prop 'Value angle1)
        )
      )
    )
  )

  (princ (strcat "\nBlock '" blockName "' inserted with adjusted rotation and matching 'Angle1' property."))
  (princ)
)
