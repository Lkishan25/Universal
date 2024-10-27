(defun convert-to-current-unit (value current-unit)
  (cond
    ((vl-string-search "mm" value) (atof (vl-string-subst "" "mm" value))) ; Already in mm
    ((vl-string-search "cm" value) (* (atof (vl-string-subst "" "cm" value)) 10)) ; Convert cm to mm
    ((vl-string-search "m" value) (* (atof (vl-string-subst "" "m" value)) 1000)) ; Convert meters to mm
    ((vl-string-search "\"" value) (* (atof (vl-string-subst "" "\"" value)) 25.4)) ; Convert inches to mm
    ((vl-string-search "'" value) (* (atof (vl-string-subst "" "'" value)) 304.8)) ; Convert feet to mm
    (T (atof value)) ; If no unit, assume it's already in mm
  )
)

(defun convert-from-mm (value current-unit)
  (cond
    ((equal current-unit "mm") value) ; Already in mm
    ((equal current-unit "inches") (/ value 25.4)) ; Convert mm to inches
    ((equal current-unit "feet") (/ value 304.8)) ; Convert mm to feet
    (T value) ; Default to mm if unit is not recognized
  )
)

(defun get-current-unit ()
  (cond
    ((= (getvar "INSUNITS") 1) "inches")
    ((= (getvar "INSUNITS") 2) "feet")
    (T "mm") ; Default to mm
  )
)

(defun format-to-precision (value)
  (rtos value 2 8) ; Convert to string with 8 decimal places
)

(defun c:UL ()
  (setq current-unit (get-current-unit))
  (setq pt1 (getpoint "\nSpecify first point: "))
  (command "._LINE" pt1)
  (while (setq pt2 (getstring T "\nSpecify next point or [Undo]: "))
    (if (or (vl-string-search "mm" pt2) (vl-string-search "cm" pt2) (vl-string-search "m" pt2) (vl-string-search "\"" pt2) (vl-string-search "'" pt2))
      (setq pt2 (convert-from-mm (convert-to-current-unit pt2 current-unit) current-unit))
    )
    (command pt2)
  )
  (command "")
)

(defun c:UO ()
  (setq current-unit (get-current-unit))
  (setq dist (getstring "\nSpecify offset distance (e.g., 3.59m or 2' or 1000mm): "))
  (setq dist-mm (convert-to-current-unit dist current-unit))
  (setq dist-final (convert-from-mm dist-mm current-unit))
  (command "OFFSET" (atof (format-to-precision dist-final)))
)



You Welcome! - Kishan Lakhani