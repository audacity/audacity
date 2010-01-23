
; profile.lsp -- support for profiling

;## show-profile -- print profile data
(defun show-profile ()
  (let ((profile-flag (profile nil)) (total 0))
    (dolist (name *PROFILE*)
            (setq total (+ total (get name '*PROFILE*))))
    (dolist (name *PROFILE*)
            (format t "~A (~A%): ~A~%"
                    (get name '*PROFILE*)
                    (truncate
                     (+ 0.5 (/ (float (* 100 (get name '*PROFILE*)))
                               total)))
                    name))
    (format t "Total: ~A~%" total)
    (profile profile-flag)))


;## start-profile -- clear old profile data and start profiling
(defun start-profile ()
  (profile nil)
  (dolist (name *PROFILE*)
          (remprop name '*PROFILE*))
  (setq *PROFILE* nil)
  (profile t))

