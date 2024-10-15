;; innerloop.lsp -- code to generate inner loops from specs

;; the inner loop has a setup, a loop, and a cleanup
;; in the setup, structure fields used in the inner loop are
;;     copied or "cached" into register variables
;; in the inner loop, access expressions are substituted for
;;     variable names in the spec
;; in the cleanup, resulting register variable "cache" is copied
;;     back into the structure fields


(defun compute-inner-loop (alg inner-loop)
  (let ((interp (get-slot alg 'interpolation))
        (sound-names (get alg 'sound-names))
        (state-list (get-slot alg 'state))
        (step-function (get alg 'step-function))
        (maintain-list (get-slot alg 'maintain))
        (constant-list (get-slot alg 'constant))
        (force-into-register (get-slot alg 'force-into-register))
        (not-register (get-slot alg 'not-register))
        register-decl register-init register-cleanup new-state-list
       )

    ;; this loop computes and applies substitutions to the INNER-LOOP spec

    (setf inner-loop (substitute inner-loop "output" "*out_ptr_reg++" nil))
    (push "        out_ptr_reg = out_ptr;\n" register-init)
    (push "        out_ptr += togo;\n" register-cleanup)

    (dotimes (n (length interp))
      (let ((name (nth n sound-names))
            interpolate-samples
            (method (nth n interp))
            expression)
        (setf interpolate-samples
              (not (member (name-to-symbol name) step-function)))

        (cond ((eq method 'NONE)
                ;-----------------
                ; NONE:
                ;      <expr> ::= *NAME_ptr_reg++
                ;-----------------
               (pushnew (format nil
                "    register sample_block_values_type ~A_ptr_reg;~%" name)
                        register-decl)
               (pushnew (format nil 
                "        ~A_ptr_reg = susp->~A_ptr;~%" name name)
                        register-init)
               (pushnew (format nil
                "        /* using ~A_ptr_reg is a bad idea on RS/6000: */~
                ~%        susp->~A_ptr += togo;~%" name name name)
                        register-cleanup)

               (setf expression (format nil "*~A_ptr_reg++" name)))

              ((eq method 'SCALE)
                ;-----------------
                ; SCALE
                ;       <expr> ::= (NAME_scale_reg * *NAME_ptr_reg++)
                ;-----------------
               (pushnew (format nil
                "    register sample_block_values_type ~A_ptr_reg;~%" name)
                        register-decl)
               (pushnew (format nil
                "    register sample_type ~A_scale_reg = susp->~A->scale;~%"
                name name)
                        register-decl)
               (pushnew (format nil 
                         "        ~A_ptr_reg = susp->~A_ptr;~%" name name)
                        register-init)
               (pushnew (format nil
                "        /* using ~A_ptr_reg is a bad idea on RS/6000: */~
                ~%        susp->~A_ptr += togo;~%" name name name)
                        register-cleanup)
               (setf expression (format nil
                "(~A_scale_reg * *~A_ptr_reg++)" name name)))

              ((and interpolate-samples (eq method 'INTERP))
                ;-----------------
                ; INTERP:
                ;       <expr> ::= susp->NAME_x1_sample * (1 - 
                ;                       susp->NAME_pHaSe +
                ;                     susp->NAME_x2_sample * susp->NAME_pHaSe)
                ;-----------------
               (pushnew (format nil
                "    register sample_type ~A_x1_sample_reg;~%" name)
                        register-decl)
               (pushnew (format nil
                "    register double ~A_pHaSe_ReG;~%" name)
                        register-decl)
               (pushnew (format nil
                "    register double ~A_pHaSe_iNcR_rEg = susp->~A_pHaSe_iNcR;~%"
                name name)
                        register-decl)

               (pushnew (format nil
                "        ~A_x1_sample_reg = susp->~A_x1_sample;~%" name name)
                        register-init)
               (pushnew (format nil
                "        ~A_pHaSe_ReG = susp->~A_pHaSe;~%" name name)
                        register-init)

               (pushnew (format nil
                "        susp->~A_x1_sample = ~A_x1_sample_reg;~%" name name)
                        register-cleanup)
               (pushnew (format nil
                "        susp->~A_pHaSe = ~A_pHaSe_ReG;~%" name name)
                        register-cleanup)

               (setf expression (format nil 
                    "\n                (~A_x1_sample_reg * (1 - ~A_pHaSe_ReG) + ~A_x2_sample * ~A_pHaSe_ReG)"
                                 name name name name)))

              ((eq method 'INTERP)
                ;-----------------
                ; STEP FUNCTION:
                ;       <expr> ::= NAME_x1_sample_reg
                ;-----------------
               (pushnew (format nil
                "    register sample_type ~A_x1_sample_reg;~%" name)
                        register-decl)
               (pushnew (format nil
                "    register double ~A_pHaSe_ReG;~%" name)
                        register-decl)
               (pushnew (format nil
                "    register double ~A_pHaSe_iNcR_rEg = susp->~A_pHaSe_iNcR;~%"
                name name)
                        register-decl)

               (pushnew (format nil
                "        ~A_x1_sample_reg = susp->~A_x1_sample;~%" name name)
                        register-init)
               (pushnew (format nil
                "        ~A_pHaSe_ReG = susp->~A_pHaSe;~%" name name)
                        register-init)

               (pushnew (format nil
                "        susp->~A_x1_sample = ~A_x1_sample_reg;~%" name name)
                        register-cleanup)
               (pushnew (format nil
                "        susp->~A_pHaSe = ~A_pHaSe_ReG;~%" name name)
                        register-cleanup)

               (setf expression (format nil "~A_x1_sample_reg" name)))
              ((and interpolate-samples (eq method 'RAMP))
                ;-----------------
                ; RAMP:
                ;       <expr>  ::= NAME_val
                ;-----------------
               (setf expression (format nil "~A_val" name)))
              ((eq method 'RAMP)
                ;-----------------
                ; RAMP:
                ;       <expr>  ::= NAME_val
                ;-----------------
                ; this doesn't seem to be used -RBD 7/97
               ;(pushnew (format nil
                ;"    register sample_type ~A_x1_sample_reg;~%" name)
                ;register-decl)
               (setf expression (format nil "~A_val" name))))

        (setf inner-loop (substitute inner-loop name expression nil))
        ))

    ;; determine the members of state-list that are actually referenced in
    ;; the inner loop.  If not, don't cache the state in registers before
    ;; starting the loop.
    (dolist (state state-list)
      (let ((var-name (cadr state)))
        (cond ((and (or (string-search var-name inner-loop)
                        (member (name-to-symbol var-name) force-into-register))
                    (not (member (name-to-symbol var-name) not-register)))
               (push state new-state-list)))))

    ;; this loop applies substitutions for state variables:
    ;;   the specified state variable name is the cadr of state-list element
    ;;   the state variable <var> is replaced in inner-loop by <var>_reg

    (dolist (state new-state-list)
      (let ((var-name (cadr state))
            maintain)
        (pushnew (format nil "    register ~A ~A_reg;~%" (car state) var-name)
                 register-decl)
        (pushnew (format nil "        ~A_reg = susp->~A;~%" var-name var-name)
                 register-init)
        (setf maintain (find-maintain-stmt var-name maintain-list))
;        (display "find-maintain-stmt returned:" maintain)
        (cond (maintain
               (pushnew (format nil "        ~A;~%" maintain) register-cleanup))
              ((not (is-constant-in-inner-loop var-name constant-list))
               ;(pushnew (format nil "var-name: ~A constant-list: ~A~%" var-name constant-list)
               ;        register-cleanup)
               (pushnew (format nil
                         "        susp->~A = ~A_reg;~%" var-name var-name)
                        register-cleanup)))
        (setf inner-loop (substitute inner-loop var-name 
                          (format nil "~A_reg" var-name) t))
        ))
    
    ;(display "register decls" state-list register-decl) (read)

    ;; if the user-written code has a break statement or if the interpolation
    ;; type is INTERP, we need to write out "togo -= n;" to get an accurate
    ;; count of how many times we went through the loop.  Otherwise don't do it
    ;; because it makes n a live variable and affects compiler optimization.
    (cond ((or (member 'INTERP interp)
               (string-search "break" inner-loop))
           (push "        togo -= n;\n" register-cleanup)))

    (put-slot alg inner-loop 'inner-loop-stmts)
    (put-slot alg register-decl 'register-decl)
    (put-slot alg register-init 'register-init)
    (put-slot alg register-cleanup 'register-cleanup)

    ;-----------------
    ; WATCH:
    ;
    ; show_samples(1,s1,s1_ptr - s1->samples)
    ;
    ; Note: this is not right because we need to have the correct
    ; parameter for s1, but that is part of the s1_ptr++ computation
    ; so I don't know where to get it...
    ;-----------------
;    (if *WATCH*
;      (format stream "            show_samples(1,s1,s1_ptr - s1_samples);~%")
;    )
    ))


;; find-maintain-list -- find an assignment for variable in a MAINTAIN spec
;;
(defun find-maintain-stmt (var specs)
  (let ((spec (assoc var specs :test #'equal)))
    (if spec (cadr spec))))


;; is-constant-in-inner-loop -- see if var is in constant-list
;;
(defun is-constant-in-inner-loop (var constant-list)
  (member var constant-list :test #'equal))


;; pushnew -- pushes string onto list unless already there
;;
(defmacro pushnew (string var)
  `(if (not (member ,string ,var :test #'equal))
     (push ,string ,var)))


;;**********
;; substitute -- string substitution 
;; Inputs:
;;      s - input string
;;      pat - pattern
;;      repl - replacement for pattern
;;      all - T or NIL (T : replace everywhere; NIL : replace once)
;;
;;**********

(defun substitute (s pat repl all)
;  (display "substitute" s pat repl)
  (let ((p (position s pat))
        (l (length pat)))
    (cond (p
           (strcat (subseq s 0 p) repl 
            ;; the remainder of the string depends on all.  If T, then
            ;; use recursion to continue substitutions:
            (cond (all (substitute (subseq s (+ p l)) pat repl all))
                  (t (subseq s (+ p l))))))
          (t s))))


(defun write-inner-loop (alg stream)
  (let ((interp (get-slot alg 'interpolation))
        (step-function (get alg 'step-function))
        (sound-names (get alg 'sound-names))
       )

    (format stream "~A;~%"
     (get-slot alg 'inner-loop-stmts))

    (dotimes (n (length interp))
      (let ((name (nth n sound-names))
            interpolate-samples
            (method (nth n interp)))
        (setf interpolate-samples
              (not (member (name-to-symbol name) step-function)))

        (cond ((eq method 'INTERP)
                ;-----------------
                ; INTERP:
                ;
                ;    NAME_pHaSe_ReG += NAME_pHaSe_iNcR_rEg;
                ;-----------------
                (format stream "            ~A_pHaSe_ReG += ~A_pHaSe_iNcR_rEg;~%"
                     name name))

              ((and interpolate-samples (eq method 'RAMP))
                ;-----------------
                ; RAMP:
                ;       NAME_val += NAME_DeLtA
                ;-----------------
               (format stream "            ~A_val += ~A_DeLtA;~%" name name)))))

    ;----------------------------
    ; WATCH:
    ;       show_samples(0,out,out_ptr - 1 - out->samples);
    ;----------------------------

;    (if *WATCH*
;       (format stream "            show_samples(0,out,out_ptr - 1 - out->samples);~%"))
    ;----------------------------
    ;  } while (--n); /* inner loop */
    ;----------------------------
    (format stream "        } while (--n); /* inner loop */~%~%")))
