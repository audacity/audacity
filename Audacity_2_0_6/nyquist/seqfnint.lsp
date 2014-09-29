
        (setfn seq-tag first)
        (setfn seq-time second)
        (setfn seq-line third)
        (setfn seq-channel fourth)
        (defun seq-value1 (e) (nth 4 e))
        (setfn seq-pitch seq-value1) ; pitch of a note
        (setfn seq-control seq-value1) ; control number of a control change
        (setfn seq-program seq-value1) ; program number of a program change
        (setfn seq-bend seq-value1) ; pitch bend amount
        (setfn seq-touch seq-value1) ; aftertouch amount
        (defun seq-value2 (e) (nth 5 e))
        (setfn seq-velocity seq-value2) ; velocity of a note
        (setfn seq-value seq-value2) ; value of a control change
        (defun seq-duration (e) (nth 6 e))
        

 (setf seq-done-tag 0) 

 (setf seq-other-tag 1) 

 (setf seq-note-tag 2) 

 (setf seq-ctrl-tag 3) 

 (setf seq-prgm-tag 4) 

 (setf seq-touch-tag 5) 

 (setf seq-bend-tag 6) 

