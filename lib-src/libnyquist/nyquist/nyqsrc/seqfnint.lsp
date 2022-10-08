
    (setfn seq-tag first)
    (setfn seq-time second)
    (setfn seq-line third)
    (setfn seq-channel fourth)
    (defun seq-value1 (e) (nth 4 e))
    (defun seq-value2 (e) (nth 5 e))
    (setfn seq-pitch seq-value1) ; pitch of a note
    (setfn seq-control seq-value1) ; control number of a control change
    (setfn seq-program seq-value2) ; program number of a program change
    (setfn seq-bend seq-value2) ; pitch bend amount
    (setfn seq-touch seq-value2) ; aftertouch amount
    (setfn seq-velocity seq-value2) ; velocity of a note
    (setfn seq-value seq-value2) ; value of a control change
    (defun seq-duration (e) (nth 6 e))
    

 (setf seq-done-tag 0) 

 (setf seq-other-tag 1) 

 (setf seq-note-tag 2) 

 (setf seq-ctrl-tag 11) 

 (setf seq-prgm-tag 12) 

 (setf seq-cpress-tag 13) 

 (setf seq-bend-tag 14) 

