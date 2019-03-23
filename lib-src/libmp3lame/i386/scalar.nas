;
;    (C) Frank Klemm 1995,99,2000
;    Dedicated to the LAME project
;
;
        %include "nasm.h"

        segment_code
        
; float_t  scalar04_float32_i387 ( 
;         const float32_t* const  p, 
;         const float32_t* const  q );

proc    scalar04_float32_i387
%$p     arg     4
%$q     arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        fld     dword [eax]
        fmul    dword [edx]
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0    
endproc


proc    scalar08_float32_i387
%$p     arg     4
%$q     arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        fld     dword [eax]
        fmul    dword [edx]
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0    
        fld     dword [eax + 16]
        fmul    dword [edx + 16]
        faddp   st1,st0    
        fld     dword [eax + 20]
        fmul    dword [edx + 20]
        faddp   st1,st0    
        fld     dword [eax + 24]
        fmul    dword [edx + 24]
        faddp   st1,st0    
        fld     dword [eax + 28]
        fmul    dword [edx + 28]
        faddp   st1,st0    
endproc


proc    scalar12_float32_i387
%$p     arg     4
%$q     arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        fld     dword [eax]
        fmul    dword [edx]
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0    
        fld     dword [eax + 16]
        fmul    dword [edx + 16]
        faddp   st1,st0    
        fld     dword [eax + 20]
        fmul    dword [edx + 20]
        faddp   st1,st0    
        fld     dword [eax + 24]
        fmul    dword [edx + 24]
        faddp   st1,st0    
        fld     dword [eax + 28]
        fmul    dword [edx + 28]
        faddp   st1,st0    
        fld     dword [eax + 32]
        fmul    dword [edx + 32]
        faddp   st1,st0    
        fld     dword [eax + 36]
        fmul    dword [edx + 36]
        faddp   st1,st0    
        fld     dword [eax + 40]
        fmul    dword [edx + 40]
        faddp   st1,st0    
        fld     dword [eax + 44]
        fmul    dword [edx + 44]
        faddp   st1,st0    
endproc


proc    scalar16_float32_i387
%$p     arg     4
%$q     arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        fld     dword [eax]
        fmul    dword [edx]
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0    
        fld     dword [eax + 16]
        fmul    dword [edx + 16]
        faddp   st1,st0    
        fld     dword [eax + 20]
        fmul    dword [edx + 20]
        faddp   st1,st0    
        fld     dword [eax + 24]
        fmul    dword [edx + 24]
        faddp   st1,st0    
        fld     dword [eax + 28]
        fmul    dword [edx + 28]
        faddp   st1,st0    
        fld     dword [eax + 32]
        fmul    dword [edx + 32]
        faddp   st1,st0    
        fld     dword [eax + 36]
        fmul    dword [edx + 36]
        faddp   st1,st0    
        fld     dword [eax + 40]
        fmul    dword [edx + 40]
        faddp   st1,st0    
        fld     dword [eax + 44]
        fmul    dword [edx + 44]
        faddp   st1,st0    
        fld     dword [eax + 48]
        fmul    dword [edx + 48]
        faddp   st1,st0    
        fld     dword [eax + 52]
        fmul    dword [edx + 52]
        faddp   st1,st0    
        fld     dword [eax + 56]
        fmul    dword [edx + 56]
        faddp   st1,st0    
        fld     dword [eax + 60]
        fmul    dword [edx + 60]
        faddp   st1,st0    
endproc


proc    scalar20_float32_i387
%$p     arg     4
%$q     arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        fld     dword [eax]
        fmul    dword [edx]
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0    
        fld     dword [eax + 16]
        fmul    dword [edx + 16]
        faddp   st1,st0    
        fld     dword [eax + 20]
        fmul    dword [edx + 20]
        faddp   st1,st0    
        fld     dword [eax + 24]
        fmul    dword [edx + 24]
        faddp   st1,st0    
        fld     dword [eax + 28]
        fmul    dword [edx + 28]
        faddp   st1,st0    
        fld     dword [eax + 32]
        fmul    dword [edx + 32]
        faddp   st1,st0    
        fld     dword [eax + 36]
        fmul    dword [edx + 36]
        faddp   st1,st0    
        fld     dword [eax + 40]
        fmul    dword [edx + 40]
        faddp   st1,st0    
        fld     dword [eax + 44]
        fmul    dword [edx + 44]
        faddp   st1,st0    
        fld     dword [eax + 48]
        fmul    dword [edx + 48]
        faddp   st1,st0    
        fld     dword [eax + 52]
        fmul    dword [edx + 52]
        faddp   st1,st0    
        fld     dword [eax + 56]
        fmul    dword [edx + 56]
        faddp   st1,st0    
        fld     dword [eax + 60]
        fmul    dword [edx + 60]
        faddp   st1,st0    
        fld     dword [eax + 64]
        fmul    dword [edx + 64]
        faddp   st1,st0    
        fld     dword [eax + 68]
        fmul    dword [edx + 68]
        faddp   st1,st0    
        fld     dword [eax + 72]
        fmul    dword [edx + 72]
        faddp   st1,st0    
        fld     dword [eax + 76]
        fmul    dword [edx + 76]
        faddp   st1,st0    
endproc


proc    scalar24_float32_i387
%$p     arg     4
%$q     arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        fld     dword [eax]
        fmul    dword [edx]
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0    
        fld     dword [eax + 16]
        fmul    dword [edx + 16]
        faddp   st1,st0    
        fld     dword [eax + 20]
        fmul    dword [edx + 20]
        faddp   st1,st0    
        fld     dword [eax + 24]
        fmul    dword [edx + 24]
        faddp   st1,st0    
        fld     dword [eax + 28]
        fmul    dword [edx + 28]
        faddp   st1,st0    
        fld     dword [eax + 32]
        fmul    dword [edx + 32]
        faddp   st1,st0    
        fld     dword [eax + 36]
        fmul    dword [edx + 36]
        faddp   st1,st0    
        fld     dword [eax + 40]
        fmul    dword [edx + 40]
        faddp   st1,st0    
        fld     dword [eax + 44]
        fmul    dword [edx + 44]
        faddp   st1,st0    
        fld     dword [eax + 48]
        fmul    dword [edx + 48]
        faddp   st1,st0    
        fld     dword [eax + 52]
        fmul    dword [edx + 52]
        faddp   st1,st0    
        fld     dword [eax + 56]
        fmul    dword [edx + 56]
        faddp   st1,st0    
        fld     dword [eax + 60]
        fmul    dword [edx + 60]
        faddp   st1,st0    
        fld     dword [eax + 64]
        fmul    dword [edx + 64]
        faddp   st1,st0    
        fld     dword [eax + 68]
        fmul    dword [edx + 68]
        faddp   st1,st0    
        fld     dword [eax + 72]
        fmul    dword [edx + 72]
        faddp   st1,st0    
        fld     dword [eax + 76]
        fmul    dword [edx + 76]
        faddp   st1,st0    
        fld     dword [eax + 80]
        fmul    dword [edx + 80]
        faddp   st1,st0    
        fld     dword [eax + 84]
        fmul    dword [edx + 84]
        faddp   st1,st0    
        fld     dword [eax + 88]
        fmul    dword [edx + 88]
        faddp   st1,st0    
        fld     dword [eax + 92]
        fmul    dword [edx + 92]
        faddp   st1,st0    
endproc


proc    scalar32_float32_i387
%$p     arg     4
%$q     arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        fld     dword [eax]
        fmul    dword [edx]
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0    
        fld     dword [eax + 16]
        fmul    dword [edx + 16]
        faddp   st1,st0    
        fld     dword [eax + 20]
        fmul    dword [edx + 20]
        faddp   st1,st0    
        fld     dword [eax + 24]
        fmul    dword [edx + 24]
        faddp   st1,st0    
        fld     dword [eax + 28]
        fmul    dword [edx + 28]
        faddp   st1,st0    
        fld     dword [eax + 32]
        fmul    dword [edx + 32]
        faddp   st1,st0    
        fld     dword [eax + 36]
        fmul    dword [edx + 36]
        faddp   st1,st0    
        fld     dword [eax + 40]
        fmul    dword [edx + 40]
        faddp   st1,st0    
        fld     dword [eax + 44]
        fmul    dword [edx + 44]
        faddp   st1,st0    
        fld     dword [eax + 48]
        fmul    dword [edx + 48]
        faddp   st1,st0    
        fld     dword [eax + 52]
        fmul    dword [edx + 52]
        faddp   st1,st0    
        fld     dword [eax + 56]
        fmul    dword [edx + 56]
        faddp   st1,st0    
        fld     dword [eax + 60]
        fmul    dword [edx + 60]
        faddp   st1,st0    
        fld     dword [eax + 64]
        fmul    dword [edx + 64]
        faddp   st1,st0    
        fld     dword [eax + 68]
        fmul    dword [edx + 68]
        faddp   st1,st0    
        fld     dword [eax + 72]
        fmul    dword [edx + 72]
        faddp   st1,st0    
        fld     dword [eax + 76]
        fmul    dword [edx + 76]
        faddp   st1,st0    
        fld     dword [eax + 80]
        fmul    dword [edx + 80]
        faddp   st1,st0    
        fld     dword [eax + 84]
        fmul    dword [edx + 84]
        faddp   st1,st0    
        fld     dword [eax + 88]
        fmul    dword [edx + 88]
        faddp   st1,st0    
        fld     dword [eax + 92]
        fmul    dword [edx + 92]
        faddp   st1,st0    
        fld     dword [eax + 96]
        fmul    dword [edx + 96]
        faddp   st1,st0    
        fld     dword [eax +100]
        fmul    dword [edx +100]
        faddp   st1,st0    
        fld     dword [eax +104]
        fmul    dword [edx +104]
        faddp   st1,st0    
        fld     dword [eax +108]
        fmul    dword [edx +108]
        faddp   st1,st0    
        fld     dword [eax +112]
        fmul    dword [edx +112]
        faddp   st1,st0    
        fld     dword [eax +116]
        fmul    dword [edx +116]
        faddp   st1,st0    
        fld     dword [eax +120]
        fmul    dword [edx +120]
        faddp   st1,st0    
        fld     dword [eax +124]
        fmul    dword [edx +124]
        faddp   st1,st0    
endproc


; float_t  scalar4n_float32_i387 ( 
;         const float32_t* const  p, 
;         const float32_t* const  q,
;         const size_t            len );

proc    scalar4n_float32_i387
%$p     arg     4
%$q     arg     4
%$len   arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        mov     ecx,[sp(%$len)]
        fld     dword [eax]
        fmul    dword [edx]
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0
        dec     ecx
        jz      .ret1
        add     eax,byte 16
        add     edx,byte 16
.lbl1
        fld     dword [eax]
        fmul    dword [edx]
        faddp   st1,st0
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0
        add     eax,byte 16
        add     edx,byte 16
        dec     ecx
        jnz     .lbl1
.ret1   
endproc


; float_t  scalar1n_float32_i387 ( 
;         const float32_t* const  p, 
;         const float32_t* const  q,
;         const size_t            len );

proc    scalar1n_float32_i387
%$p     arg     4
%$q     arg     4
%$len   arg     4
;;;     alloc

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        mov     ecx,[sp(%$len)]
        fld0
        shr     ecx,1
        jnc     .lbl2
        fld     dword [eax]
        fmul    dword [edx]
        faddp   st1,st0
        add     eax,byte 4
        add     edx,byte 4
.lbl2
        shr     ecx,1
        jnc     .lbl3
        fld     dword [eax]
        fmul    dword [edx]
        faddp   st1,st0
        fld     dword [eax + 4]
        fmul    dword [edx + 4]
        faddp   st1,st0
        add     eax,byte 8
        add     edx,byte 8
        and     ecx,ecx
.lbl3
        jz      .ret2
.lbl4
        fld     dword [eax]
        fmul    dword [edx]
        faddp   st1,st0
        fld     dword [eax +  4]
        fmul    dword [edx +  4]
        faddp   st1,st0
        fld     dword [eax +  8]
        fmul    dword [edx +  8]
        faddp   st1,st0
        fld     dword [eax + 12]
        fmul    dword [edx + 12]
        faddp   st1,st0
        add     eax,byte 16
        add     edx,byte 16
        dec     ecx
        jnz     .lbl4
.ret2
endproc


proc    scalar04_float32_3DNow
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        pmov    mm0,qword [eax]
        pmov    mm1,qword [eax+8]
        pfmul   mm0,qword [edx]
        pfmul   mm1,qword [edx+8]

        pfadd   mm0,mm1
        pmov    qword [sp(%$p)],mm0
        femms
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc


proc    scalar08_float32_3DNow
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        pmov    mm0,qword [eax]
        pmov    mm1,qword [eax+8]
        pfmul   mm0,qword [edx]
        pfmul   mm1,qword [edx+8]

        pmov    mm2,qword [eax+16]
        pmov    mm3,qword [eax+24]
        pfmul   mm2,qword [edx+16]
        pfmul   mm3,qword [edx+24]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pfadd   mm0,mm1
        pmov    qword [sp(%$p)],mm0
        femms
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc


proc    scalar12_float32_3DNow
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        pmov    mm0,qword [eax]
        pmov    mm1,qword [eax+8]
        pfmul   mm0,qword [edx]
        pfmul   mm1,qword [edx+8]

        pmov    mm2,qword [eax+16]
        pmov    mm3,qword [eax+24]
        pfmul   mm2,qword [edx+16]
        pfmul   mm3,qword [edx+24]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+32]
        pmov    mm3,qword [eax+40]
        pfmul   mm2,qword [edx+32]
        pfmul   mm3,qword [edx+40]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pfadd   mm0,mm1
        pmov    qword [sp(%$p)],mm0
        femms
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc


proc    scalar16_float32_3DNow
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        pmov    mm0,qword [eax]
        pmov    mm1,qword [eax+8]
        pfmul   mm0,qword [edx]
        pfmul   mm1,qword [edx+8]

        pmov    mm2,qword [eax+16]
        pmov    mm3,qword [eax+24]
        pfmul   mm2,qword [edx+16]
        pfmul   mm3,qword [edx+24]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+32]
        pmov    mm3,qword [eax+40]
        pfmul   mm2,qword [edx+32]
        pfmul   mm3,qword [edx+40]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+48]
        pmov    mm3,qword [eax+56]
        pfmul   mm2,qword [edx+48]
        pfmul   mm3,qword [edx+56]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pfadd   mm0,mm1
        pmov    qword [sp(%$p)],mm0
        femms
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc


proc    scalar20_float32_3DNow
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        pmov    mm0,qword [eax]
        pmov    mm1,qword [eax+8]
        pfmul   mm0,qword [edx]
        pfmul   mm1,qword [edx+8]

        pmov    mm2,qword [eax+16]
        pmov    mm3,qword [eax+24]
        pfmul   mm2,qword [edx+16]
        pfmul   mm3,qword [edx+24]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+32]
        pmov    mm3,qword [eax+40]
        pfmul   mm2,qword [edx+32]
        pfmul   mm3,qword [edx+40]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+48]
        pmov    mm3,qword [eax+56]
        pfmul   mm2,qword [edx+48]
        pfmul   mm3,qword [edx+56]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+64]
        pmov    mm3,qword [eax+72]
        pfmul   mm2,qword [edx+64]
        pfmul   mm3,qword [edx+72]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pfadd   mm0,mm1
        pmov    qword [sp(%$p)],mm0
        femms
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc


proc    scalar24_float32_3DNow
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        pmov    mm0,qword [eax]
        pmov    mm1,qword [eax+8]
        pfmul   mm0,qword [edx]
        pfmul   mm1,qword [edx+8]

        pmov    mm2,qword [eax+16]
        pmov    mm3,qword [eax+24]
        pfmul   mm2,qword [edx+16]
        pfmul   mm3,qword [edx+24]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+32]
        pmov    mm3,qword [eax+40]
        pfmul   mm2,qword [edx+32]
        pfmul   mm3,qword [edx+40]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+48]
        pmov    mm3,qword [eax+56]
        pfmul   mm2,qword [edx+48]
        pfmul   mm3,qword [edx+56]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+64]
        pmov    mm3,qword [eax+72]
        pfmul   mm2,qword [edx+64]
        pfmul   mm3,qword [edx+72]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+80]
        pmov    mm3,qword [eax+88]
        pfmul   mm2,qword [edx+80]
        pfmul   mm3,qword [edx+88]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pfadd   mm0,mm1
        pmov    qword [sp(%$p)],mm0
        femms
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc

proc    scalar32_float32_3DNow
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        pmov    mm0,qword [eax]
        pmov    mm1,qword [eax+8]
        pfmul   mm0,qword [edx]
        pfmul   mm1,qword [edx+8]

        pmov    mm2,qword [eax+16]
        pmov    mm3,qword [eax+24]
        pfmul   mm2,qword [edx+16]
        pfmul   mm3,qword [edx+24]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+32]
        pmov    mm3,qword [eax+40]
        pfmul   mm2,qword [edx+32]
        pfmul   mm3,qword [edx+40]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+48]
        pmov    mm3,qword [eax+56]
        pfmul   mm2,qword [edx+48]
        pfmul   mm3,qword [edx+56]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+64]
        pmov    mm3,qword [eax+72]
        pfmul   mm2,qword [edx+64]
        pfmul   mm3,qword [edx+72]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+80]
        pmov    mm3,qword [eax+88]
        pfmul   mm2,qword [edx+80]
        pfmul   mm3,qword [edx+88]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+96]
        pmov    mm3,qword [eax+104]
        pfmul   mm2,qword [edx+96]
        pfmul   mm3,qword [edx+104]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pmov    mm2,qword [eax+112]
        pmov    mm3,qword [eax+120]
        pfmul   mm2,qword [edx+112]
        pfmul   mm3,qword [edx+120]
        pfadd   mm0,mm2
        pfadd   mm1,mm3

        pfadd   mm0,mm1
        pmov    qword [sp(%$p)],mm0
        femms
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc


proc    scalar4n_float32_3DNow
%$p     arg     4
%$q     arg     4
%$len   arg     4

        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]
        mov     ecx,[sp(%$len)]

        pmov    mm0,qword [eax]
        pmov    mm1,qword [eax+8]
        pfmul   mm0,qword [edx]
        pfmul   mm1,qword [edx+8]
        dec     ecx
        jz      .ret4
        
        add     eax,byte 16
        add     edx,byte 16
.lbl4:  
        pmov    mm2,qword [eax]
        pmov    mm3,qword [eax+8]
        pfmul   mm2,qword [edx]
        pfmul   mm3,qword [edx+8]
        add     eax,byte 16
        add     edx,byte 16
        pfadd   mm0,mm2
        pfadd   mm1,mm3
        dec     ecx
        jnz     .lbl4

.ret4:  pfadd   mm0,mm1
        pmov    qword [sp(%$p)],mm0
        femms
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc


proc    scalar1n_float32_3DNow
        jmp     scalar24_float32_i387
endproc


proc    scalar04_float32_SIMD
        jmp     scalar04_float32_i387
endproc


proc    scalar08_float32_SIMD
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        movups  xmm0, [eax]
        movups  xmm1, [eax+16]
        mulps   xmm0, [edx]
        mulps   xmm1, [edx+16]

        addps   xmm0,xmm1
        sub     esp,16
        movups  [esp],xmm0
        fld     dword [esp+ 0]
        fadd    dword [esp+ 4]
        fadd    dword [esp+ 8]
        fadd    dword [esp+12]
        add     esp,16
endproc


proc    scalar12_float32_SIMD
        jmp     scalar12_float32_i387
endproc


proc    scalar16_float32_SIMD
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        movups  xmm0, [eax]
        movups  xmm1, [eax+16]
        mulps   xmm0, [edx]
        mulps   xmm1, [edx+16]

        movups  xmm2, [eax+32]
        movups  xmm3, [eax+48]
        mulps   xmm2, [edx+32]
        mulps   xmm3, [edx+48]
        addps   xmm0,xmm2
        addps   xmm1,xmm3

        addps   xmm0,xmm1
        sub     esp,16
        movups  [esp],xmm0
        fld     dword [esp+ 0]
        fadd    dword [esp+ 4]
        fadd    dword [esp+ 8]
        fadd    dword [esp+12]
        add     esp,16
endproc


proc    scalar20_float32_SIMD
        jmp     scalar20_float32_i387
endproc


proc    scalar24_float32_SIMD
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        movups  xmm0, [eax]
        movups  xmm1, [eax+16]
        mulps   xmm0, [edx]
        mulps   xmm1, [edx+16]

        movups  xmm2, [eax+32]
        movups  xmm3, [eax+48]
        mulps   xmm2, [edx+32]
        mulps   xmm3, [edx+48]
        addps   xmm0,xmm2
        addps   xmm1,xmm3

        movups  xmm2, [eax+64]
        movups  xmm3, [eax+80]
        mulps   xmm2, [edx+64]
        mulps   xmm3, [edx+80]
        addps   xmm0,xmm2
        addps   xmm1,xmm3

        addps   xmm0,xmm1
        sub     esp,16
        movups  [esp],xmm0
        fld     dword [esp+ 0]
        fadd    dword [esp+ 4]
        fadd    dword [esp+ 8]
        fadd    dword [esp+12]
        add     esp,16
endproc


proc    scalar32_float32_SIMD
%$p     arg     4
%$q     arg     4
        mov     eax,[sp(%$p)]
        mov     edx,[sp(%$q)]

        movups  xmm0, [eax]
        movups  xmm1, [eax+16]
        mulps   xmm0, [edx]
        mulps   xmm1, [edx+16]

        movups  xmm2, [eax+32]
        movups  xmm3, [eax+48]
        mulps   xmm2, [edx+32]
        mulps   xmm3, [edx+48]
        addps   xmm0,xmm2
        addps   xmm1,xmm3

        movups  xmm2, [eax+64]
        movups  xmm3, [eax+80]
        mulps   xmm2, [edx+64]
        mulps   xmm3, [edx+80]
        addps   xmm0,xmm2
        addps   xmm1,xmm3

        movups  xmm2, [eax+96]
        movups  xmm3, [eax+112]
        mulps   xmm2, [edx+96]
        mulps   xmm3, [edx+112]
        addps   xmm0,xmm2
        addps   xmm1,xmm3

        addps   xmm0,xmm1

        ;sub     esp,16
        ;movups  [esp],xmm0
        ;fld     dword [esp+ 0]
        ;fadd    dword [esp+ 4]
        ;fadd    dword [esp+ 8]
        ;fadd    dword [esp+12]
        ;add     esp,16
         
         movhlps xmm1,xmm0
         addps   xmm0,xmm1
         movlps  [sp(%$p)],xmm0
        fld     dword [sp(%$p)]
        fadd    dword [sp(%$p)+4]
endproc


proc    scalar4n_float32_SIMD
        jmp     scalar4n_float32_i387
endproc


proc    scalar1n_float32_SIMD
        jmp     scalar1n_float32_i387
endproc

; end of scalar.nas
