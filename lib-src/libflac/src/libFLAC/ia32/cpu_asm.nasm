;  vim:filetype=nasm ts=8

;  libFLAC - Free Lossless Audio Codec library
;  Copyright (C) 2001-2009  Josh Coalson
;  Copyright (C) 2011-2014  Xiph.Org Foundation
;
;  Redistribution and use in source and binary forms, with or without
;  modification, are permitted provided that the following conditions
;  are met:
;
;  - Redistributions of source code must retain the above copyright
;  notice, this list of conditions and the following disclaimer.
;
;  - Redistributions in binary form must reproduce the above copyright
;  notice, this list of conditions and the following disclaimer in the
;  documentation and/or other materials provided with the distribution.
;
;  - Neither the name of the Xiph.org Foundation nor the names of its
;  contributors may be used to endorse or promote products derived from
;  this software without specific prior written permission.
;
;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR
;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%include "nasm.h"

	data_section

cglobal FLAC__cpu_have_cpuid_asm_ia32
cglobal FLAC__cpu_info_asm_ia32

	code_section

; **********************************************************************
;
; FLAC__uint32 FLAC__cpu_have_cpuid_asm_ia32()
;

cident FLAC__cpu_have_cpuid_asm_ia32
	pushfd
	pop	eax
	mov	edx, eax
	xor	eax, 0x00200000
	push	eax
	popfd
	pushfd
	pop	eax
	xor	eax, edx
	and	eax, 0x00200000
	shr	eax, 0x15
	push	edx
	popfd
	ret

; **********************************************************************
;
; void FLAC__cpu_info_asm_ia32(FLAC__uint32 *flags_edx, FLAC__uint32 *flags_ecx)
;

cident FLAC__cpu_info_asm_ia32
	;[esp + 8] == flags_edx
	;[esp + 12] == flags_ecx

	push	ebx
	call	FLAC__cpu_have_cpuid_asm_ia32
	test	eax, eax
	jz	.no_cpuid
	mov	eax, 0
	cpuid
	cmp	eax, 1
	jb	.no_cpuid
	mov	eax, 1
	cpuid
	mov	ebx, [esp + 8]
	mov	[ebx], edx
	mov	ebx, [esp + 12]
	mov	[ebx], ecx
	jmp	.end
.no_cpuid:
	xor	eax, eax
	mov	ebx, [esp + 8]
	mov	[ebx], eax
	mov	ebx, [esp + 12]
	mov	[ebx], eax
.end:
	pop	ebx
	ret

; end
