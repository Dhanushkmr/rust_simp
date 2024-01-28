	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 13, 0
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #32
	.cfi_def_cfa_offset 32
	mov	w8, #4                          ; =0x4
	mov	w9, #1                          ; =0x1
	str	wzr, [sp, #12]
	stp	wzr, w8, [sp, #24]
	stp	wzr, w9, [sp, #16]
LBB0_1:                                 ; %cond
                                        ; =>This Inner Loop Header: Depth=1
	ldr	w8, [sp, #16]
	ldr	w9, [sp, #28]
	cmp	w8, w9
	b.ge	LBB0_3
; %bb.2:                                ; %body
                                        ;   in Loop: Header=BB0_1 Depth=1
	ldp	w9, w8, [sp, #20]
	ldr	w10, [sp, #16]
	add	w11, w8, w9
	str	w8, [sp, #12]
	add	w8, w10, #1
	stp	w11, w9, [sp, #20]
	str	w8, [sp, #16]
	b	LBB0_1
LBB0_3:                                 ; %end
	ldr	w0, [sp, #20]
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
