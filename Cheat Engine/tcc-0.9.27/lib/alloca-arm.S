	.text
	.align	2
	.global	alloca
	.type	alloca, %function
alloca:
#ifdef __TINYC__
        .int 0xe060d00d
        .int 0xe3cdd007
        .int 0xe1a0000d
        .int 0xe1a0f00e
#else
	rsb	sp, r0, sp
	bic	sp, sp, #7
	mov	r0, sp
	mov	pc, lr
#endif
	.size	alloca, .-alloca
