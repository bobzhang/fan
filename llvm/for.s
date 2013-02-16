	.section	__TEXT,__text,regular,pure_instructions
	.section	__TEXT,__literal8,8byte_literals
	.align	3
LCPI0_0:
	.quad	4607182418800017408     ## double 1
LCPI0_1:
	.quad	4631107791820423168     ## double 42
LCPI0_2:
	.quad	0                       ## double 0
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_printstar
	.align	4, 0x90
_printstar:                             ## @printstar
	.cfi_startproc
## BB#0:                                ## %entry
	subq	$24, %rsp
Ltmp1:
	.cfi_def_cfa_offset 32
	vmovsd	%xmm0, 8(%rsp)          ## 8-byte Spill
	vmovsd	LCPI0_0(%rip), %xmm0
	vmovapd	%xmm0, %xmm1
	.align	4, 0x90
LBB0_1:                                 ## %loop
                                        ## =>This Inner Loop Header: Depth=1
	vmovsd	%xmm1, 16(%rsp)         ## 8-byte Spill
	vmovsd	LCPI0_1(%rip), %xmm0
	callq	_putchard
	vmovsd	16(%rsp), %xmm1         ## 8-byte Reload
	vucomisd	8(%rsp), %xmm1  ## 8-byte Folded Reload
	sbbl	%eax, %eax
	andl	$1, %eax
	vcvtsi2sdl	%eax, %xmm0, %xmm0
	vaddsd	LCPI0_0(%rip), %xmm1, %xmm1
	vucomisd	LCPI0_2(%rip), %xmm0
	jne	LBB0_1
## BB#2:                                ## %afterloop
	vxorpd	%xmm0, %xmm0, %xmm0
	addq	$24, %rsp
	ret
	.cfi_endproc


.subsections_via_symbols
