.section .text

.global _start
_start:
	call load_gp
	call main
	# @TODO: How to propagate exit status from 'main'?
	j    exit

# Taken from: https://github.com/bminor/glibc/blob/master/sysdeps/riscv/start.S
load_gp:
.option push
.option norelax
	lla gp, __global_pointer$
.option pop
	ret
