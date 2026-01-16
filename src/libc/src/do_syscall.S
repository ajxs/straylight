.global straylight_libc_do_syscall
.type straylight_libc_do_syscall, @function
straylight_libc_do_syscall:
	ecall
	ret
