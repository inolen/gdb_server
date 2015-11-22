default: example

format: example.c gdb_server.h sh4.h sh4_interpreter.c sh4_interpreter.h sh4_server.c sh4_server.h
	clang-format -i $^

test: test_sh4.c sh4.h sh4_interpreter.c sh4_interpreter.h sh4_server.c sh4_server.h test_sh4.inc
	clang -g -Wall -Wextra -Werror -Wno-unused-parameter -xc $(filter %.c, $^) -o test

example: example.c gdb_server.h sh4.h sh4_interpreter.c sh4_interpreter.h sh4_server.c sh4_server.h
	clang -g -Wall -Wextra -Werror -Wno-unused-parameter -xc $(filter %.c, $^) -o example

