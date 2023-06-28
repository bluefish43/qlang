fdef factorial 1 num int int

push int 1
pop result

label loop_start

ld num
push int 1
lt
jmps loop_end

ld result
ld num
mul
pop result

ld num
push int 1
sub
pop num

jmp loop_start

label loop_end

ld result
ret

fendef

push int 20
tastk
ivk factorial
putln

throw "Hello, world!"