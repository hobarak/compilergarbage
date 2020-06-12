section .text
extern error
extern print
global our_code_label
our_code_label:
push dword  ebp
mov ebp, esp
sub esp, 400
_impl:
mov eax, 4
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 24
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 4
imul  eax, 24
sar eax, 1
mov [ebp - 4 * 1] , eax
mov eax, 4
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 4
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 4
imul  eax, 4
sar eax, 1
mov [ebp - 4 * 2] , eax
mov eax, [ebp - 4 * 2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 6
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 2] 
sub eax, 6
and eax, 0x80000000
or eax, 0x00000001
mov [ebp - 4 * 3] , eax
mov eax, [ebp - 4 * 3] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000001
jne label_error
mov eax, [ebp - 4 * 3] 
cmp eax, 0x00000001
jne label_true_13
mov eax, 2
jmp label_end_13
label_true_13:
mov eax, [ebp - 4 * 2] 
label_end_13:
mov [ebp - 4 * 4] , eax
mov eax, [ebp - 4 * 4] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 6
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 4] 
imul  eax, 6
sar eax, 1
mov [ebp - 4 * 2] , eax
mov eax, [ebp - 4 * 1] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 1] 
imul  eax, [ebp - 4 * 2] 
sar eax, 1
mov [ebp - 4 * 1] , eax
mov eax, 4
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 1] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 4
add eax, [ebp - 4 * 1] 
mov [ebp - 4 * 2] , eax
push dword  8
push dword  6
call duygus
mov [ebp - 4 * 3] , eax
mov eax, 8
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 4
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 8
add eax, 4
mov [ebp - 4 * 4] , eax
push dword  [ebp - 4 * 4] 
push dword  [ebp - 4 * 3] 
call boran
mov [ebp - 4 * 5] , eax
mov eax, [ebp - 4 * 2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 5] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 2] 
add eax, [ebp - 4 * 5] 
mov [ebp - 4 * 6] , eax
push dword  10
call fac
mov [ebp - 4 * 7] , eax
mov eax, [ebp - 4 * 6] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 7] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 6] 
add eax, [ebp - 4 * 7] 
mov [ebp - 4 * 8] , eax
push dword  10
push dword  2
call fac2
mov [ebp - 4 * 9] , eax
mov eax, [ebp - 4 * 8] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 9] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 8] 
add eax, [ebp - 4 * 9] 
mov esp, ebp
pop dword ebp
ret
boran:
push dword  ebp
mov ebp, esp
sub esp, 400
boran_impl:
mov eax, [ebp - 4 * -2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -3] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -2] 
add eax, [ebp - 4 * -3] 
mov esp, ebp
pop dword ebp
ret
duygus:
push dword  ebp
mov ebp, esp
sub esp, 400
duygus_impl:
mov eax, [ebp - 4 * -2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -3] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -2] 
imul  eax, [ebp - 4 * -3] 
sar eax, 1
mov esp, ebp
pop dword ebp
ret
fac:
push dword  ebp
mov ebp, esp
sub esp, 400
fac_impl:
mov eax, [ebp - 4 * -2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 4
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -2] 
sub eax, 4
and eax, 0x80000000
or eax, 0x00000001
mov [ebp - 4 * 2] , eax
mov eax, [ebp - 4 * 2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000001
jne label_error
mov eax, [ebp - 4 * 2] 
cmp eax, 0x00000001
jne label_true_16
mov eax, [ebp - 4 * -2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 2
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -2] 
sub eax, 2
mov [ebp - 4 * 3] , eax
push dword  [ebp - 4 * 3] 
call fac
mov [ebp - 4 * 4] , eax
mov eax, [ebp - 4 * -2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * 4] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -2] 
imul  eax, [ebp - 4 * 4] 
sar eax, 1
jmp label_end_16
label_true_16:
mov eax, 2
label_end_16:
mov esp, ebp
pop dword ebp
ret
fac2:
push dword  ebp
mov ebp, esp
sub esp, 400
fac2_impl:
mov eax, [ebp - 4 * -3] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 4
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -3] 
sub eax, 4
and eax, 0x80000000
or eax, 0x00000001
mov [ebp - 4 * 3] , eax
mov eax, [ebp - 4 * 3] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000001
jne label_error
mov eax, [ebp - 4 * 3] 
cmp eax, 0x00000001
jne label_true_17
mov eax, [ebp - 4 * -2] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -3] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -2] 
imul  eax, [ebp - 4 * -3] 
sar eax, 1
mov [ebp - 4 * 4] , eax
mov eax, [ebp - 4 * -3] 
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, 2
mov ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov eax, [ebp - 4 * -3] 
sub eax, 2
mov [ebp - 4 * 5] , eax
mov ebx, [ebp - 4 * 4] 
mov [ebp - 4 * -2] , ebx
mov ebx, [ebp - 4 * 5] 
mov [ebp - 4 * -3] , ebx
jmp fac2_impl
jmp label_end_17
label_true_17:
mov eax, [ebp - 4 * -2] 
label_end_17:
mov esp, ebp
pop dword ebp
ret
label_error:
push eax
push 0
call error
