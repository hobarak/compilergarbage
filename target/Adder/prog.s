section .text
extern error
extern print
global our_code_label
our_code_label:
mov dword esi, [esp + 4] 
push dword  ebp
mov dword ebp, esp
sub esp, 400
_impl:
mov dword eax, 4
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 24
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 4
imul  eax, 24
sar eax, 1
mov dword [ebp - 4 * 1] , eax
mov dword eax, 4
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 4
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 4
imul  eax, 4
sar eax, 1
mov dword [ebp - 4 * 2] , eax
mov dword eax, [ebp - 4 * 2] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 6
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 2] 
sub eax, 6
and eax, 0x80000000
or eax, 0x00000001
mov dword [ebp - 4 * 3] , eax
mov dword eax, [ebp - 4 * 3] 
mov dword ebx, eax
and ebx, 0x00000007
cmp ebx, 0x00000001
jne label_error
mov dword eax, [ebp - 4 * 3] 
cmp eax, 0x00000001
jne label_true_13
mov dword eax, 2
jmp label_end_13
label_true_13:
mov dword eax, [ebp - 4 * 2] 
label_end_13:
mov dword [ebp - 4 * 4] , eax
mov dword eax, [ebp - 4 * 4] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 6
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 4] 
imul  eax, 6
sar eax, 1
mov dword [ebp - 4 * 2] , eax
mov dword eax, [ebp - 4 * 1] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 2] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 1] 
imul  eax, [ebp - 4 * 2] 
sar eax, 1
mov dword [ebp - 4 * 1] , eax
mov dword eax, 222
mov dword [esi - 4 * -1] , eax
mov dword eax, 246
mov dword [esi - 4 * -2] , eax
mov dword eax, 24
mov dword [esi - 4 * -3] , eax
mov dword [esi - 4 * 0] , 3
mov dword eax, esi
or eax, 0x00000007
add esi, 16
mov dword [ebp - 4 * 2] , eax
mov dword eax, 8
mov dword [esi - 4 * -1] , eax
mov dword eax, 0x00000001
mov dword [esi - 4 * -2] , eax
mov dword [esi - 4 * 0] , 2
mov dword eax, esi
or eax, 0x00000007
add esi, 16
mov dword [ebp - 4 * 3] , eax
mov dword eax, 4
mov dword [esi - 4 * -1] , eax
mov dword eax, [ebp - 4 * 3] 
mov dword [esi - 4 * -2] , eax
mov dword [esi - 4 * 0] , 2
mov dword eax, esi
or eax, 0x00000007
add esi, 16
mov dword [ebp - 4 * 4] , eax
mov dword eax, 2
mov dword [esi - 4 * -1] , eax
mov dword eax, [ebp - 4 * 4] 
mov dword [esi - 4 * -2] , eax
mov dword [esi - 4 * 0] , 2
mov dword eax, esi
or eax, 0x00000007
add esi, 16
mov dword [ebp - 4 * 3] , eax
mov dword eax, 4
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 1] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 4
add eax, [ebp - 4 * 1] 
mov dword [ebp - 4 * 4] , eax
push dword  8
push dword  6
call duygus
mov dword [ebp - 4 * 5] , eax
mov dword eax, 8
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 4
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 8
add eax, 4
mov dword [ebp - 4 * 6] , eax
push dword  [ebp - 4 * 6] 
push dword  [ebp - 4 * 5] 
call boran
mov dword [ebp - 4 * 7] , eax
mov dword eax, [ebp - 4 * 4] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 7] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 4] 
add eax, [ebp - 4 * 7] 
mov dword [ebp - 4 * 8] , eax
push dword  10
call fac
mov dword [ebp - 4 * 9] , eax
mov dword eax, [ebp - 4 * 8] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 9] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 8] 
add eax, [ebp - 4 * 9] 
mov dword [ebp - 4 * 10] , eax
push dword  10
push dword  2
call fac2
mov dword [ebp - 4 * 11] , eax
mov dword eax, [ebp - 4 * 10] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 11] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 10] 
add eax, [ebp - 4 * 11] 
mov dword [ebp - 4 * 12] , eax
mov dword eax, [ebp - 4 * 2] 
mov dword ebx, eax
and ebx, 0x00000007
cmp ebx, 0x00000007
jne label_error
mov dword eax, 0
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword ebx, [ebp - 4 * 2] 
and ebx, 0xfffffff8
mov dword eax, 0
imul  eax, 4
sar eax, 1
add ebx, eax
mov dword eax, [ebx - 4 * -1] 
mov dword [ebp - 4 * 13] , eax
mov dword eax, [ebp - 4 * 12] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 13] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 12] 
add eax, [ebp - 4 * 13] 
mov dword [ebp - 4 * 14] , eax
mov dword eax, [ebp - 4 * 2] 
mov dword ebx, eax
and ebx, 0x00000007
cmp ebx, 0x00000007
jne label_error
mov dword eax, 2
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword ebx, [ebp - 4 * 2] 
and ebx, 0xfffffff8
mov dword eax, 2
imul  eax, 4
sar eax, 1
add ebx, eax
mov dword eax, [ebx - 4 * -1] 
mov dword [ebp - 4 * 15] , eax
mov dword eax, [ebp - 4 * 14] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 15] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 14] 
add eax, [ebp - 4 * 15] 
mov dword [ebp - 4 * 16] , eax
mov dword eax, [ebp - 4 * 2] 
mov dword ebx, eax
and ebx, 0x00000007
cmp ebx, 0x00000007
jne label_error
mov dword eax, 4
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword ebx, [ebp - 4 * 2] 
and ebx, 0xfffffff8
mov dword eax, 4
imul  eax, 4
sar eax, 1
add ebx, eax
mov dword eax, [ebx - 4 * -1] 
mov dword [ebp - 4 * 17] , eax
mov dword eax, [ebp - 4 * 16] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 17] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 16] 
add eax, [ebp - 4 * 17] 
mov dword [ebp - 4 * 18] , eax
mov dword eax, 2
mov dword [esi - 4 * -1] , eax
mov dword eax, 6
mov dword [esi - 4 * -2] , eax
mov dword eax, 8
mov dword [esi - 4 * -3] , eax
mov dword eax, [ebp - 4 * 18] 
mov dword [esi - 4 * -4] , eax
mov dword [esi - 4 * 0] , 4
mov dword eax, esi
or eax, 0x00000007
add esi, 24
mov dword esp, ebp
pop dword ebp
ret
boran:
push dword  ebp
mov dword ebp, esp
sub esp, 400
boran_impl:
mov dword eax, [ebp - 4 * -2] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -3] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -2] 
add eax, [ebp - 4 * -3] 
mov dword esp, ebp
pop dword ebp
ret
duygus:
push dword  ebp
mov dword ebp, esp
sub esp, 400
duygus_impl:
mov dword eax, [ebp - 4 * -2] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -3] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -2] 
imul  eax, [ebp - 4 * -3] 
sar eax, 1
mov dword esp, ebp
pop dword ebp
ret
fac:
push dword  ebp
mov dword ebp, esp
sub esp, 400
fac_impl:
mov dword eax, [ebp - 4 * -2] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 4
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -2] 
sub eax, 4
and eax, 0x80000000
or eax, 0x00000001
mov dword [ebp - 4 * 2] , eax
mov dword eax, [ebp - 4 * 2] 
mov dword ebx, eax
and ebx, 0x00000007
cmp ebx, 0x00000001
jne label_error
mov dword eax, [ebp - 4 * 2] 
cmp eax, 0x00000001
jne label_true_16
mov dword eax, [ebp - 4 * -2] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 2
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -2] 
sub eax, 2
mov dword [ebp - 4 * 3] , eax
push dword  [ebp - 4 * 3] 
call fac
mov dword [ebp - 4 * 4] , eax
mov dword eax, [ebp - 4 * -2] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * 4] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -2] 
imul  eax, [ebp - 4 * 4] 
sar eax, 1
jmp label_end_16
label_true_16:
mov dword eax, 2
label_end_16:
mov dword esp, ebp
pop dword ebp
ret
fac2:
push dword  ebp
mov dword ebp, esp
sub esp, 400
fac2_impl:
mov dword eax, [ebp - 4 * -3] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 4
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -3] 
sub eax, 4
and eax, 0x80000000
or eax, 0x00000001
mov dword [ebp - 4 * 3] , eax
mov dword eax, [ebp - 4 * 3] 
mov dword ebx, eax
and ebx, 0x00000007
cmp ebx, 0x00000001
jne label_error
mov dword eax, [ebp - 4 * 3] 
cmp eax, 0x00000001
jne label_true_17
mov dword eax, [ebp - 4 * -2] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -3] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -2] 
imul  eax, [ebp - 4 * -3] 
sar eax, 1
mov dword [ebp - 4 * 4] , eax
mov dword eax, [ebp - 4 * -3] 
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, 2
mov dword ebx, eax
and ebx, 0x00000001
cmp ebx, 0x00000000
jne label_error
mov dword eax, [ebp - 4 * -3] 
sub eax, 2
mov dword [ebp - 4 * 5] , eax
mov dword ebx, [ebp - 4 * 4] 
mov dword [ebp - 4 * -2] , ebx
mov dword ebx, [ebp - 4 * 5] 
mov dword [ebp - 4 * -3] , ebx
jmp fac2_impl
jmp label_end_17
label_true_17:
mov dword eax, [ebp - 4 * -2] 
label_end_17:
mov dword esp, ebp
pop dword ebp
ret
label_error:
push eax
push 0
call error
