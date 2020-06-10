section .text
extern error
extern print
global our_code_label
our_code_label:
mov eax, 12
imul  eax, 2
mov [ebp - 4 * 1] , eax
mov eax, 2
imul  eax, 2
mov [ebp - 4 * 2] , eax
mov eax, [ebp - 4 * 2] 
cmp eax, 0
jne label_true_10
mov eax, 1
jmp label_end_10
label_true_10:
mov eax, [ebp - 4 * 2] 
label_end_10:
mov [ebp - 4 * 3] , eax
mov eax, [ebp - 4 * 3] 
imul  eax, 3
mov [ebp - 4 * 2] , eax
mov eax, [ebp - 4 * 1] 
imul  eax, [ebp - 4 * 2] 
mov [ebp - 4 * 1] , eax
mov eax, 2
add eax, [ebp - 4 * 1] 
mov [ebp - 4 * 2] , eax
mov eax, 2
cmp eax, 0
jne label_true_27
mov eax, 1
jmp label_end_27
label_true_27:
mov eax, 2
label_end_27:
mov [ebp - 4 * 3] , eax
mov eax, [ebp - 4 * 2] 
add eax, [ebp - 4 * 3] 

ret