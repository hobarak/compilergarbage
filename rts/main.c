#include <stdio.h>


extern int our_code_label() asm("our_code_label");

//returns the function in eax

int main (){
  printf("%d\n", our_code_label());
  return 0;
}
 