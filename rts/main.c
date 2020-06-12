#include <stdio.h>
#include   <stdlib.h>

extern int our_code_label() asm("our_code_label");
extern void error(int code, int v) asm("error");

#define MyTrue  0x80000001
#define MyFalse   0x00000001

void error(int code, int v){
    printf("type error %d %d\n", code, v);
    exit(1);
}

int print (int value){
  if (value == MyTrue) {
    printf("True");
  }
  else if(value == MyFalse){
    printf("False");

  }
  else if ((value & 0x00000001) == 0) {
      printf("print from c %d\n", value >> 1);
  }
  else {
    printf("unknown vlaue");
  }
  return value;
}

int main (){
  int result =  our_code_label();
  print(  result);
  return 0;
}
 

