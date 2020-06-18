#include <stdio.h>
#include   <stdlib.h>

extern int our_code_label(int* heap) asm("our_code_label");
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
    printf("bool : False");

  }
  else if ((value & 0x00000001) == 0) {
      printf("int %d\n", value >> 1);
  }
  else if ((value & 0x0000007) == 7) {
    int* ptr = value & 0xFFFFFFF8;
    printf("tuple\n");
    int size = *ptr;
    for(int i=1; i <= size; i++) {
        print(*(ptr + i));
    }
   

  }
  else {
    printf("unknown vlaue");
  }
  return value;
}
int main (){
  int* HEAP = calloc(1000, sizeof (int));
  int result =  our_code_label(HEAP);
  print(  result);
  printf("%d\n", *(HEAP+4));
  printf("%p\n", HEAP);
  return 0;
}
 

