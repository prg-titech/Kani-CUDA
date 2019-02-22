#include<stdio.h>

int main(){
    int j = (1>0) ? 1 : 2 + (1>0) ? 3 : 4;
    printf("%d",j);
    return 0;
}
