#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char* show_int(int n) {
  int size = snprintf(NULL, 0, "%d", n);
  char* str = malloc(size + 1);
  sprintf(str, "%d", n);
  return str;
}

char* show_float(double n) {
  int size = snprintf(NULL, 0, "%f", n);
  char* str = malloc(size + 1);
  sprintf(str, "%f", n);
  return str;
}

char* concat(char* x, char* y) {
  int size = strlen(x) + strlen(y);
  char* str = malloc(size + 1);
  strcat(str, x);
  strcat(str, y);
  return str;
}

char* show_char(char c) {
  char* str = malloc(2 * sizeof(char));
  str[0] = c;
  str[1] = '\0';
  return str;
}