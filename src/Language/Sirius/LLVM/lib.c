#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <execinfo.h>
#include "color.h"
#include <stdarg.h>

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

void panic(char* message) {
  printf("%s[ERROR]%s %s", BRED, COLOR_RESET, message);


  printf("\n");
  
  void* callstack[10];
  int i, frames = backtrace(callstack, 10);
  printf(HBLK);
  char** strs = backtrace_symbols(callstack, frames);
  for (i = 0; i < frames; ++i) {
    printf("  %s\n", strs[i]);
  }
  printf(COLOR_RESET);
  free(strs);

  exit(0);
}

int maximum(int len, ...) {
  va_list valist;
  int max = 0;
  int i;
  va_start(valist, len);
  for (i = 0; i < len; i++) {
    int n = va_arg(valist, int);
    if (n > max) {
      max = n;
    }
  }
  va_end(valist);
  return max;
}