/* keiko/lib.c */

#include "obx.h"
#include <string.h>

char *lib_version = "Primitives for labkit";

int lib_argc(void) {
     return saved_argc;
}

void lib_argv(int n, char *s) {
     /* Buffer overflow waiting to happen */
     strcpy(s, saved_argv[n]);
}

void lib_print_num(int n) {
     printf("%d", n);
}

void lib_print_string(char *s, int n) {
     printf("%.*s", n, s);
}

void lib_print_char(char c) {
     putchar(c);
}

static FILE *infile = NULL;

int lib_open_in(char *name) {
     FILE *f = fopen(name, "r");
     if (f == NULL) return 0;
     if (infile != NULL) fclose(infile);
     infile = f;
     return 1;
}

void lib_close_in(void) {
     if (infile == NULL) return;
     fclose(infile);
     infile = NULL;
}

void lib_read_char(char *p) {
     FILE *f = (infile == NULL ? stdin : infile);
     int ch = fgetc(f);
     *p = (ch == EOF ? 127 : ch);
}

value *lib_new(value *bp) {
     int size = bp[HEAD+1].i;
     value *sp = bp;
     (*--sp).a = virtual_alloc(size);
     return sp;
}

value *lib_gc_alloc(value *bp) {
     int size = bp[HEAD+1].i;
     value *sp = bp;
     (*--sp).a = gc_alloc(size, bp);
     return sp;
}
