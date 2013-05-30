/* -*- Mode:C; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: test.c,v 0.0 2012/08/16 08:38:36 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*/


#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <pwd.h>
#include <grp.h>
#include <signal.h>
#include <setjmp.h>


#define STR(x)   #x
#define XSTR(x)  STR(x)
#define A 3 
int
main(int argc, char *argv[]){
  printf(XSTR(A));
  printf(STR(A));
  return 0;
}

