#ifndef JAIC_TERMINAL
#define JAIC_TERMINAL
/* Terminal textstyles */

static char* RED = "";
static char* GREEN = "";
static char* YELLOW = "";
static char* BLUE = "";
static char* BOLD = "";
static char* UNBOLD = "";
static char* RESET_FORMAT = "";
static char* RESET_COLOR = "";

static void enable_formatting() {
  RED = "\x1B[31m";
  GREEN = "\x1B[32m";
  YELLOW = "\x1B[33m";
  BLUE = "\x1B[34m";
  BOLD = "\x1B[1m";
  UNBOLD = "\x1B[21m";
  RESET_FORMAT = "\x1B[0m";
  RESET_COLOR = "\x1B[39m";
}

#endif /* JAIC_TERMINAL */