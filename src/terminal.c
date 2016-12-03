#ifndef JAIC_TERMINAL
#define JAIC_TERMINAL
// Terminal textstyles

static char* RED = "";
static char* GREEN = "";
static char* YELLOW = "";
static char* BLUE = "";
static char* BOLD = "";
static char* UNBOLD = "";
static char* RESET_FORMAT = "";
static char* RESET_COLOR = "";

static void enableFormatting() {
  RED = "\e[31m";
  GREEN = "\e[32m";
  YELLOW = "\e[33m";
  BLUE = "\e[34m";
  BOLD = "\e[1m";
  UNBOLD = "\e[21m";
  RESET_FORMAT = "\e[0m";
  RESET_COLOR = "\e[39m";
}

#endif /* JAIC_TERMINAL */