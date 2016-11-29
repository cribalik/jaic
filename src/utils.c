// Logging
#define logError(msg, ...) fprintf(stderr, BOLD "%s:%i: " RED "error: " RESET_COLOR RESET_FORMAT msg, __FILE__, __LINE__, ##__VA_ARGS__)

#ifdef DEBUG
#define logDebug(msg, ...) printf(BOLD "%s:%i: " GREEN "debug: " RESET_COLOR RESET_FORMAT msg, __FILE__, __LINE__, ##__VA_ARGS__)
#else
#define logDebug(msg, ...)
#endif

// Some macros
#define UNIMPLEMENTED logError("Unimplemented function at %s:%d\n", __FILE__, __LINE__);
#define UNREACHABLE logError("Unreachable\n"); exit(1);

// typedefs
typedef char byte;
typedef char bool;
typedef int bool32;
#define true 1
#define false 0

