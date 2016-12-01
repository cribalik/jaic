// Terminal textstyles
#ifdef USE_COLORS

#define RED "\e[31m"
#define GREEN "\e[32m"
#define YELLOW "\e[33m"
#define BLUE "\e[34m"
#define BOLD "\e[1m"
#define UNBOLD "\e[21m"
#define RESET_FORMAT "\e[0m"
#define RESET_COLOR "\e[39m"

#else

#define RED
#define GREEN
#define YELLOW
#define BLUE
#define BOLD
#define UNBOLD
#define RESET_FORMAT
#define RESET_COLOR

#endif 
