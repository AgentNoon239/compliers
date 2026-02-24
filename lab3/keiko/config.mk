# keiko/config.mk

### Let's use the portable code for all platforms,
### so we hear of problems quickly.
CC = gcc -std=gnu99
HOST_DEFINES = -DSEGMEM
