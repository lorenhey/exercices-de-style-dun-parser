CC=gcc
CFLAGS=-O2 -std=c99 -Wall -Wextra -pedantic

all: parser_v7

parser_v7: parser_v7.c
	$(CC) $(CFLAGS) -o parser_v7 parser_v7.c

clean:
	rm -f parser_v7 out.json
