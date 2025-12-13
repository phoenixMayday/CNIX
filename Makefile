CC = gcc
CFLAGS = -Wall -Wextra -std=c11
TARGET = cnix
SRC_DIR = src
SRC = $(SRC_DIR)/main.c

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) $(SRC) -o $(TARGET)

clean:
	rm -f $(TARGET) out.s out

run: $(TARGET)
ifndef FILE
	$(error FILE is not set. Usage: make run FILE=examples/example.cnix)
endif
	./$(TARGET) $(FILE)
	gcc -nostdlib out.s -o out
	./out
	@echo $$?
