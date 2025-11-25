#include <stdlib.h>
#include <stdio.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

typedef enum {
    TOKEN_RETURN,
    TOKEN_INT_LIT,
    TOKEN_SEMI
} TokenType;

typedef struct {
    TokenType type;
    char *value;
} Token;

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input.cnix>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *fptr = fopen(argv[1], "r");
    if (!fptr) {
        fprintf(stderr, "Error opening file");
        return EXIT_FAILURE;
    }

    // get file size and allocate buffer
    fseek(fptr, 0, SEEK_END);
    long length = ftell(fptr);
    fseek(fptr, 0, SEEK_SET);
    char *buffer = malloc(length + 1);

    // read file content into buffer
    fread(buffer, 1, length, fptr);
    buffer[length] = '\0';
    fclose(fptr);

    printf("File content:\n%s\n", buffer);

    return EXIT_SUCCESS;
}