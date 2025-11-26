#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

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

Token *tokenise(const char *str) {
    Token *tokens = NULL;
    int count = 0;

    for (int i = 0; i < strlen(str); i++) {
        if (isalpha(str[i])) {
            int start = i;
            int len = 0;

            while (isalnum(str[i + len]))
                len++;
            
            char *buf = malloc(len + 1);
            memcpy(buf, &str[start], len);
            buf[len] = '\0';
            i += len - 1; // -1 because for loop will increment i

            // allocate new token
            tokens = realloc(tokens, sizeof(Token) * (count + 1));
            tokens[count].value = buf;
            
            if (strcmp(buf, "return") == 0)
                tokens[count].type = TOKEN_RETURN;
            else
                fprintf(stderr, "\"%s\" is not a valid token\n", buf);

            count++;
        }
    }
    // null terminate the token list
    tokens = realloc(tokens, sizeof(Token) * (count + 1));
    tokens[count].value = NULL;
    tokens[count].type = 0;

    return tokens;
}

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

    Token *tokens = tokenise(buffer);
    for (int i = 0; tokens[i].value != NULL; i++) {
        printf("Token: %s\n", tokens[i].value);
        free(tokens[i].value);
    }

    free(buffer);
    free(tokens);

    return EXIT_SUCCESS;
}