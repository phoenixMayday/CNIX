#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    TOKEN_EXIT,
    TOKEN_INT_LIT,
    TOKEN_SEMI
} TokenType;

typedef struct {
    TokenType type;
    char *value;
} Token;

Token *tokenise(const char *str, int *out_count) {
    Token *tokens = NULL;
    int count = 0;

    for (int i = 0; i < strlen(str); i++) {
        char c = str[i];
        if (isalpha(c)) {
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
            
            if (strcmp(buf, "exit") == 0) {
                tokens[count].type = TOKEN_EXIT;
                tokens[count].value = NULL;
            }
            else {
                fprintf(stderr, "\"%s\" is not a valid token\n", buf);
                exit(EXIT_FAILURE);
            }

            count++;
        }
        else if (isdigit(c))
        {
            int start = i;
            int len = 0;

            while (isdigit(str[i + len]))
                len++;

            char *buf = malloc(len + 1);
            memcpy(buf, &str[start], len);
            buf[len] = '\0';
            i += len - 1;

            // allocate new token
            tokens = realloc(tokens, sizeof(Token) * (count + 1));
            tokens[count].type = TOKEN_INT_LIT;
            tokens[count].value = buf;

            count++;
        }
        else if (c == ';')
        {
            // allocate new token
            tokens = realloc(tokens, sizeof(Token) * (count + 1));
            tokens[count].type = TOKEN_SEMI;
            tokens[count].value = NULL;

            count++;
        }
        else if (isspace(c))
        {
            continue;
        }
        else {
            fprintf(stderr, "\"%c\" is not valid here\n", c);
            exit(EXIT_FAILURE);
        }
    }
    // null terminate the token list
    tokens = realloc(tokens, sizeof(Token) * (count + 1));
    tokens[count].value = NULL;
    tokens[count].type = 0;

    *out_count = count;
    return tokens;
}