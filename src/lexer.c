#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    TOKEN_EXIT,
    TOKEN_INT_LIT,
    TOKEN_SEMI,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_MUL,
    TOKEN_DIV
} TokenType;

typedef struct {
    TokenType type;
    char *value;
} Token;

static void push_token(Token **tokens, int *count, TokenType type, char *value) {
    *tokens = realloc(*tokens, sizeof(Token) * (*count + 1));
    (*tokens)[*count].type = type;
    (*tokens)[*count].value = value;
    (*count)++;
}

static void *copy_lexme(const char *str, int start, int len) {
    char *buf = malloc(len + 1);
    memcpy(buf, &str[start], len);
    buf[len] = '\0';
    return buf;
}

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
            
            char *buf = copy_lexme(str, start, len);
            i += len - 1; // -1 because for loop will increment i
           
            if (strcmp(buf, "exit") == 0) {
                push_token(&tokens, &count, TOKEN_EXIT, NULL);
            }
            else {
                fprintf(stderr, "\"%s\" is not a valid token\n", buf);
                exit(EXIT_FAILURE);
            }
            free(buf);
        }
        else if (isdigit(c)) {
            int start = i;
            int len = 0;

            while (isdigit(str[i + len]))
                len++;

            char *buf = copy_lexme(str, start, len);
            i += len - 1;

            push_token(&tokens, &count, TOKEN_INT_LIT, buf);
        } 
        else if (c == ';') {
            push_token(&tokens, &count, TOKEN_SEMI, NULL);
        } else if (c == '+') {
            push_token(&tokens, &count, TOKEN_PLUS, NULL);
        } else if (c == '-') {
            push_token(&tokens, &count, TOKEN_MINUS, NULL);
        } else if (c == '*') {
            push_token(&tokens, &count, TOKEN_MUL, NULL);
        } else if (c == '/') {
            push_token(&tokens, &count, TOKEN_DIV, NULL);
        } 
        else if (isspace(c)) {
            continue;
        }
        else {
            fprintf(stderr, "\"%c\" is not valid here\n", c);
            exit(EXIT_FAILURE);
        }
    }

    *out_count = count;
    return tokens;
}