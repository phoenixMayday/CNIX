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
    TOKEN_ASTERISK,
    TOKEN_FSLASH,
    TOKEN_EQUALS,
    TOKEN_IDENT,
    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_CURLY,
    TOKEN_CLOSE_CURLY,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_GT,
    TOKEN_LT,
    TOKEN_GTE,
    TOKEN_LTE,
    TOKEN_DOUBLE_EQUALS,
    TOKEN_AMPERSAND,
    TOKEN_PIPE,
    TOKEN_FOR,
    TOKEN_BYTE,
    TOKEN_WORD,
    TOKEN_LONG,
    TOKEN_QWORD,
    TOKEN_INT8,
    TOKEN_INT16,
    TOKEN_INT32,
    TOKEN_INT64,
    TOKEN_UINT8,
    TOKEN_UINT16,
    TOKEN_UINT32,
    TOKEN_UINT64,
    TOKEN_CHAR,
    TOKEN_ALLOC,
    TOKEN_FREE,
    TOKEN_OPEN_SQUARE,
    TOKEN_CLOSE_SQUARE
} TokenType;

static int get_precedence(int token_type) {
    switch (token_type) {
        case TOKEN_ASTERISK:
        case TOKEN_FSLASH:
            return 6;
        case TOKEN_PLUS:
        case TOKEN_MINUS:
            return 5;
        case TOKEN_GTE:
        case TOKEN_LTE:
        case TOKEN_GT:
        case TOKEN_LT:
            return 4;
        case TOKEN_DOUBLE_EQUALS:
            return 3;
        case TOKEN_AMPERSAND:
            return 2;
        case TOKEN_PIPE:
            return 1;
        default:
            return 0;
    }
}

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

    for (size_t i = 0; i < strlen(str); i++) {
        char c = str[i];
        char c_next = str[i + 1];
        if (isalpha(c)) {
            int start = i;
            int len = 0;

            while (isalnum(str[i + len]))
                len++;
            
            char *buf = copy_lexme(str, start, len);
            i += len - 1; // -1 because for loop will increment i
           
            if (strcmp(buf, "exit") == 0) {
                push_token(&tokens, &count, TOKEN_EXIT, NULL);
                free(buf);
            } else if (strcmp(buf, "if") == 0) {
                push_token(&tokens, &count, TOKEN_IF, NULL);
                free(buf);
            } else if (strcmp(buf, "else") == 0) {
                push_token(&tokens, &count, TOKEN_ELSE, NULL);
                free(buf);
            } else if (strcmp(buf, "for") == 0) {
                push_token(&tokens, &count, TOKEN_FOR, NULL);
                free(buf);
            } else if (strcmp(buf, "byte") == 0) {
                push_token(&tokens, &count, TOKEN_BYTE, NULL);
                free(buf);
            } else if (strcmp(buf, "word") == 0) {
                push_token(&tokens, &count, TOKEN_WORD, NULL);
                free(buf);
            } else if (strcmp(buf, "long") == 0) {
                push_token(&tokens, &count, TOKEN_LONG, NULL);
                free(buf);
            } else if (strcmp(buf, "qword") == 0) {
                push_token(&tokens, &count, TOKEN_QWORD, NULL);
                free(buf);
            } else if (strcmp(buf, "int8") == 0) {
                push_token(&tokens, &count, TOKEN_INT8, NULL);
                free(buf);
            } else if (strcmp(buf, "int16") == 0) {
                push_token(&tokens, &count, TOKEN_INT16, NULL);
                free(buf);
            } else if (strcmp(buf, "int32") == 0) {
                push_token(&tokens, &count, TOKEN_INT32, NULL);
                free(buf);
            } else if (strcmp(buf, "int64") == 0) {
                push_token(&tokens, &count, TOKEN_INT64, NULL);
                free(buf);
            } else if (strcmp(buf, "uint8") == 0) {
                push_token(&tokens, &count, TOKEN_UINT8, NULL);
                free(buf);
            } else if (strcmp(buf, "uint16") == 0) {
                push_token(&tokens, &count, TOKEN_UINT16, NULL);
                free(buf);
            } else if (strcmp(buf, "uint32") == 0) {
                push_token(&tokens, &count, TOKEN_UINT32, NULL);
                free(buf);
            } else if (strcmp(buf, "uint64") == 0) {
                push_token(&tokens, &count, TOKEN_UINT64, NULL);
                free(buf);
            } else if (strcmp(buf, "char") == 0) {
                push_token(&tokens, &count, TOKEN_CHAR, NULL);
                free(buf);
            } else if (strcmp(buf, "alloc") == 0) {
                push_token(&tokens, &count, TOKEN_ALLOC, NULL);
                free(buf);
            } else if (strcmp(buf, "free") == 0) {
                push_token(&tokens, &count, TOKEN_FREE, NULL);
                free(buf);
            } else {
                push_token(&tokens, &count, TOKEN_IDENT, buf);
            }
        }
        else if (isdigit(c) || (c == '-' && isdigit(c_next))) {
            int start = i;
            int len = 0;

            // handle optional negative sign
            if (c == '-') {
                len++;
            }

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
            push_token(&tokens, &count, TOKEN_ASTERISK, NULL);
        } else if (c == '/' && str[i + 1] == '/') {
            // comment: skip until end of line
            i += 2;
            while (i < strlen(str) && str[i] != '\n') {
                i++;
            }
            continue;
        } else if (c == '/') {
            push_token(&tokens, &count, TOKEN_FSLASH, NULL);
        } else if (c == '=' && str[i + 1] == '=') {
            push_token(&tokens, &count, TOKEN_DOUBLE_EQUALS, NULL);
            i++;
        } else if (c == '=') {
            push_token(&tokens, &count, TOKEN_EQUALS, NULL);
        } else if (c == '(') {
            push_token(&tokens, &count, TOKEN_OPEN_PAREN, NULL);
        } else if (c == ')') {
            push_token(&tokens, &count, TOKEN_CLOSE_PAREN, NULL);
        } else if (c == '{') {
            push_token(&tokens, &count, TOKEN_OPEN_CURLY, NULL);
        } else if (c == '}') {
            push_token(&tokens, &count, TOKEN_CLOSE_CURLY, NULL);
        } else if (c == '>' && str[i + 1] == '=') {
            push_token(&tokens, &count, TOKEN_GTE, NULL);
            i++;
        } else if (c == '<' && str[i + 1] == '=') {
            push_token(&tokens, &count, TOKEN_LTE, NULL);
            i++;
        } else if (c == '>') {
            push_token(&tokens, &count, TOKEN_GT, NULL);
        } else if (c == '<') {
            push_token(&tokens, &count, TOKEN_LT, NULL);
        } else if (c == '&') {
            push_token(&tokens, &count, TOKEN_AMPERSAND, NULL);
        } else if (c == '|') {
            push_token(&tokens, &count, TOKEN_PIPE, NULL);
        } else if (c == '[') {
            push_token(&tokens, &count, TOKEN_OPEN_SQUARE, NULL);
        } else if (c == ']') {
            push_token(&tokens, &count, TOKEN_CLOSE_SQUARE, NULL);
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