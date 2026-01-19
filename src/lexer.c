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
    TOKEN_CLOSE_SQUARE,
    TOKEN_COMMA,
    TOKEN_QUOTE,
    TOKEN_CHAR_LIT,
    TOKEN_COLON
} TokenType;

// static int is_type_token(TokenType token_type) {
//     switch (token_type) {
//         case TOKEN_BYTE:
//         case TOKEN_WORD:
//         case TOKEN_LONG:
//         case TOKEN_QWORD:
//         case TOKEN_INT8:
//         case TOKEN_INT16:
//         case TOKEN_INT32:
//         case TOKEN_INT64:
//         case TOKEN_UINT8:
//         case TOKEN_UINT16:
//         case TOKEN_UINT32:
//         case TOKEN_UINT64:
//         case TOKEN_CHAR:
//             return 1;
//         default:
//             return 0;
//     }
// }

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

char *char_to_denary_string(char c) {
    char *buf = malloc(4);   // max "255" + null terminator
    if (buf)
        snprintf(buf, 4, "%d", (unsigned char)c);
    return buf;
}

typedef struct {
    TokenType type;
    char *value;
} Token;

typedef struct {
    const char* str;
    size_t str_index;
    Token *tokens;
    int token_count;
} LexerCtx;

static void push_token(LexerCtx *ctx, TokenType type, char *value) {
    ctx->tokens = realloc(ctx->tokens, sizeof(Token) * (ctx->token_count + 1));
    ctx->tokens[ctx->token_count].type = type;
    ctx->tokens[ctx->token_count].value = value;
    ctx->token_count++;
}

void lex_char(LexerCtx *ctx) {
    if (ctx->str[ctx->str_index] == '\\') {
        // escape sequence
        ctx->str_index++;
        char esc_body = ctx->str[ctx->str_index];
        char esc_char;
        switch (esc_body) {
            case 'n': esc_char = '\n'; break;
            case '\\': esc_char = '\\'; break;
            case '\'': esc_char = '\''; break;
            case '\"': esc_char = '\"'; break;
            default:
                fprintf(stderr, "Unknown escape sequence: \\%c\n", esc_body);
                exit(EXIT_FAILURE);
        }
        push_token(ctx, TOKEN_CHAR_LIT, char_to_denary_string(esc_char));
    }
    else {
        push_token(ctx, TOKEN_CHAR_LIT, char_to_denary_string(ctx->str[ctx->str_index]));
    }
    ctx->str_index++;
}

static void *copy_lexme(const char *str, int start, int len) {
    char *buf = malloc(len + 1);
    memcpy(buf, &str[start], len);
    buf[len] = '\0';
    return buf;
}

Token *tokenise(const char *str, int *out_count) {
    LexerCtx ctx = {
        .str = str,
        .str_index = 0,
        .tokens = NULL,
        .token_count = 0
    };

    while (ctx.str_index < strlen(str)) {
        char c = str[ctx.str_index];
        char c_next = str[ctx.str_index + 1];
        if (isalpha(c) || c == '_') {
            int start = ctx.str_index;
            int len = 0;

            while (isalnum(str[ctx.str_index + len]) || str[ctx.str_index + len] == '_')
                len++;
            
            char *buf = copy_lexme(str, start, len);
            ctx.str_index += len - 1; // -1 because for loop will increment i
           
            if (strcmp(buf, "exit") == 0) {
                push_token(&ctx, TOKEN_EXIT, NULL);
                free(buf);
            } else if (strcmp(buf, "if") == 0) {
                push_token(&ctx, TOKEN_IF, NULL);
                free(buf);
            } else if (strcmp(buf, "else") == 0) {
                push_token(&ctx, TOKEN_ELSE, NULL);
                free(buf);
            } else if (strcmp(buf, "for") == 0) {
                push_token(&ctx, TOKEN_FOR, NULL);
                free(buf);
            } else if (strcmp(buf, "byte") == 0) {
                push_token(&ctx, TOKEN_BYTE, NULL);
                free(buf);
            } else if (strcmp(buf, "word") == 0) {
                push_token(&ctx, TOKEN_WORD, NULL);
                free(buf);
            } else if (strcmp(buf, "long") == 0) {
                push_token(&ctx, TOKEN_LONG, NULL);
                free(buf);
            } else if (strcmp(buf, "qword") == 0) {
                push_token(&ctx, TOKEN_QWORD, NULL);
                free(buf);
            } else if (strcmp(buf, "int8") == 0) {
                push_token(&ctx, TOKEN_INT8, NULL);
                free(buf);
            } else if (strcmp(buf, "int16") == 0) {
                push_token(&ctx, TOKEN_INT16, NULL);
                free(buf);
            } else if (strcmp(buf, "int32") == 0) {
                push_token(&ctx, TOKEN_INT32, NULL);
                free(buf);
            } else if (strcmp(buf, "int64") == 0) {
                push_token(&ctx, TOKEN_INT64, NULL);
                free(buf);
            } else if (strcmp(buf, "uint8") == 0) {
                push_token(&ctx, TOKEN_UINT8, NULL);
                free(buf);
            } else if (strcmp(buf, "uint16") == 0) {
                push_token(&ctx, TOKEN_UINT16, NULL);
                free(buf);
            } else if (strcmp(buf, "uint32") == 0) {
                push_token(&ctx, TOKEN_UINT32, NULL);
                free(buf);
            } else if (strcmp(buf, "uint64") == 0) {
                push_token(&ctx, TOKEN_UINT64, NULL);
                free(buf);
            } else if (strcmp(buf, "char") == 0) {
                push_token(&ctx, TOKEN_CHAR, NULL);
                free(buf);
            } else if (strcmp(buf, "alloc") == 0) {
                push_token(&ctx, TOKEN_ALLOC, NULL);
                free(buf);
            } else if (strcmp(buf, "free") == 0) {
                push_token(&ctx, TOKEN_FREE, NULL);
                free(buf);
            } else {
                push_token(&ctx, TOKEN_IDENT, buf);
            }
        }
        else if (isdigit(c) || (c == '-' && isdigit(c_next))) {
            int start = ctx.str_index;
            int len = 0;

            // handle optional negative sign
            if (c == '-') {
                len++;
            }

            while (isdigit(str[ctx.str_index + len]))
                len++;

            char *buf = copy_lexme(str, start, len);
            ctx.str_index += len - 1;
            push_token(&ctx, TOKEN_INT_LIT, buf);
        } 
        else if (c == ';') {
            push_token(&ctx, TOKEN_SEMI, NULL);
        } else if (c == '+') {
            push_token(&ctx, TOKEN_PLUS, NULL);
        } else if (c == '-') {
            push_token(&ctx, TOKEN_MINUS, NULL);
        } else if (c == '*') {
            push_token(&ctx, TOKEN_ASTERISK, NULL);
        } else if (c == '/' && str[ctx.str_index + 1] == '/') {
            // comment: skip until end of line
            ctx.str_index += 2;
            while (ctx.str_index < strlen(str) && str[ctx.str_index] != '\n') {
                ctx.str_index++;
            }
            ctx.str_index++;
            continue;
        } else if (c == '/') {
            push_token(&ctx, TOKEN_FSLASH, NULL);
        } else if (c == '=' && str[ctx.str_index + 1] == '=') {
            push_token(&ctx, TOKEN_DOUBLE_EQUALS, NULL);
            ctx.str_index++;
        } else if (c == '=') {
            push_token(&ctx, TOKEN_EQUALS, NULL);
        } else if (c == '(') {
            push_token(&ctx, TOKEN_OPEN_PAREN, NULL);
        } else if (c == ')') {
            push_token(&ctx, TOKEN_CLOSE_PAREN, NULL);
        } else if (c == '{') {
            push_token(&ctx, TOKEN_OPEN_CURLY, NULL);
        } else if (c == '}') {
            push_token(&ctx, TOKEN_CLOSE_CURLY, NULL);
        } else if (c == '>' && str[ctx.str_index + 1] == '=') {
            push_token(&ctx, TOKEN_GTE, NULL);
            ctx.str_index++;
        } else if (c == '<' && str[ctx.str_index + 1] == '=') {
            push_token(&ctx, TOKEN_LTE, NULL);
            ctx.str_index++;
        } else if (c == '>') {
            push_token(&ctx, TOKEN_GT, NULL);
        } else if (c == '<') {
            push_token(&ctx, TOKEN_LT, NULL);
        } else if (c == '&') {
            push_token(&ctx, TOKEN_AMPERSAND, NULL);
        } else if (c == '|') {
            push_token(&ctx, TOKEN_PIPE, NULL);
        } else if (c == '[') {
            push_token(&ctx, TOKEN_OPEN_SQUARE, NULL);
        } else if (c == ']') {
            push_token(&ctx, TOKEN_CLOSE_SQUARE, NULL);
        } else if (c == ',') {
            push_token(&ctx, TOKEN_COMMA, NULL);
        } else if (c == ':') {
            push_token(&ctx, TOKEN_COLON, NULL);
        }
        else if (c == '\'') {
            ctx.str_index++;
            lex_char(&ctx);
            if (str[ctx.str_index] != '\'') {
                fprintf(stderr, "Expected closing single quote for character literal\n");
                exit(EXIT_FAILURE);
            }
        } else if (c == '\"') {
            push_token(&ctx, TOKEN_QUOTE, NULL);
            ctx.str_index++;
            while (ctx.str_index < strlen(str) && str[ctx.str_index] != '"') {
                lex_char(&ctx);
            }
            if (str[ctx.str_index] != '"') {
                fprintf(stderr, "Expected closing double quote for string literal\n");
                exit(EXIT_FAILURE);
            }
            push_token(&ctx, TOKEN_QUOTE, NULL);
        }
        else if (isspace(c)) {
            ctx.str_index++;
            continue;
        }
        else {
            fprintf(stderr, "\"%c\" is not valid here\n", c);
            exit(EXIT_FAILURE);
        }
        ctx.str_index++;
    }

    *out_count = ctx.token_count;
    return ctx.tokens;
}