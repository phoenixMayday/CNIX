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
            
            if (strcmp(buf, "return") == 0) {
                tokens[count].type = TOKEN_RETURN;
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

char *tokens_to_asm(const Token *tokens, int token_count) {
    char *output;
    asprintf(&output, ".global _start\n_start:\n");
    for (int i = 0; i < token_count; i++) {
        const Token token = tokens[i];

        // this is fucked
        if (token.type == TOKEN_RETURN) {
            if (i + 1 < token_count && tokens[i + 1].type == TOKEN_INT_LIT) {
                if (i + 2 < token_count && tokens[i + 2].type == TOKEN_SEMI) {
                    // append assembly code to output
                    char *tmp;
                    asprintf(&tmp,
                        "%s"
                        "    mov $60, %%rax\n"
                        "    mov $%s, %%rdi\n"
                        "    syscall\n",
                        output, tokens[i + 1].value);
                    free(output); // free old string
                    output = tmp;
                }
            }
        }
    }
    return output;
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

    int token_count;
    Token *tokens = tokenise(buffer, &token_count);
    for (int i = 0; i < token_count; i++) {
        printf("Token: %s\n", 
            tokens[i].type == TOKEN_RETURN ? "RETURN" :
            tokens[i].type == TOKEN_INT_LIT ? "INT_LIT" :
            tokens[i].type == TOKEN_SEMI ? "SEMI" : "UNKNOWN");
    }
    free(buffer);

    char *asm_code = tokens_to_asm(tokens, token_count);
    printf("Assembly code:\n%s\n", asm_code);
    FILE *out_fptr = fopen("out.s", "w");
    fprintf(out_fptr, "%s", asm_code);
    fclose(out_fptr);
    free(asm_code);

    // free tokens
    for (int i = 0; i < token_count; i++) {
        free(tokens[i].value);
    }
    free(tokens);

    return EXIT_SUCCESS;
}