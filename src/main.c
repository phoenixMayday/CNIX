#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
    
#include "lexer.c"

char *tokens_to_asm(const Token *tokens, int token_count) {
    char *output;
    asprintf(&output, ".global _start\n_start:\n");
    for (int i = 0; i < token_count; i++) {
        const Token token = tokens[i];

        // this is fucked
        if (token.type == TOKEN_EXIT) {
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
            tokens[i].type == TOKEN_EXIT ? "EXIT" :
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