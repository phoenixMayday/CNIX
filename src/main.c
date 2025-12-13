#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
    
#include "generator.c"

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input.cnix>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *fptr = fopen(argv[1], "r");
    if (!fptr) {
        fprintf(stderr, "Error opening file\n");
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

    printf("\nTokens:\n");

    int token_count;
    Token *tokens = tokenise(buffer, &token_count);
    for (int i = 0; i < token_count; i++) {
        printf("%s\n", 
            tokens[i].type == TOKEN_EXIT ? "EXIT" :
            tokens[i].type == TOKEN_INT_LIT ? "INT_LIT" :
            tokens[i].type == TOKEN_SEMI ? "SEMI" :
            tokens[i].type == TOKEN_PLUS ? "PLUS" : 
            tokens[i].type == TOKEN_MINUS ? "MINUS" :
            tokens[i].type == TOKEN_ASTERISK ? "MUL" :
            tokens[i].type == TOKEN_FSLASH ? "DIV" :
            tokens[i].type == TOKEN_VAR ? "VAR" :
            tokens[i].type == TOKEN_EQUALS ? "EQUALS" :
            tokens[i].type == TOKEN_IDENT ? "IDENT" :
            tokens[i].type == TOKEN_OPEN_PAREN ? "OPEN_PAREN" :
            tokens[i].type == TOKEN_CLOSE_PAREN ? "CLOSE_PAREN" :
            tokens[i].type == TOKEN_OPEN_CURLY ? "OPEN_CURLY" :
            tokens[i].type == TOKEN_CLOSE_CURLY ? "CLOSE_CURLY" :
            tokens[i].type == TOKEN_IF ? "IF" :
            tokens[i].type == TOKEN_ELSE ? "ELSE" :
            tokens[i].type == TOKEN_GT ? "GT" :
            tokens[i].type == TOKEN_LT ? "LT" :
            tokens[i].type == TOKEN_GTE ? "GTE" :
            tokens[i].type == TOKEN_LTE ? "LTE" :
            tokens[i].type == TOKEN_DOUBLE_EQUALS ? "DOUBLE_EQUALS" :
            tokens[i].type == TOKEN_AMPERSAND ? "AMPERSAND" :
            tokens[i].type == TOKEN_PIPE ? "PIPE" :
            tokens[i].type == TOKEN_FOR ? "FOR" :
            "UNKNOWN"
        );
    }
    free(buffer);

    NodeProg *prog = parse_prog(&tokens, token_count);
    char *asm_code = gen_prog(prog);
    printf("\nx86 64 assembly:\n%s\n", asm_code);
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