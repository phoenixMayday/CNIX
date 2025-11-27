#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
    
// #include "lexer.c"
// #include "parser.c"
#include "generator.c"

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
            tokens[i].type == TOKEN_SEMI ? "SEMI" :
            tokens[i].type == TOKEN_PLUS ? "PLUS" : 
            tokens[i].type == TOKEN_MINUS ? "MINUS" :
            "UNKNOWN"
        );
    }
    free(buffer);

    NodeProg *prog = parse_prog(&tokens, token_count);
    char *asm_code = gen_stmts(prog);
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