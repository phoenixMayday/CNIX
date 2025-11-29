#include "parser.c"

static void append(char **buf, const char *text) {
    size_t old_len = strlen(*buf);
    size_t add_len = strlen(text);

    *buf = realloc(*buf, old_len + add_len + 1); // +1 for \0
    memcpy(*buf + old_len, text, add_len + 1);
}

void gen_expr(NodeExpr *expr, char **expr_output) {
    if (expr->kind == NODE_EXPR_TERM) {
        NodeTerm *term = expr->as.term;
        if (term->kind == NODE_TERM_INT_LIT) {
            char *tmp;
            asprintf(&tmp,
                "    mov $%s, %%rax\n"
                "    push %%rax\n",
                term->as.int_lit->int_lit.value);
            
            append(expr_output, tmp);
            free(tmp);
        } else {
            fprintf(stderr, "Unknown term kind in code generation\n");
            exit(EXIT_FAILURE);
        }
    } else if (expr->kind == NODE_EXPR_ADD) {
        gen_expr(expr->as.add->rhs, expr_output);
        gen_expr(expr->as.add->lhs, expr_output);
        append(expr_output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    add %rbx, %rax\n"
            "    push %rax\n");
    } else if (expr->kind == NODE_EXPR_SUB) {
        gen_expr(expr->as.sub->rhs, expr_output);
        gen_expr(expr->as.sub->lhs, expr_output);
        append(expr_output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    sub %rbx, %rax\n"
            "    push %rax\n");
    } else if (expr->kind == NODE_EXPR_MUL) {
        gen_expr(expr->as.sub->rhs, expr_output);
        gen_expr(expr->as.sub->lhs, expr_output);
        append(expr_output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    mul %rbx\n"
            "    push %rax\n");
    } else if (expr->kind == NODE_EXPR_DIV) {
        gen_expr(expr->as.sub->rhs, expr_output);
        gen_expr(expr->as.sub->lhs, expr_output);
        append(expr_output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    div %rbx\n"
            "    push %rax\n");
    } else {
        fprintf(stderr, "Unknown expression kind in code generation\n");
        exit(EXIT_FAILURE);
    }
}

char *gen_stmts(NodeProg *prog) {
    char *output;
    asprintf(&output, ".global _start\n_start:\n");
    for (int i = 0; i < prog->stmt_count; i++) {
        const NodeStmt *current_stmt = prog->stmts[i];
        if (current_stmt->kind == NODE_STMT_EXIT)
        {
            char *expr_output = strdup("");
            gen_expr(current_stmt->as.exit_stmt->expr, &expr_output);

            append(&output, expr_output);
            free(expr_output);

            // syscall exit
            append(&output,
                   "    mov $60, %rax\n"
                   "    pop %rdi\n"
                   "    syscall\n");
        } else {
            fprintf(stderr, "Unknown statement kind in code generation\n");
            exit(EXIT_FAILURE);
        }
    }
    return output;
}
                    