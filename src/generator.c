#include "parser.c"

void gen_expr(NodeExpr *expr, char **expr_output) {
    if (expr->kind == NODE_EXPR_TERM) {
        NodeTerm *term = expr->as.term;
        if (term->kind == NODE_TERM_INT_LIT) {
            char *tmp;
            asprintf(&tmp,
                "%s"
                "    mov $%s, %%rax\n"
                "    push %%rax\n",
                *expr_output,
                term->as.int_lit->int_lit.value);
            free(*expr_output);
            *expr_output = tmp;
        } else {
            fprintf(stderr, "Unknown term kind in code generation\n");
            exit(EXIT_FAILURE);
        }
    } else if (expr->kind == NODE_EXPR_ADD) {
        gen_expr(expr->as.add->rhs, expr_output);
        gen_expr(expr->as.add->lhs, expr_output);
        char *tmp;
        asprintf(&tmp,
            "%s"
            "    pop %%rax\n"
            "    pop %%rbx\n"
            "    add %%rbx, %%rax\n"
            "    push %%rax\n",
            *expr_output
        );
        free(*expr_output);
        *expr_output = tmp;
    } else if (expr->kind == NODE_EXPR_SUB) {
        gen_expr(expr->as.sub->rhs, expr_output);
        gen_expr(expr->as.sub->lhs, expr_output);
        char *tmp;
        asprintf(&tmp,
            "%s"
            "    pop %%rax\n"
            "    pop %%rbx\n"
            "    sub %%rbx, %%rax\n"
            "    push %%rax\n",
            *expr_output
        );
        free(*expr_output);
        *expr_output = tmp;
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
            char *tmp;
            asprintf(&tmp,
                "%s"
                "%s"
                "    mov $60, %%rax\n"
                "    pop %%rdi\n"
                "    syscall\n",
                output, 
                expr_output);
            free(output);
            output = tmp;
            free(expr_output);
        } else {
            fprintf(stderr, "Unknown statement kind in code generation\n");
            exit(EXIT_FAILURE);
        }
    }
    return output;
}
                    