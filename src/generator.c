#include "parser.c"

typedef struct {
    char *name;
    size_t stack_loc;
} Var;

typedef struct {
    char *output;
    Var *vars;
    int var_count;
    size_t stack_size;
} CodegenCtx;

static void append(char **buf, const char *text) {
    size_t old_len = strlen(*buf);
    size_t add_len = strlen(text);

    *buf = realloc(*buf, old_len + add_len + 1); // +1 for \0
    memcpy(*buf + old_len, text, add_len + 1);
}

void gen_expr(NodeExpr *expr, CodegenCtx *ctx) {
    if (expr->kind == NODE_EXPR_TERM) {
        NodeTerm *term = expr->as.term;
        if (term->kind == NODE_TERM_INT_LIT) {
            char *tmp;
            asprintf(&tmp,
                "    mov $%s, %%rax\n"
                "    push %%rax\n",
                term->as.int_lit->int_lit.value);

            ctx->stack_size++;
            
            append(&ctx->output, tmp);
            free(tmp);
        } 
        else if (term->kind == NODE_TERM_IDENT) {
            // TODO
        }
        else {
            fprintf(stderr, "Unknown term kind in code generation\n");
            exit(EXIT_FAILURE);
        }
    } else if (expr->kind == NODE_EXPR_ADD) {
        gen_expr(expr->as.add->rhs, ctx);
        gen_expr(expr->as.add->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    add %rbx, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_SUB) {
        gen_expr(expr->as.sub->rhs, ctx);
        gen_expr(expr->as.sub->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    sub %rbx, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_MUL) {
        gen_expr(expr->as.sub->rhs, ctx);
        gen_expr(expr->as.sub->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    mul %rbx\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_DIV) {
        gen_expr(expr->as.sub->rhs, ctx);
        gen_expr(expr->as.sub->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    div %rbx\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else {
        fprintf(stderr, "Unknown expression kind in code generation\n");
        exit(EXIT_FAILURE);
    }
}

void gen_stmt(NodeStmt *stmt, CodegenCtx *ctx) {
    if (stmt->kind == NODE_STMT_EXIT)
    {
        gen_expr(stmt->as.stmt_exit->expr, ctx);

        // syscall exit
        append(&ctx->output,
            "    mov $60, %rax\n"
            "    pop %rdi\n"
            "    syscall\n");

        ctx->stack_size--;
    } 
    else if (stmt->kind == NODE_STMT_VAR) {
        // check if var already exists
        for (int i = 0; i < ctx->var_count; i++) {
            if (strcmp(ctx->vars[i].name, stmt->as.stmt_var->ident.value) == 0) {
                fprintf(stderr, "Variable \"%s\" already declared\n", stmt->as.stmt_var->ident.value);
                exit(EXIT_FAILURE);
            }
        }

        // push expression result to top of stack
        gen_expr(stmt->as.stmt_var->expr, ctx);

        ctx->vars = realloc(ctx->vars, sizeof(Var) * (ctx->var_count + 1));
        Var *new_var = &ctx->vars[ctx->var_count];
        new_var->name = stmt->as.stmt_var->ident.value;
        new_var->stack_loc = ctx->stack_size;
        ctx->var_count++;

        printf("Declared variable \"%s\" at stack location %zu\n", new_var->name, new_var->stack_loc);
    } else {
        fprintf(stderr, "Unknown statement kind in code generation\n");
        exit(EXIT_FAILURE);
    }
}

char *gen_prog(NodeProg *prog) {
    CodegenCtx ctx = {0};
    asprintf(&ctx.output, ".global _start\n_start:\n");
    for (int i = 0; i < prog->stmt_count; i++) {
        gen_stmt(prog->stmts[i], &ctx);
    }
    return ctx.output;
}

// vars
// output
// stack size
// scopes?