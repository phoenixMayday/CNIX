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
    size_t *scopes;
    int scope_count;
    int label_count;
} CodegenCtx;

int get_var_index(CodegenCtx *ctx, const char *name) {
    for (int i = 0; i < ctx->var_count; i++) {
        if (strcmp(ctx->vars[i].name, name) == 0) {
            return i;
        }
    }
    return -1; // not found
}

static void append(char **buf, const char *text) {
    size_t old_len = strlen(*buf);
    size_t add_len = strlen(text);

    *buf = realloc(*buf, old_len + add_len + 1); // +1 for \0
    memcpy(*buf + old_len, text, add_len + 1);
}

void gen_expr(NodeExpr *expr, CodegenCtx *ctx) {
    // Terms
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
            // find variable
            int var_index = get_var_index(ctx, term->as.ident->ident.value);
            if (var_index == -1) {
                fprintf(stderr, "Undefined variable \"%s\"\n", term->as.ident->ident.value);
                exit(EXIT_FAILURE);
            }

            // push copy of variable's value to top of stack
            char *tmp;
            asprintf(&tmp,
                "    pushq %zu(%%rsp)\n",
                (ctx->stack_size - ctx->vars[var_index].stack_loc) * 8);
            
            ctx->stack_size++;

            append(&ctx->output, tmp);
            free(tmp);
        } 
        else if (term->kind == NODE_TERM_PAREN) {
            gen_expr(term->as.paren->expr, ctx);
        }
        else {
            fprintf(stderr, "Unknown term kind in code generation\n");
            exit(EXIT_FAILURE);
        }
    // Expressions
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
    } else if (expr->kind == NODE_EXPR_GTE) {
        gen_expr(expr->as.gte->rhs, ctx);
        gen_expr(expr->as.gte->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    cmp %rbx, %rax\n"
            "    setge %al\n"
            "    movzbq %al, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_LTE) {
        gen_expr(expr->as.lte->rhs, ctx);
        gen_expr(expr->as.lte->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    cmp %rbx, %rax\n"
            "    setle %al\n"
            "    movzbq %al, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_GT) {
        gen_expr(expr->as.gt->rhs, ctx);
        gen_expr(expr->as.gt->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    cmp %rbx, %rax\n"
            "    setg %al\n"
            "    movzbq %al, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_LT) {
        gen_expr(expr->as.lt->rhs, ctx);
        gen_expr(expr->as.lt->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    cmp %rbx, %rax\n"
            "    setl %al\n"
            "    movzbq %al, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_EQUALITY) {
        gen_expr(expr->as.equality->rhs, ctx);
        gen_expr(expr->as.equality->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    cmp %rbx, %rax\n"
            "    sete %al\n"
            "    movzbq %al, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_AND) {
        gen_expr(expr->as.and->rhs, ctx);
        gen_expr(expr->as.and->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    and %rbx, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else if (expr->kind == NODE_EXPR_OR) {
        gen_expr(expr->as.or->rhs, ctx);
        gen_expr(expr->as.or->lhs, ctx);
        append(&ctx->output,
            "    pop %rax\n"
            "    pop %rbx\n"
            "    or %rbx, %rax\n"
            "    push %rax\n");
        ctx->stack_size--;
    } else {
        fprintf(stderr, "Unknown expression kind in code generation\n");
        exit(EXIT_FAILURE);
    }
}

void gen_stmt(NodeStmt *stmt, CodegenCtx *ctx);

void gen_scope(NodeScope *scope, CodegenCtx *ctx) {
    // push var count to scopes stack
    ctx->scopes = realloc(ctx->scopes, sizeof(size_t) * (ctx->scope_count + 1));
    ctx->scopes[ctx->scope_count] = ctx->var_count;
    ctx->scope_count++;

    // generate statements in scope
    for (int i = 0; i < scope->stmt_count; i++) {
        gen_stmt(scope->stmts[i], ctx);
    }

    // figure out how many vars to pop
    size_t prev_var_count = ctx->scopes[ctx->scope_count - 1];
    size_t vars_to_pop = ctx->var_count - prev_var_count;

    ctx->scope_count--;

    if (vars_to_pop == 0) {
        return;
    }

    // adjust stack pointer
    char *tmp;
    asprintf(&tmp,
        "    add $%zu, %%rsp\n",
        vars_to_pop * 8);
    append(&ctx->output, tmp);
    free(tmp);

    // update context
    ctx->stack_size -= vars_to_pop;
    ctx->var_count = prev_var_count;
    ctx->vars = realloc(ctx->vars, sizeof(Var) * ctx->var_count);
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
        int var_index = get_var_index(ctx, stmt->as.stmt_var->ident.value);
        if (var_index >= 0) {
            fprintf(stderr, "Variable \"%s\" already defined\n", stmt->as.stmt_var->ident.value);
            exit(EXIT_FAILURE);
        }

        // push expression result to top of stack
        gen_expr(stmt->as.stmt_var->expr, ctx);

        // push new variable to array
        ctx->vars = realloc(ctx->vars, sizeof(Var) * (ctx->var_count + 1));
        Var *new_var = &ctx->vars[ctx->var_count];
        new_var->name = stmt->as.stmt_var->ident.value;
        new_var->stack_loc = ctx->stack_size;
        ctx->var_count++;
    } 
    else if (stmt->kind == NODE_STMT_REASSIGN) {
        // find variable
        int var_index = get_var_index(ctx, stmt->as.stmt_reassign->ident.value);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", stmt->as.stmt_reassign->ident.value);
            exit(EXIT_FAILURE);
        }

        // generate expression
        gen_expr(stmt->as.stmt_reassign->expr, ctx);

        // pop expression result into variable's stack location
        char *tmp;
        asprintf(&tmp,
            "    pop %%rax\n"
            "    movq %%rax, %zu(%%rsp)\n",
            // subtract 1 because we popped
            (ctx->stack_size - 1 - ctx->vars[var_index].stack_loc) * 8);
        append(&ctx->output, tmp);
        free(tmp);

        ctx->stack_size--;
    }
    else if (stmt->kind == NODE_STMT_SCOPE) {
        gen_scope(stmt->as.stmt_scope->scope, ctx);
    } 
    else if (stmt->kind == NODE_STMT_IF) {
        gen_expr(stmt->as.stmt_if->expr, ctx);

        // jump to end or else if expr is 0 (false)
        int if_end_label_id = ctx->label_count++;
        char *tmp;
        asprintf(&tmp, 
            "    pop %%rax\n"
            "    cmp $0, %%rax\n"
            "    je .Lend_if_%d\n",
            if_end_label_id);
        append(&ctx->output, tmp);
        free(tmp);
        ctx->stack_size--;

        gen_scope(stmt->as.stmt_if->scope, ctx);

        // else scope
        if (stmt->as.stmt_if->else_scope != NULL) {
            int else_end_label_id = ctx->label_count++;

            asprintf(&tmp,
                "    jmp .Lend_if_%d\n" // jump to the end of the else (skip the else) if executed
                ".Lend_if_%d:\n",       // end of the if (else start)
                else_end_label_id,
                if_end_label_id);
            append(&ctx->output, tmp);
            free(tmp);

            gen_scope(stmt->as.stmt_if->else_scope, ctx);

            asprintf(&tmp,
                ".Lend_if_%d:\n",
                else_end_label_id);
            append(&ctx->output, tmp);
            free(tmp);
        } else {
            // just end label
            asprintf(&tmp,
                ".Lend_if_%d:\n",
                if_end_label_id);
            append(&ctx->output, tmp);
            free(tmp);
        }
    }
    else {
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