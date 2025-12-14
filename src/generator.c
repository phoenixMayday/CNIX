#include <stdarg.h>

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

int get_var_index(const char *name, CodegenCtx *ctx) {
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

// variadic append function
static void appendf(char **buf, const char *fmt, ...) {
    va_list args;
    char *tmp;
    
    va_start(args, fmt);
    vasprintf(&tmp, fmt, args);
    va_end(args);
    
    append(buf, tmp);
    free(tmp);
}

void gen_expr(NodeExpr *expr, CodegenCtx *ctx);

// helper function for binary expressions
static void gen_binary_op(NodeExpr *lhs, NodeExpr *rhs, const char *op_asm, CodegenCtx *ctx) {
    gen_expr(rhs, ctx);
    gen_expr(lhs, ctx);
    appendf(&ctx->output,
        "    popq %%rax\n"
        "    popq %%rbx\n"
        "%s"
        "    pushq %%rax\n",
        op_asm);
    ctx->stack_size--;
}

// helper function for comparison expressions
static void gen_comparison_op(NodeExpr *lhs, NodeExpr *rhs, const char *op_asm, CodegenCtx *ctx) {
    gen_expr(rhs, ctx);
    gen_expr(lhs, ctx);
    appendf(&ctx->output,
        "    popq %%rax\n"
        "    popq %%rbx\n"
        "    cmpq %%rbx, %%rax\n"
        "%s"
        "    movzbq %%al, %%rax\n"
        "    pushq %%rax\n",
        op_asm);
    ctx->stack_size--;
}

void gen_term(NodeTerm *term, CodegenCtx *ctx) {
    if (term->kind == NODE_TERM_INT_LIT) {
        ctx->stack_size++;
        
        appendf(&ctx->output,
            "    movq $%s, %%rax\n"
            "    pushq %%rax\n",
            term->as.int_lit->int_lit.value);
    } 
    else if (term->kind == NODE_TERM_IDENT) {
        // find variable
        int var_index = get_var_index(term->as.ident->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", term->as.ident->ident.value);
            exit(EXIT_FAILURE);
        }

        // push copy of variable's value to top of stack
        appendf(&ctx->output,
            "    pushq %zu(%%rsp)\n",
            (ctx->stack_size - ctx->vars[var_index].stack_loc) * 8);

        ctx->stack_size++;
    } 
    else if (term->kind == NODE_TERM_PAREN) {
        gen_expr(term->as.paren->expr, ctx);
    }
    else {
        fprintf(stderr, "Unknown term kind in code generation\n");
        exit(EXIT_FAILURE);
    }
}

void gen_expr(NodeExpr *expr, CodegenCtx *ctx) {
    if (expr->kind == NODE_EXPR_TERM) {
        NodeTerm *term = expr->as.term;
        gen_term(term, ctx);
    } else if (expr->kind == NODE_EXPR_ADD) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    addq %rbx, %rax\n", ctx);
    } else if (expr->kind == NODE_EXPR_SUB) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    subq %rbx, %rax\n", ctx);
    } else if (expr->kind == NODE_EXPR_MUL) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    mulq %rbx\n", ctx);
    } else if (expr->kind == NODE_EXPR_DIV) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    divq %rbx\n", ctx);
    } else if (expr->kind == NODE_EXPR_GTE) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    setge %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_LTE) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    setle %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_GT) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    setg %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_LT) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    setl %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_EQUALITY) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    sete %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_AND) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    andq %rbx, %rax\n", ctx);
    } else if (expr->kind == NODE_EXPR_OR) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    orq %rbx, %rax\n", ctx);
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
    appendf(&ctx->output,
        "    add $%zu, %%rsp\n",
        vars_to_pop * 8);

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
        appendf(&ctx->output,
            "    movq $60, %%rax\n"
            "    popq %%rdi\n"
            "    syscall\n");

        ctx->stack_size--;
    } 
    else if (stmt->kind == NODE_STMT_VAR) {
        // check if var already exists
        int var_index = get_var_index(stmt->as.stmt_var->ident.value, ctx);
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
        int var_index = get_var_index(stmt->as.stmt_reassign->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", stmt->as.stmt_reassign->ident.value);
            exit(EXIT_FAILURE);
        }

        // generate expression
        gen_expr(stmt->as.stmt_reassign->expr, ctx);

        // pop expression result into variable's stack location
        appendf(&ctx->output,
            "    popq %%rax\n"
            "    movq %%rax, %zu(%%rsp)\n",
            // subtract 1 because we popped
            (ctx->stack_size - 1 - ctx->vars[var_index].stack_loc) * 8);

        ctx->stack_size--;
    }
    else if (stmt->kind == NODE_STMT_SCOPE) {
        gen_scope(stmt->as.stmt_scope->scope, ctx);
    } 
    else if (stmt->kind == NODE_STMT_IF) {
        gen_expr(stmt->as.stmt_if->expr, ctx);

        // jump to end or else if expr is 0 (false)
        int if_end_label_id = ctx->label_count++;
        appendf(&ctx->output, 
            "    popq %%rax\n"
            "    cmpq $0, %%rax\n"
            "    je .Lend_if_%d\n",
            if_end_label_id);
        ctx->stack_size--;

        gen_scope(stmt->as.stmt_if->scope, ctx);

        // else scope
        if (stmt->as.stmt_if->else_scope != NULL) {
            int else_end_label_id = ctx->label_count++;

            appendf(&ctx->output,
                "    jmp .Lend_if_%d\n" // jump to the end of the else (skip the else) if executed
                ".Lend_if_%d:\n",       // end of the if (else start)
                else_end_label_id,
                if_end_label_id);

            gen_scope(stmt->as.stmt_if->else_scope, ctx);

            appendf(&ctx->output,
                ".Lend_if_%d:\n",
                else_end_label_id);
        } else {
            // just end label
            appendf(&ctx->output,
                ".Lend_if_%d:\n",
                if_end_label_id);
        }
    }
    else if (stmt->kind == NODE_STMT_FOR) {
        // init statement
        if (stmt->as.stmt_for->init != NULL) {
            gen_stmt(stmt->as.stmt_for->init, ctx);
        }

        int for_start_label_id = ctx->label_count++;
        int for_end_label_id = ctx->label_count++;

        // start label
        appendf(&ctx->output,
            ".Lstart_for_%d:\n",
            for_start_label_id);

        // condition
        if (stmt->as.stmt_for->condition != NULL) {
            gen_expr(stmt->as.stmt_for->condition, ctx);

            // jump to end if condition is false
            appendf(&ctx->output,
                "    popq %%rax\n"
                "    cmpq $0, %%rax\n"
                "    je .Lend_for_%d\n",
                for_end_label_id);
            ctx->stack_size--;
        }

        // loop body
        gen_scope(stmt->as.stmt_for->scope, ctx);

        // increment
        if (stmt->as.stmt_for->increment != NULL) {
            gen_stmt(stmt->as.stmt_for->increment, ctx);
        }

        // jump back to start
        appendf(&ctx->output,
            "    jmp .Lstart_for_%d\n"
            ".Lend_for_%d:\n",
            for_start_label_id,
            for_end_label_id);
    }
    else {
        fprintf(stderr, "Unknown statement kind in code generation: %d\n", stmt->kind);
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