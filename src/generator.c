#include <stdarg.h>

#include "parser.c"

typedef enum {
    BYTE = 1,  // 1 byte
    WORD = 2,  // 2 bytes
    LONG = 4,  // 4 bytes
    QWORD = 8  // 8 bytes
} MemWidth;

typedef struct {
    MemWidth size;
    int is_signed;
    int is_pointer;
    MemWidth pointee_size;
} VarType;

typedef struct {
    char *name;
    size_t stack_loc;
    VarType type;
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

VarType token_type_to_var_type(TokenType token_type, int is_pointer) {
    VarType type;
    type.is_pointer = is_pointer;
    
    MemWidth base_size;
    int base_signed;
    
    switch (token_type) {
        case TOKEN_BYTE:
        case TOKEN_UINT8:
        case TOKEN_CHAR:
            base_size = BYTE;
            base_signed = 0;
            break;
        case TOKEN_WORD:
        case TOKEN_UINT16:
            base_size = WORD;
            base_signed = 0;
            break;
        case TOKEN_LONG:
        case TOKEN_UINT32:
            base_size = LONG;
            base_signed = 0;
            break;
        case TOKEN_QWORD:
        case TOKEN_UINT64:
            base_size = QWORD;
            base_signed = 0;
            break;
        case TOKEN_INT8:
            base_size = BYTE;
            base_signed = 1;
            break;
        case TOKEN_INT16:
            base_size = WORD;
            base_signed = 1;
            break;
        case TOKEN_INT32:
            base_size = LONG;
            base_signed = 1;
            break;
        case TOKEN_INT64:
            base_size = QWORD;
            base_signed = 1;
            break;
        default:
            fprintf(stderr, "Invalid type token\n");
            exit(EXIT_FAILURE);
    }
    
    if (is_pointer) {
        type.size = QWORD;             // pointers are always 8 bytes
        type.is_signed = 0;
        type.pointee_size = base_size; // but remember what they point to
    } else {
        type.size = base_size;
        type.is_signed = base_signed;
        type.pointee_size = 0;         // non-pointers don't have a pointee
    }
    
    return type;
}

const char* get_mov_suffix(MemWidth width) {
    switch (width) {
        case BYTE:
            return "b";
        case WORD:
            return "w";
        case LONG:
            return "l";
        case QWORD:
            return "q";
        default:
            return "q";
    }
}

const char* get_register_for_width(MemWidth width, const char reg_name) {
    // reg_name should be 'a', 'b', etc. for rax, rbx
    static char reg_buf[8];
    switch (width) {
        case BYTE:
            snprintf(reg_buf, sizeof(reg_buf), "%%%cl", reg_name);  // %al, %bl
            break;
        case WORD:
            snprintf(reg_buf, sizeof(reg_buf), "%%%cx", reg_name);  // %ax, %bx
            break;
        case LONG:
            snprintf(reg_buf, sizeof(reg_buf), "%%e%cx", reg_name); // %eax, %ebx
            break;
        case QWORD:
            snprintf(reg_buf, sizeof(reg_buf), "%%r%cx", reg_name); // %rax, %rbx
            break;
    }
    return reg_buf;
}

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
    ctx->stack_size -= QWORD;
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
    ctx->stack_size -= QWORD;
}

void gen_term(NodeTerm *term, CodegenCtx *ctx) {
    if (term->kind == NODE_TERM_INT_LIT) {
        ctx->stack_size += QWORD;
        
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

        // read variable value and push as qword (with sign or zero extension)
        VarType var_type = ctx->vars[var_index].type;
        MemWidth var_width = var_type.size;
        size_t offset = ctx->stack_size - ctx->vars[var_index].stack_loc;
        
        if (var_width == QWORD) {
            // can push directly
            appendf(&ctx->output,
                "    pushq %zu(%%rsp)\n",
                offset);
        } else if (var_width == LONG) {
            if (var_type.is_signed) {
                // movslq for sign-extension (32-bit to 64-bit)
                appendf(&ctx->output,
                    "    movslq %zu(%%rsp), %%rax\n"
                    "    pushq %%rax\n",
                    offset);
            } else {
                // movl automatically zero-extends to 64-bit
                appendf(&ctx->output,
                    "    movl %zu(%%rsp), %%eax\n"
                    "    pushq %%rax\n",
                    offset);
            }
        } else {
            // load byte/word with sign or zero extension
            if (var_type.is_signed) {
                // sign-extension: movsbq (byte) or movswq (word)
                appendf(&ctx->output,
                    "    movs%sq %zu(%%rsp), %%rax\n"
                    "    pushq %%rax\n",
                    get_mov_suffix(var_width),
                    offset);
            } else {
                // zero-extension: movzbq (byte) or movzwq (word)
                appendf(&ctx->output,
                    "    movz%sq %zu(%%rsp), %%rax\n"
                    "    pushq %%rax\n",
                    get_mov_suffix(var_width),
                    offset);
            }
        }

        ctx->stack_size += QWORD;
    } 
    else if (term->kind == NODE_TERM_PAREN) {
        gen_expr(term->as.paren->expr, ctx);
    }
    else if (term->kind == NODE_TERM_ADDR_OF) {
        int var_index = get_var_index(term->as.addr_of->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", term->as.addr_of->ident.value);
            exit(EXIT_FAILURE);
        }
        
        size_t offset = ctx->stack_size - ctx->vars[var_index].stack_loc;
        
        // get address of a variable and push pointer to stack
        appendf(&ctx->output,
            "    leaq %zu(%%rsp), %%rax\n"
            "    pushq %%rax\n",
            offset);
        
        ctx->stack_size += QWORD;
    }
    else if (term->kind == NODE_TERM_DEREF) {
        gen_expr(term->as.deref->expr, ctx);
        
        // pop pointer address from stack, push value at that address
        appendf(&ctx->output,
            "    popq %%rax\n"
            "    movq (%%rax), %%rbx\n"
            "    pushq %%rbx\n");
    }
    else if (term->kind == NODE_TERM_ARRAY_INDEX) {
        // array indexing: arr[index]
        int var_index = get_var_index(term->as.array_index->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", term->as.array_index->ident.value);
            exit(EXIT_FAILURE);
        }
        
        VarType var_type = ctx->vars[var_index].type;
        if (!var_type.is_pointer) {
            fprintf(stderr, "Cannot index non-pointer variable \"%s\"\n", 
                    term->as.array_index->ident.value);
            exit(EXIT_FAILURE);
        }
        
        MemWidth element_size = var_type.pointee_size;
        
        // push pointer value to stack
        size_t offset = ctx->stack_size - ctx->vars[var_index].stack_loc;
        appendf(&ctx->output,
            "    pushq %zu(%%rsp)\n",
            offset);
        ctx->stack_size += QWORD;
        
        // generate index expression
        gen_expr(term->as.array_index->index, ctx);
        
        // pop index and pointer, calculate address and load value
        if (element_size == QWORD || element_size == LONG) {
            // qwords and longs can be loaded directly
            appendf(&ctx->output,
                "    popq %%rbx\n"              // index
                "    popq %%rax\n"              // pointer
                "    imulq $%d, %%rbx\n"        // multiply by element size
                "    addq %%rbx, %%rax\n"       // add to pointer
                "    mov%s (%%rax), %s\n"       // load value (into rcx or ecx)
                "    pushq %%rcx\n",            // push result
                element_size,
                get_mov_suffix(element_size),
                get_register_for_width(element_size, 'c'));
        } else {
            // byte and word need explicit zero-extension
            appendf(&ctx->output,
                "    popq %%rbx\n"              // index
                "    popq %%rax\n"              // pointer
                "    imulq $%d, %%rbx\n"        // multiply by element size
                "    addq %%rbx, %%rax\n"       // add to pointer
                "    movz%sq (%%rax), %%rcx\n"  // load value with zero-extension
                "    pushq %%rcx\n",            // push result
                element_size,
                get_mov_suffix(element_size));
        }
        
        ctx->stack_size -= QWORD;
    }
    else if (term->kind == NODE_TERM_ALLOC) {
        // allocate memory using mmap syscall
        /*
        mmap(
            NULL,                      // let kernel choose address
            size,                      // bytes to allocate
            PROT_READ|PROT_WRITE,      // read/write (1|2 = 3)
            MAP_PRIVATE|MAP_ANONYMOUS, // private (not shared) and anonymous(no file, just ram) (2|32 = 34)
            -1,                        // file descriptor (-1 because anonymous)
            0                          // file offset (none because anonymous)
        )
        returns pointer to allocated memory, -1 on error
        */
        
        gen_expr(term->as.alloc->size, ctx);
        
        appendf(&ctx->output,
            "    popq %%rsi\n"              // size (2nd arg)
            "    movq $9, %%rax\n"          // mmap syscall number 
            "    xorq %%rdi, %%rdi\n"       // NULL (1st arg)
            "    movq $3, %%rdx\n"          // PROT_READ|PROT_WRITE (3rd arg)
            "    movq $34, %%r10\n"         // MAP_PRIVATE|MAP_ANONYMOUS (4th arg)
            "    movq $-1, %%r8\n"          // -1 (5th arg)
            "    xorq %%r9, %%r9\n"         // 0 (6th arg)
            "    syscall\n"
            "    pushq %%rax\n");           // push returned pointer
    }
    else if (term->kind == NODE_TERM_FREE) {
        // free memory using munmap syscall

        /*
        munmap(
            addr,    // address to unmap
            length   // number of bytes to unmap
        )
        returns 0 on success, -1 on error
        */
        
        gen_expr(term->as.free_ptr->size, ctx);
        gen_expr(term->as.free_ptr->ptr, ctx);
        
        appendf(&ctx->output,
            "    popq %%rdi\n"              // addr (1st arg)
            "    popq %%rsi\n"              // length (2nd arg)
            "    movq $11, %%rax\n"         // munmap syscall number
            "    syscall\n"
            "    pushq %%rax\n");           // push return value
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
        // note: returns 128 bit result in rdx:rax, but we only care about lower 64 bits in rax
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    mulq %rbx\n", ctx);
    } else if (expr->kind == NODE_EXPR_DIV) {
        // clear rdx (remainder) before div
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    xorq %rdx, %rdx\n"
                                                            "    divq %rbx\n", ctx);
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

    // calculate total bytes to pop based on variable widths
    size_t bytes_to_pop = 0;
    for (int i = prev_var_count; i < ctx->var_count; i++) {
        bytes_to_pop += ctx->vars[i].type.size;
    }

    // adjust stack pointer
    appendf(&ctx->output,
        "    add $%zu, %%rsp\n",
        bytes_to_pop);

    // update context
    ctx->stack_size -= bytes_to_pop;
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

        ctx->stack_size -= QWORD;
    } 
    else if (stmt->kind == NODE_STMT_ASSIGN) {
        // check if var already exists
        int var_index = get_var_index(stmt->as.stmt_assign->ident.value, ctx);
        if (var_index >= 0) {
            fprintf(stderr, "Variable \"%s\" already defined\n", stmt->as.stmt_assign->ident.value);
            exit(EXIT_FAILURE);
        }

        // push expression result (qword) to top of stack
        gen_expr(stmt->as.stmt_assign->expr, ctx);

        // get variable type
        VarType var_type = token_type_to_var_type(stmt->as.stmt_assign->var_type, 
                                                   stmt->as.stmt_assign->is_pointer);
        MemWidth var_width = var_type.size;

        // pop expression result and allocate exact space for variable
        appendf(&ctx->output,
            "    popq %%rax\n"
            "    sub $%d, %%rsp\n"
            "    mov%s %s, (%%rsp)\n",
            var_width,
            get_mov_suffix(var_width),
            get_register_for_width(var_width, 'a'));

        // update stack size: popped 8 bytes, allocated var_width bytes
        ctx->stack_size = ctx->stack_size - QWORD + var_width;

        // push new variable to array
        ctx->vars = realloc(ctx->vars, sizeof(Var) * (ctx->var_count + 1));
        Var *new_var = &ctx->vars[ctx->var_count];
        new_var->name = stmt->as.stmt_assign->ident.value;
        new_var->type = var_type;
        new_var->stack_loc = ctx->stack_size;
        ctx->var_count++;
    } 
    else if (stmt->kind == NODE_STMT_REASSIGN) {
        // check if "_" (discard identifier)
        if (strcmp(stmt->as.stmt_reassign->ident.value, "_") == 0) {
            // generate expression and discard result
            gen_expr(stmt->as.stmt_reassign->expr, ctx);
            
            appendf(&ctx->output,
                "    addq $8, %%rsp\n");
            ctx->stack_size -= QWORD;

            return;
        }

        // find variable
        int var_index = get_var_index(stmt->as.stmt_reassign->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", stmt->as.stmt_reassign->ident.value);
            exit(EXIT_FAILURE);
        }

        // generate expression
        gen_expr(stmt->as.stmt_reassign->expr, ctx);

        // pop expression result into variable's stack location
        MemWidth var_width = ctx->vars[var_index].type.size;
        size_t offset = ctx->stack_size - QWORD - ctx->vars[var_index].stack_loc;
        
        appendf(&ctx->output,
            "    popq %%rax\n"
            "    mov%s %s, %zu(%%rsp)\n",
            get_mov_suffix(var_width),
            get_register_for_width(var_width, 'a'),
            offset);

        ctx->stack_size -= QWORD;
    }
    else if (stmt->kind == NODE_STMT_ASSIGN_ARRAY) {
        int var_index = get_var_index(stmt->as.stmt_assign_array->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", stmt->as.stmt_assign_array->ident.value);
            exit(EXIT_FAILURE);
        }
        
        VarType var_type = ctx->vars[var_index].type;
        if (!var_type.is_pointer) {
            fprintf(stderr, "Cannot index non-pointer variable \"%s\"\n", 
                    stmt->as.stmt_assign_array->ident.value);
            exit(EXIT_FAILURE);
        }
        
        MemWidth element_size = var_type.pointee_size;
        
        // generate index expression
        gen_expr(stmt->as.stmt_assign_array->index, ctx);
        
        // generate value expression
        gen_expr(stmt->as.stmt_assign_array->expr, ctx);
        
        // load the pointer value
        size_t offset = ctx->stack_size - ctx->vars[var_index].stack_loc;
        
        // pop value and index, load pointer, calc address and store
        appendf(&ctx->output,
            "    popq %%rax\n"              // value
            "    popq %%rbx\n"              // index
            "    movq %zu(%%rsp), %%rcx\n"  // load pointer
            "    imulq $%d, %%rbx\n"        // multiply by element size
            "    addq %%rbx, %%rcx\n"       // add to pointer
            "    mov%s %s, (%%rcx)\n",      // store value at address
            offset - 2 * QWORD,
            element_size,
            get_mov_suffix(element_size),
            get_register_for_width(element_size, 'a'));
        
        ctx->stack_size -= 2 * QWORD;
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
        ctx->stack_size -= QWORD;

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
            ctx->stack_size -= QWORD;
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