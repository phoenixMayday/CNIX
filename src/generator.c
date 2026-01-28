#include <stdarg.h>

#include "parser.c"

typedef enum {
    BYTE = 1,  // 1 byte
    WORD = 2,  // 2 bytes
    LONG = 4,  // 4 bytes
    QWORD = 8  // 8 bytes
} MemWidth;

typedef struct {
    char *name;
    size_t stack_loc;
    MemWidth total_width;
    int is_signed;
    int is_pointer;
    MemWidth element_width; // 0 if not indexable
} Var;

int is_token_signed(TokenType token_type) {
    switch (token_type) {
        case TOKEN_INT8:
        case TOKEN_INT16:
        case TOKEN_INT32:
        case TOKEN_INT64:
            return 1;
        default:
            return 0;
    }
}

int is_indexable(Var *var) {
    return var->element_width > 0;
}

int is_array(Var *var) {
    return !var->is_pointer && var->element_width > 0;
}

typedef struct {
    char *output; 
    Var *vars;
    int var_count;
    size_t stack_size;
    size_t *scopes;
    int scope_count;
    int label_count;
} CodegenCtx;

MemWidth token_to_base_size(TokenType token_type) {
    switch (token_type) {
        case TOKEN_BYTE:
        case TOKEN_UINT8:
        case TOKEN_INT8:
        case TOKEN_CHAR:
            return BYTE;
        case TOKEN_WORD:
        case TOKEN_UINT16:
        case TOKEN_INT16:
            return WORD;
        case TOKEN_LONG:
        case TOKEN_UINT32:
        case TOKEN_INT32:
            return LONG;
        case TOKEN_QWORD:
        case TOKEN_UINT64:
        case TOKEN_INT64:
            return QWORD;
        default:
            fprintf(stderr, "Invalid type token\n");
            exit(EXIT_FAILURE);
    }
}

void set_var_type_from_token(Var *var, TokenType token_type, int is_pointer) {
    MemWidth base_size = token_to_base_size(token_type);
    int base_signed = is_token_signed(token_type);
    
    var->is_pointer = is_pointer;
    if (is_pointer) {
        var->total_width = QWORD;        // pointers are always 8 bytes
        var->is_signed = 0;
        var->element_width = base_size;  // remember what they point to
    } else {
        var->total_width = base_size;
        var->is_signed = base_signed;
        var->element_width = 0;          // non-pointers don't have elements
    }
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

// push array/pointer address onto stack
static void push_indexable_address(Var *var, CodegenCtx *ctx) {
    size_t var_offset = ctx->stack_size - var->stack_loc;
    
    // array: compute address with leaq
    if (is_array(var)) {
        appendf(&ctx->output,
            "    # push stack array address\n"
            "    leaq %zu(%%rsp), %%rax\n"
            "    pushq %%rax\n",
            var_offset);
    } 
    // pointer: load pointer value with movq
    else {
        appendf(&ctx->output,
            "    # push pointer value\n"
            "    movq %zu(%%rsp), %%rax\n"
            "    pushq %%rax\n",
            var_offset);
    }
    ctx->stack_size += QWORD;
}

void gen_term(NodeTerm *term, CodegenCtx *ctx) {
    if (term->kind == NODE_TERM_INT_LIT) {
        ctx->stack_size += QWORD;
        
        appendf(&ctx->output,
            "    # push integer literal `%s`\n"
            "    movq $%s, %%rax\n"
            "    pushq %%rax\n",
            term->as.int_lit->int_lit.value,
            term->as.int_lit->int_lit.value);
    }
    else if (term->kind == NODE_TERM_CHAR_LIT) {
        // treat char literal the same as integer literal
        ctx->stack_size += QWORD;
        appendf(&ctx->output,
            "    # push character literal `%s`\n"
            "    movq $%s, %%rax\n"
            "    pushq %%rax\n",
            term->as.char_lit->char_lit.value,
            term->as.char_lit->char_lit.value);
    }
    else if (term->kind == NODE_TERM_IDENT) {
        // find variable
        int var_index = get_var_index(term->as.ident->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", term->as.ident->ident.value);
            exit(EXIT_FAILURE);
        }

        // read variable value and push as qword (with sign or zero extension)
        Var *var = &ctx->vars[var_index];
        MemWidth var_width = var->total_width;
        size_t offset = ctx->stack_size - var->stack_loc;

        printf("Stack size: %zu, Var stack loc: %zu, Offset: %zu\n", 
               ctx->stack_size, var->stack_loc, offset);

        // if array: push address, not value
        if (is_array(var)) {
            appendf(&ctx->output,
                "    # load variable `%s` (array address)\n"
                "    leaq %zu(%%rsp), %%rax\n"
                "    pushq %%rax\n",
                term->as.ident->ident.value,
                offset);
            ctx->stack_size += QWORD;
            return;
        }
        
        if (var_width == QWORD) {
            // can push directly
            appendf(&ctx->output,
                "    # load variable `%s`\n"
                "    pushq %zu(%%rsp)\n",
                term->as.ident->ident.value,
                offset);
        } else if (var_width == LONG) {
            if (var->is_signed) {
                // movslq for sign-extension (32-bit to 64-bit)
                appendf(&ctx->output,
                    "    # load variable `%s` (sign-extend)\n"
                    "    movslq %zu(%%rsp), %%rax\n"
                    "    pushq %%rax\n",
                    term->as.ident->ident.value,
                    offset);
            } else {
                // movl automatically zero-extends to 64-bit
                appendf(&ctx->output,
                    "    # load variable `%s` (zero-extend)\n"
                    "    movl %zu(%%rsp), %%eax\n"
                    "    pushq %%rax\n",
                    term->as.ident->ident.value,
                    offset);
            }
        } else {
            // load byte/word with sign or zero extension
            if (var->is_signed) {
                // sign-extension: movsbq (byte) or movswq (word)
                appendf(&ctx->output,
                    "    # load variable `%s` (sign-extend)\n"
                    "    movs%sq %zu(%%rsp), %%rax\n"
                    "    pushq %%rax\n",
                    term->as.ident->ident.value,
                    get_mov_suffix(var_width),
                    offset);
            } else {
                // zero-extension: movzbq (byte) or movzwq (word)
                appendf(&ctx->output,
                    "    # load variable `%s` (zero-extend)\n"
                    "    movz%sq %zu(%%rsp), %%rax\n"
                    "    pushq %%rax\n",
                    term->as.ident->ident.value,
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
            "    # address of variable `%s`\n"
            "    leaq %zu(%%rsp), %%rax\n"
            "    pushq %%rax\n",
            term->as.addr_of->ident.value,
            offset);
        
        ctx->stack_size += QWORD;
    }
    else if (term->kind == NODE_TERM_DEREF) {
        gen_expr(term->as.deref->expr, ctx);
        
        // pop pointer address from stack, push value at that address
        appendf(&ctx->output,
            "    # dereference pointer\n"
            "    popq %%rax\n"
            "    movq (%%rax), %%rbx\n"
            "    pushq %%rbx\n");
    }
    else if (term->kind == NODE_TERM_INDEX) {
        // indexing: arr[index]
        int var_index = get_var_index(term->as.index->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", term->as.index->ident.value);
            exit(EXIT_FAILURE);
        }
        
        Var *var = &ctx->vars[var_index];
        if (var->element_width == 0) {
            fprintf(stderr, "Cannot index non-array variable \"%s\"\n", 
                    term->as.index->ident.value);
            exit(EXIT_FAILURE);
        }
        
        MemWidth element_size = var->element_width;
        
        // push array/pointer address onto stack
        push_indexable_address(var, ctx);
        
        // generate index expression
        gen_expr(term->as.index->index, ctx);
        
        // pop index and address, compute final address and load value
        if (element_size == QWORD || element_size == LONG) {
            appendf(&ctx->output,
                "    # array access: %s[index]\n"
                "    popq %%rbx\n"              // index
                "    popq %%rax\n"              // array/pointer address
                "    imulq $%d, %%rbx\n"        // multiply by element size
                "    addq %%rbx, %%rax\n"       // add to address
                "    mov%s (%%rax), %s\n"       // load value
                "    pushq %%rcx\n",            // push result
                term->as.index->ident.value,
                element_size,
                get_mov_suffix(element_size),
                get_register_for_width(element_size, 'c'));
        } else {
            // byte and word need explicit zero-extension
            appendf(&ctx->output,
                "    # array access: %s[index] (zero-extend)\n"
                "    popq %%rbx\n"              // index
                "    popq %%rax\n"              // array/pointer address
                "    imulq $%d, %%rbx\n"        // multiply by element size
                "    addq %%rbx, %%rax\n"       // add to address
                "    movz%sq (%%rax), %%rcx\n"  // load value with zero-extension
                "    pushq %%rcx\n",            // push result
                term->as.index->ident.value,
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
            "    # alloc(size) - allocate memory using mmap\n"
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
            "    # free(ptr, size) - deallocate memory using munmap\n"
            "    popq %%rdi\n"              // addr (1st arg)
            "    popq %%rsi\n"              // length (2nd arg)
            "    movq $11, %%rax\n"         // munmap syscall number
            "    syscall\n"
            "    pushq %%rax\n");           // push return value

        ctx->stack_size -= QWORD;
    }
    else if (term->kind == NODE_TERM_ARRAY_LIT) {
        // default array literal: { elem1, elem2, ... }
        // if array is used in a variable declaration, the assign statement will handle storing the elements instead
        int element_count = term->as.array_lit->element_count;

        // push all the elements onto the stack
        for (int i = 0; i < element_count; i++) {
            gen_expr(term->as.array_lit->elements[i], ctx);

            // `gen_expr` pushes qwords, but we want our default behaviour to store bytes (might be easier for strings)
            appendf(&ctx->output,
                "    # adjust stack pointer to shrink array element to byte\n"
                "    addq $3, %%rsp\n");
            ctx->stack_size -= 3 * BYTE;
        }
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
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # binary operation: +\n    addq %rbx, %rax\n", ctx);
    } else if (expr->kind == NODE_EXPR_SUB) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # binary operation: -\n    subq %rbx, %rax\n", ctx);
    } else if (expr->kind == NODE_EXPR_MUL) {
        // note: returns 128 bit result in rdx:rax, but we only care about lower 64 bits in rax
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # binary operation: *\n    mulq %rbx\n", ctx);
    } else if (expr->kind == NODE_EXPR_DIV) {
        // clear rdx (remainder) before div
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # binary operation: /\n    xorq %rdx, %rdx\n"
                                                            "    divq %rbx\n", ctx);
    } else if (expr->kind == NODE_EXPR_GTE) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # comparison: >=\n    setge %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_LTE) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # comparison: <=\n    setle %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_GT) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # comparison: >\n    setg %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_LT) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # comparison: <\n    setl %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_EQUALITY) {
        gen_comparison_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # comparison: ==\n    sete %al\n", ctx);
    } else if (expr->kind == NODE_EXPR_AND) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # binary operation: &\n    andq %rbx, %rax\n", ctx);
    } else if (expr->kind == NODE_EXPR_OR) {
        gen_binary_op(expr->as.bin->lhs, expr->as.bin->rhs, "    # binary operation: |\n    orq %rbx, %rax\n", ctx);
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
        bytes_to_pop += ctx->vars[i].total_width;
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
            "    # exit(code)\n"
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

        // check if this is an array literal assignment
        int is_array_lit = stmt->as.stmt_assign->is_array;

        // push new variable to array
        ctx->vars = realloc(ctx->vars, sizeof(Var) * (ctx->var_count + 1));
        Var *new_var = &ctx->vars[ctx->var_count];
        new_var->name = stmt->as.stmt_assign->ident.value;
        
        // set variable type
        set_var_type_from_token(new_var, stmt->as.stmt_assign->var_type, 
                                stmt->as.stmt_assign->is_pointer);
        
        // for explicit array assignment, handle it differently from array_lit expressions 
        if (is_array_lit) {
            if (!(stmt->as.stmt_assign->expr->kind == NODE_EXPR_TERM) ||
                !(stmt->as.stmt_assign->expr->as.term->kind == NODE_TERM_ARRAY_LIT)) {
                fprintf(stderr, "Expected array literal for array variable assignment\n");
                exit(EXIT_FAILURE);
            }

            // for array literals, elements are pushed directly to stack
            NodeTermArrayLit *array_lit = stmt->as.stmt_assign->expr->as.term->as.array_lit;
            int element_count = array_lit->element_count;
            TokenType element_type = stmt->as.stmt_assign->var_type;
            MemWidth element_size = token_to_base_size(element_type);
            
            // update variable to be a stack array
            new_var->is_pointer = 0;
            new_var->element_width = element_size;
            new_var->total_width = element_count * element_size;
            new_var->is_signed = is_token_signed(element_type);
            
            // push each element with proper sizing (in reverse order so they appear forward in memory)
            for (int i = element_count - 1; i >= 0; i--) {
                gen_expr(array_lit->elements[i], ctx);
                
                // pop qword and store with proper element size
                appendf(&ctx->output,
                    "    # store array element with specific size\n"
                    "    popq %%rax\n"
                    "    sub $%d, %%rsp\n"
                    "    mov%s %s, (%%rsp)\n",
                    element_size,
                    get_mov_suffix(element_size),
                    get_register_for_width(element_size, 'a'));
                
                // adjust stack: popped qword but allocated element_size
                ctx->stack_size = ctx->stack_size - QWORD + element_size;
            }
            
            // stack now has the array elements with proper sizing
            new_var->stack_loc = ctx->stack_size;

            ctx->var_count++;
            return;
        }

        // regular value assignment
        gen_expr(stmt->as.stmt_assign->expr, ctx);
        
        MemWidth var_width = new_var->total_width;

        // pop expression result and allocate exact space for variable
        appendf(&ctx->output,
            "    # declare and initialise variable `%s`\n"
            "    popq %%rax\n"
            "    sub $%d, %%rsp\n"
            "    mov%s %s, (%%rsp)\n",
            stmt->as.stmt_assign->ident.value,
            var_width,
            get_mov_suffix(var_width),
            get_register_for_width(var_width, 'a'));

        // update stack size: popped 8 bytes, allocated var_width bytes
        ctx->stack_size = ctx->stack_size - QWORD + var_width;
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
        MemWidth var_width = ctx->vars[var_index].total_width;
        size_t offset = ctx->stack_size - QWORD - ctx->vars[var_index].stack_loc;
        
        appendf(&ctx->output,
            "    # reassign variable `%s`\n"
            "    popq %%rax\n"
            "    mov%s %s, %zu(%%rsp)\n",
            stmt->as.stmt_reassign->ident.value,
            get_mov_suffix(var_width),
            get_register_for_width(var_width, 'a'),
            offset);

        ctx->stack_size -= QWORD;
    }
    else if (stmt->kind == NODE_STMT_ASSIGN_INDEXABLE_ELEMENT) {
        int var_index = get_var_index(stmt->as.stmt_assign_indexable_element->ident.value, ctx);
        if (var_index == -1) {
            fprintf(stderr, "Undefined variable \"%s\"\n", stmt->as.stmt_assign_indexable_element->ident.value);
            exit(EXIT_FAILURE);
        }
        
        Var *var = &ctx->vars[var_index];
        if (!is_indexable(var)) {
            fprintf(stderr, "Cannot index non-indexable variable \"%s\"\n", 
                    stmt->as.stmt_assign_indexable_element->ident.value);
            exit(EXIT_FAILURE);
        }
        
        MemWidth element_size = var->element_width;
        
        // generate index expression
        gen_expr(stmt->as.stmt_assign_indexable_element->index, ctx);
        
        // generate value expression
        gen_expr(stmt->as.stmt_assign_indexable_element->expr, ctx);
        
        // push array/pointer address onto stack
        push_indexable_address(var, ctx);
        
        appendf(&ctx->output,
            "    # array assignment: %s[index] = value\n"
            "    popq %%rcx\n"              // array/pointer address
            "    popq %%rax\n"              // value
            "    popq %%rbx\n"              // index
            "    imulq $%d, %%rbx\n"        // multiply index by element size
            "    addq %%rbx, %%rcx\n"       // add to address
            "    mov%s %s, (%%rcx)\n",      // store value at address
            stmt->as.stmt_assign_indexable_element->ident.value,
            element_size,
            get_mov_suffix(element_size),
            get_register_for_width(element_size, 'a'));
        
        ctx->stack_size -= 3 * QWORD;
    }
    else if (stmt->kind == NODE_STMT_SCOPE) {
        gen_scope(stmt->as.stmt_scope->scope, ctx);
    } 
    else if (stmt->kind == NODE_STMT_IF) {
        gen_expr(stmt->as.stmt_if->expr, ctx);

        // jump to end or else if expr is 0 (false)
        int if_end_label_id = ctx->label_count++;
        appendf(&ctx->output, 
            "    # if statement: evaluate condition\n"
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
        appendf(&ctx->output,
            "    # for loop: initialisation\n");
        if (stmt->as.stmt_for->init != NULL) {
            gen_stmt(stmt->as.stmt_for->init, ctx);
        }

        int for_start_label_id = ctx->label_count++;
        int for_end_label_id = ctx->label_count++;

        // start label
        appendf(&ctx->output,
            "    # for loop: start\n"
            ".Lstart_for_%d:\n",
            for_start_label_id);

        // condition
        if (stmt->as.stmt_for->condition != NULL) {
            appendf(&ctx->output,
                "    # for loop: check condition\n");
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
        appendf(&ctx->output,
            "    # for loop: body\n");
        gen_scope(stmt->as.stmt_for->scope, ctx);

        // increment
        if (stmt->as.stmt_for->increment != NULL) {
            appendf(&ctx->output,
                "    # for loop: increment\n");
            gen_stmt(stmt->as.stmt_for->increment, ctx);
        }

        // jump back to start
        appendf(&ctx->output,
            "    jmp .Lstart_for_%d\n"
            "    # for loop: end\n"
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