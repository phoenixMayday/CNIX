#include "lexer.c"

typedef struct {
    Token *tokens;
    int current_pos;
    int token_count;
} ParserCtx;

void expect_token(TokenType expected_type, ParserCtx *ctx) {
    if (ctx->current_pos >= ctx->token_count || ctx->tokens[ctx->current_pos].type != expected_type) {
        fprintf(stderr, "Expected token type %d\n", expected_type);
        exit(EXIT_FAILURE);
    }
    ctx->current_pos++;
}

// Forward declarations
typedef struct NodeExpr NodeExpr;
typedef struct NodeStmt NodeStmt;
NodeExpr *parse_expr(int min_prec, ParserCtx *ctx);

// Terms
typedef enum {
    NODE_TERM_INT_LIT,
    NODE_TERM_IDENT,
    NODE_TERM_PAREN,
    NODE_TERM_ADDR_OF,
    NODE_TERM_DEREF,
    NODE_TERM_ARRAY_INDEX,
    NODE_TERM_ALLOC,
    NODE_TERM_FREE
} NodeTermKind;

typedef struct {
    Token int_lit;
} NodeTermIntLit;

typedef struct {
    Token ident;
} NodeTermIdent;

typedef struct{
    NodeExpr *expr;
} NodeTermParen;

typedef struct {
    Token ident;
} NodeTermAddrOf;

typedef struct {
    NodeExpr *expr;
} NodeTermDeref;

typedef struct {
    Token ident;
    NodeExpr *index;
} NodeTermArrayIndex;

typedef struct {
    NodeExpr *size;
} NodeTermAlloc;

typedef struct {
    NodeExpr *ptr;
} NodeTermFree;

typedef struct {
    NodeTermKind kind;
    union {
        NodeTermIntLit *int_lit;
        NodeTermIdent *ident;
        NodeTermParen *paren;
        NodeTermAddrOf *addr_of;
        NodeTermDeref *deref;
        NodeTermArrayIndex *array_index;
        NodeTermAlloc *alloc;
        NodeTermFree *free_ptr;
    } as;
} NodeTerm;

// Expressions
typedef enum {
    NODE_EXPR_ADD,
    NODE_EXPR_SUB,
    NODE_EXPR_MUL,
    NODE_EXPR_DIV,
    NODE_EXPR_GTE,
    NODE_EXPR_LTE,
    NODE_EXPR_GT,
    NODE_EXPR_LT,
    NODE_EXPR_EQUALITY,
    NODE_EXPR_AND,
    NODE_EXPR_OR,
    NODE_EXPR_TERM
} NodeExprKind;

typedef struct {
    NodeExpr *lhs;
    NodeExpr *rhs;
} NodeExprBinary;

typedef struct NodeExpr {
    NodeExprKind kind;
    union {
        NodeExprBinary *bin;
        NodeTerm *term;
    } as;
} NodeExpr;

// Scope
typedef struct {
    NodeStmt **stmts;
    int stmt_count;
} NodeScope;

// Statements
typedef enum {
    NODE_STMT_EXIT,
    NODE_STMT_ASSIGN,
    NODE_STMT_REASSIGN,
    NODE_STMT_ASSIGN_ARRAY,
    NODE_STMT_SCOPE,
    NODE_STMT_IF,
    NODE_STMT_FOR
} NodeStmtKind;

typedef struct {
    NodeExpr *expr;
} NodeStmtExit;

typedef struct {
    Token ident;
    NodeExpr *expr;
    TokenType var_type;
    int is_pointer;
} NodeStmtAssignVar;

typedef struct {
    Token ident;
    NodeExpr *expr;
} NodeStmtReassignVar;

typedef struct {
    Token ident;
    NodeExpr *index;
    NodeExpr *expr;
} NodeStmtAssignArray;

typedef struct {
    NodeScope *scope;
} NodeStmtScope;

typedef struct {
    NodeExpr *expr;
    NodeScope *scope;
    NodeScope *else_scope;
} NodeStmtIf;

typedef struct {
    NodeStmt *init;
    NodeExpr *condition;
    NodeStmt *increment;
    NodeScope *scope;
} NodeStmtFor;

typedef struct NodeStmt {
    NodeStmtKind kind;
    union {
        NodeStmtExit *stmt_exit;
        NodeStmtAssignVar *stmt_assign;
        NodeStmtReassignVar *stmt_reassign;
        NodeStmtAssignArray *stmt_assign_array;
        NodeStmtScope *stmt_scope;
        NodeStmtIf *stmt_if;
        NodeStmtFor *stmt_for;
    } as;
} NodeStmt;

// Program
typedef struct {
    NodeStmt **stmts;
    int stmt_count;
} NodeProg;


NodeTerm *parse_term(ParserCtx *ctx) {
    Token *peek = &ctx->tokens[ctx->current_pos];
    if (peek->type == TOKEN_INT_LIT) {
        NodeTermIntLit *int_lit_node = malloc(sizeof(NodeTermIntLit));
        int_lit_node->int_lit = *peek;

        NodeTerm *term_base = malloc(sizeof(NodeTerm));
        term_base->kind = NODE_TERM_INT_LIT;
        term_base->as.int_lit = int_lit_node;

        ctx->current_pos++;
        return term_base;
    } 
    else if (peek->type == TOKEN_AMPERSAND) {
        // address-of operator: &variable
        ctx->current_pos++;
        
        if (ctx->current_pos >= ctx->token_count ||
            ctx->tokens[ctx->current_pos].type != TOKEN_IDENT) {
            fprintf(stderr, "Address-of operator requires an identifier\n");
            exit(EXIT_FAILURE);
        }

        Token ident_token = ctx->tokens[ctx->current_pos];
        ctx->current_pos++; // consume identifier
        
        NodeTermAddrOf *addr_of_node = malloc(sizeof(NodeTermAddrOf));
        addr_of_node->ident = ident_token;
        
        NodeTerm *term_base = malloc(sizeof(NodeTerm));
        term_base->kind = NODE_TERM_ADDR_OF;
        term_base->as.addr_of = addr_of_node;
        
        return term_base;
    }
    else if (peek->type == TOKEN_ASTERISK) {
        // dereference operator: *expr
        ctx->current_pos++;
        
        NodeTermDeref *deref_node = malloc(sizeof(NodeTermDeref));
        deref_node->expr = parse_expr(0, ctx);
        
        NodeTerm *term_base = malloc(sizeof(NodeTerm));
        term_base->kind = NODE_TERM_DEREF;
        term_base->as.deref = deref_node;
        
        return term_base;
    }
    else if (peek->type == TOKEN_ALLOC) {
        // alloc(size)
        ctx->current_pos++;

        expect_token(TOKEN_OPEN_PAREN, ctx);
        
        NodeTermAlloc *alloc_node = malloc(sizeof(NodeTermAlloc));
        alloc_node->size = parse_expr(0, ctx);
        
        expect_token(TOKEN_CLOSE_PAREN, ctx);
        
        NodeTerm *term_base = malloc(sizeof(NodeTerm));
        term_base->kind = NODE_TERM_ALLOC;
        term_base->as.alloc = alloc_node;
        
        return term_base;
    }
    else if (peek->type == TOKEN_FREE) {
        // free(ptr)
        ctx->current_pos++;
        expect_token(TOKEN_OPEN_PAREN, ctx);
        
        NodeTermFree *free_node = malloc(sizeof(NodeTermFree));
        free_node->ptr = parse_expr(0, ctx);
        
        expect_token(TOKEN_CLOSE_PAREN, ctx);
        
        NodeTerm *term_base = malloc(sizeof(NodeTerm));
        term_base->kind = NODE_TERM_FREE;
        term_base->as.free_ptr = free_node;
        
        return term_base;
    }
    else if (peek->type == TOKEN_IDENT) {
        Token ident_token = *peek;
        ctx->current_pos++;
        
        // check indexing array: ident[expr]
        if (ctx->current_pos < ctx->token_count && 
            ctx->tokens[ctx->current_pos].type == TOKEN_OPEN_SQUARE) {
            ctx->current_pos++;
            
            NodeTermArrayIndex *array_index_node = malloc(sizeof(NodeTermArrayIndex));
            array_index_node->ident = ident_token;
            array_index_node->index = parse_expr(0, ctx);
            
            expect_token(TOKEN_CLOSE_SQUARE, ctx);
            
            NodeTerm *term_base = malloc(sizeof(NodeTerm));
            term_base->kind = NODE_TERM_ARRAY_INDEX;
            term_base->as.array_index = array_index_node;
            
            return term_base;
        } else {
            // regular identifier
            NodeTermIdent *ident_node = malloc(sizeof(NodeTermIdent));
            ident_node->ident = ident_token;
            
            NodeTerm *term_base = malloc(sizeof(NodeTerm));
            term_base->kind = NODE_TERM_IDENT;
            term_base->as.ident = ident_node;
            
            return term_base;
        }
    } 
    else if (peek->type == TOKEN_OPEN_PAREN) {
        ctx->current_pos++; // consume '('

        // expect expression
        NodeTermParen *paren_node = malloc(sizeof(NodeTermParen));
        paren_node->expr = parse_expr(0, ctx);

        expect_token(TOKEN_CLOSE_PAREN, ctx);

        NodeTerm *term_base = malloc(sizeof(NodeTerm));
        term_base->kind = NODE_TERM_PAREN;
        term_base->as.paren = paren_node;

        return term_base;
    }
    else {
        fprintf(stderr, "Unexpected token in term\n");
        exit(EXIT_FAILURE);
    }
}

NodeExpr *parse_expr(int min_prec, ParserCtx *ctx) {
    // eval left term
    NodeTerm *lhs_term = parse_term(ctx);
    NodeExpr *lhs_expr = malloc(sizeof(NodeExpr));
    lhs_expr->kind = NODE_EXPR_TERM;
    lhs_expr->as.term = lhs_term;
    
    while (ctx->current_pos < ctx->token_count) {
        Token *peek = &ctx->tokens[ctx->current_pos];
        int prec = get_precedence(peek->type);
        if (prec == 0 || prec < min_prec) break;

        // eval operator
        Token op = *peek;
        ctx->current_pos++;

        // parse right-hand side expression with higher precedence for left associativity
        NodeExpr *rhs_expr = parse_expr(prec + 1, ctx);

        // create binary expression node
        NodeExprBinary *bin_node = malloc(sizeof(NodeExprBinary));
        bin_node->lhs = lhs_expr;
        bin_node->rhs = rhs_expr;

        // create combined expression with appropriate kind
        NodeExpr *combined = malloc(sizeof(NodeExpr));
        combined->as.bin = bin_node;
        
        switch (op.type) {
            case TOKEN_PLUS: combined->kind = NODE_EXPR_ADD; break;
            case TOKEN_MINUS: combined->kind = NODE_EXPR_SUB; break;
            case TOKEN_ASTERISK: combined->kind = NODE_EXPR_MUL; break;
            case TOKEN_FSLASH: combined->kind = NODE_EXPR_DIV; break;
            case TOKEN_GTE: combined->kind = NODE_EXPR_GTE; break;
            case TOKEN_LTE: combined->kind = NODE_EXPR_LTE; break;
            case TOKEN_GT: combined->kind = NODE_EXPR_GT; break;
            case TOKEN_LT: combined->kind = NODE_EXPR_LT; break;
            case TOKEN_DOUBLE_EQUALS: combined->kind = NODE_EXPR_EQUALITY; break;
            case TOKEN_AMPERSAND: combined->kind = NODE_EXPR_AND; break;
            case TOKEN_PIPE: combined->kind = NODE_EXPR_OR; break;
            default:
                fprintf(stderr, "Unexpected operator in expression: %d\n", op.type);
                exit(EXIT_FAILURE);
        }

        // make combined expression the new lhs
        lhs_expr = combined;
    }

    return lhs_expr;
}

NodeStmt *parse_stmt(ParserCtx *ctx);

NodeScope *parse_scope_block(ParserCtx *ctx) {
    expect_token(TOKEN_OPEN_CURLY, ctx);

    NodeScope *scope = malloc(sizeof(NodeScope));
    scope->stmts = NULL;
    scope->stmt_count = 0;

    // parse statements until closing curly brace
    // this will result in infinite loop if no close curly :(
    Token *peek = &ctx->tokens[ctx->current_pos];
    while (ctx->current_pos < ctx->token_count && peek->type != TOKEN_CLOSE_CURLY) {
        NodeStmt *stmt = parse_stmt(ctx);
        scope->stmts = realloc(scope->stmts, sizeof(NodeStmt*) * (scope->stmt_count + 1));
        scope->stmts[scope->stmt_count] = stmt;
        scope->stmt_count++;
        peek = &ctx->tokens[ctx->current_pos];
    }

    expect_token(TOKEN_CLOSE_CURLY, ctx);

    return scope;
}

// just a one-statement scope for else and if statements
NodeScope *parse_implicit_scope(ParserCtx *ctx) {
    NodeStmt *stmt = parse_stmt(ctx);

    NodeScope *scope = malloc(sizeof(NodeScope));
    scope->stmts = malloc(sizeof(NodeStmt*));
    scope->stmts[0] = stmt;
    scope->stmt_count = 1;

    return scope;
}

NodeScope *parse_scope(ParserCtx *ctx) {
    Token *peek = &ctx->tokens[ctx->current_pos];
    if (peek->type == TOKEN_OPEN_CURLY) {
        return parse_scope_block(ctx);
    } 

    return parse_implicit_scope(ctx);
}

NodeStmt *parse_stmt(ParserCtx *ctx) {
    Token *current = &ctx->tokens[ctx->current_pos];

    if (current->type == TOKEN_EXIT) {
        ctx->current_pos++;

        // expect open parenethesis
        expect_token(TOKEN_OPEN_PAREN, ctx);

        // expect expression
        NodeExpr *expr = parse_expr(0, ctx);

        // expect close parenthesis
        expect_token(TOKEN_CLOSE_PAREN, ctx);

        // expect semicolon
        expect_token(TOKEN_SEMI, ctx);

        NodeStmtExit *stmt_exit = malloc(sizeof(NodeStmtExit));
        stmt_exit->expr = expr;

        NodeStmt *stmt_base = malloc(sizeof(NodeStmt));
        stmt_base->kind = NODE_STMT_EXIT;
        stmt_base->as.stmt_exit = stmt_exit;

        return stmt_base;
    } 
    
    if (current->type == TOKEN_BYTE || current->type == TOKEN_WORD ||
        current->type == TOKEN_LONG || current->type == TOKEN_QWORD ||
        current->type == TOKEN_INT8 || current->type == TOKEN_INT16 ||
        current->type == TOKEN_INT32 || current->type == TOKEN_INT64 ||
        current->type == TOKEN_UINT8 || current->type == TOKEN_UINT16 ||
        current->type == TOKEN_UINT32 || current->type == TOKEN_UINT64 ||
        current->type == TOKEN_CHAR) {
        
        TokenType var_type = current->type;
        ctx->current_pos++;
        
        // check if pointer type
        int is_pointer = 0;
        if (ctx->current_pos < ctx->token_count && 
            ctx->tokens[ctx->current_pos].type == TOKEN_ASTERISK) {
            is_pointer = 1;
            ctx->current_pos++; // consume '*'
        }
        
        Token ident_token = ctx->tokens[ctx->current_pos];

        // expect identifier
        expect_token(TOKEN_IDENT, ctx);

        // expect equals
        expect_token(TOKEN_EQUALS, ctx);

        // expect expression
        NodeExpr *expr = parse_expr(0, ctx);

        // expect semicolon
        expect_token(TOKEN_SEMI, ctx);

        NodeStmtAssignVar *stmt_assign = malloc(sizeof(NodeStmtAssignVar));
        stmt_assign->ident = ident_token;
        stmt_assign->expr = expr;
        stmt_assign->var_type = var_type;
        stmt_assign->is_pointer = is_pointer;

        NodeStmt *stmt_base = malloc(sizeof(NodeStmt));
        stmt_base->kind = NODE_STMT_ASSIGN;
        stmt_base->as.stmt_assign = stmt_assign;
        return stmt_base;
    } 
    
    if (current->type == TOKEN_IDENT) {
        Token ident_token = *current;
        ctx->current_pos++;

        // check if array assignment: arr[index] = expr
        if (ctx->current_pos < ctx->token_count &&
            ctx->tokens[ctx->current_pos].type == TOKEN_OPEN_SQUARE) {
            ctx->current_pos++; // consume '['
            
            NodeExpr *index_expr = parse_expr(0, ctx);
            
            expect_token(TOKEN_CLOSE_SQUARE, ctx);
            expect_token(TOKEN_EQUALS, ctx);
            
            NodeExpr *value_expr = parse_expr(0, ctx);
            
            expect_token(TOKEN_SEMI, ctx);
            
            NodeStmtAssignArray *stmt_assign_array = malloc(sizeof(NodeStmtAssignArray));
            stmt_assign_array->ident = ident_token;
            stmt_assign_array->index = index_expr;
            stmt_assign_array->expr = value_expr;
            
            NodeStmt *stmt_base = malloc(sizeof(NodeStmt));
            stmt_base->kind = NODE_STMT_ASSIGN_ARRAY;
            stmt_base->as.stmt_assign_array = stmt_assign_array;
            
            return stmt_base;
        }

        // regular reassignment
        expect_token(TOKEN_EQUALS, ctx);

        // expect expression
        NodeExpr *expr = parse_expr(0, ctx);

        // expect semicolon
        expect_token(TOKEN_SEMI, ctx);

        NodeStmtReassignVar *stmt_reassign = malloc(sizeof(NodeStmtReassignVar));
        stmt_reassign->ident = ident_token;
        stmt_reassign->expr = expr;

        NodeStmt *stmt_base = malloc(sizeof(NodeStmt));
        stmt_base->kind = NODE_STMT_REASSIGN;
        stmt_base->as.stmt_reassign = stmt_reassign;

        return stmt_base;
    } 
    
    if (current->type == TOKEN_OPEN_CURLY) {
        // this is kinda messy with `NodeScope` and `NodeStmtScope` but oh well
        NodeScope *scope = parse_scope_block(ctx);

        NodeStmtScope *stmt_scope = malloc(sizeof(NodeStmtScope));
        stmt_scope->scope = scope;

        NodeStmt *stmt_base = malloc(sizeof(NodeStmt));
        stmt_base->kind = NODE_STMT_SCOPE;
        stmt_base->as.stmt_scope = stmt_scope;

        return stmt_base;
    }
    
    if (current->type == TOKEN_IF) {
        ctx->current_pos++;

        // expect expression in parentheses
        expect_token(TOKEN_OPEN_PAREN, ctx);
        NodeExpr *expr = parse_expr(0, ctx);
        expect_token(TOKEN_CLOSE_PAREN, ctx);

        // expect scope
        NodeScope *scope = parse_scope(ctx);

        NodeStmtIf *stmt_if = malloc(sizeof(NodeStmtIf));
        stmt_if->expr = expr;
        stmt_if->scope = scope;
        stmt_if->else_scope = NULL;

        // check for else
        Token *peek = &ctx->tokens[ctx->current_pos];
        if (ctx->current_pos < ctx->token_count && peek->type == TOKEN_ELSE) {
            ctx->current_pos++;
            NodeScope *else_scope = parse_scope(ctx);
            stmt_if->else_scope = else_scope;
        }

        NodeStmt *stmt_base = malloc(sizeof(NodeStmt));
        stmt_base->kind = NODE_STMT_IF;
        stmt_base->as.stmt_if = stmt_if;

        return stmt_base;
    }

    if (current->type == TOKEN_FOR) {
        ctx->current_pos++;

        expect_token(TOKEN_OPEN_PAREN, ctx);

        // parse (optional) init statement 
        NodeStmt *init_stmt = NULL;
        Token *peek = &ctx->tokens[ctx->current_pos];
        if (peek->type != TOKEN_SEMI) {
            init_stmt = parse_stmt(ctx);
        } else {
            ctx->current_pos++; // consume semicolon
        }

        // parse (optional) condition expression 
        NodeExpr *condition_expr = NULL;
        peek = &ctx->tokens[ctx->current_pos];
        if (peek->type != TOKEN_SEMI) {
            condition_expr = parse_expr(0, ctx);
        }
        expect_token(TOKEN_SEMI, ctx);

        // parse (optional) increment statement
        NodeStmt *increment_stmt = NULL;
        peek = &ctx->tokens[ctx->current_pos];
        if (peek->type != TOKEN_SEMI) {
            increment_stmt = parse_stmt(ctx);
        } else {
            ctx->current_pos++; // consume semicolon
        }

        expect_token(TOKEN_CLOSE_PAREN, ctx);

        NodeScope *scope_inner = parse_scope(ctx);

        NodeStmtFor *stmt_for = malloc(sizeof(NodeStmtFor));
        stmt_for->init = init_stmt;
        stmt_for->condition = condition_expr;
        stmt_for->increment = increment_stmt;
        stmt_for->scope = scope_inner;

        NodeStmt *stmt_base_inner = malloc(sizeof(NodeStmt));
        stmt_base_inner->kind = NODE_STMT_FOR;
        stmt_base_inner->as.stmt_for = stmt_for;

        // wrap for loop in implicit scope so everything in init, condition, and increment is scoped
        NodeScope *scope_outer = malloc(sizeof(NodeScope));
        scope_outer->stmts = malloc(sizeof(NodeStmt*));
        scope_outer->stmts[0] = stmt_base_inner;
        scope_outer->stmt_count = 1;

        NodeStmtScope *stmt_scope_outer = malloc(sizeof(NodeStmtScope));
        stmt_scope_outer->scope = scope_outer;

        NodeStmt *stmt_base_outer = malloc(sizeof(NodeStmt));
        stmt_base_outer->kind = NODE_STMT_SCOPE;
        stmt_base_outer->as.stmt_scope = stmt_scope_outer;

        return stmt_base_outer;
    }

    fprintf(stderr, "Unexpected token in statement\n");
    exit(EXIT_FAILURE);
}

NodeProg *parse_prog(Token **tokens, int token_count) {
    NodeProg *prog = malloc(sizeof(NodeProg));
    prog->stmts = NULL;
    prog->stmt_count = 0;

    ParserCtx ctx = {
        .tokens = *tokens,
        .current_pos = 0,
        .token_count = token_count
    };

    while (ctx.current_pos < token_count) {
        NodeStmt *stmt = parse_stmt(&ctx);
        prog->stmts = realloc(prog->stmts, sizeof(NodeStmt*) * (prog->stmt_count + 1));
        prog->stmts[prog->stmt_count] = stmt;
        prog->stmt_count++;
    }

    return prog;
}